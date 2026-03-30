# ============================================================
#  CollocationCalculator — LADAL Collocation Analysis Tool
#  https://ladal.edu.au
#
#  Calculates association measures for user-defined node words
#  across uploaded plain-text files.
#
#  Measures implemented (all from scratch, no external AM pkg):
#    O/E, MI, MI2, MI3, log-likelihood (G2), t-score,
#    Delta P12, Delta P21, Fisher's exact test p-value
# ============================================================

library(shiny)
library(data.table)
library(stringi)
library(ggplot2)
library(writexl)
library(DT)

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

# ── Measure metadata ─────────────────────────────────────────
MEASURES <- data.frame(
  id    = c("OE", "MI", "MI2", "MI3", "G2",
            "tscore", "DeltaP12", "DeltaP21", "Fisher"),
  label = c("O/E", "MI", "MI2", "MI3", "Log-likelihood (G2)",
            "t-score", "Delta P12", "Delta P21", "Fisher p"),
  desc  = c(
    "Observed / Expected frequency",
    "Mutual Information: log2(O/E)",
    "MI2: log2(O² / E)",
    "MI3: log2(O³ / E)",
    "Log-likelihood ratio (G²)",
    "t-score: (O − E) / √O",
    "ΔP₁₂: P(collocate | node) − P(collocate | ¬node)",
    "ΔP₂₁: P(node | collocate) − P(node | ¬collocate)",
    "Fisher's exact test p-value (two-tailed)"
  ),
  stringsAsFactors = FALSE
)

# ── Collocation engine ───────────────────────────────────────

# Tokenise a single string into a character vector of words.
# If ignore_case=TRUE, output is lowercased.
tokenise <- function(text, ignore_case = TRUE) {
  toks <- stri_extract_all_words(text, simplify = FALSE)[[1]]
  toks <- toks[!is.na(toks)]
  if (ignore_case) stri_trans_tolower(toks) else toks
}

# Build token vector from multiple files.
# Returns a single integer-indexed character vector (all files concatenated).
build_token_vector <- function(file_df, ignore_case = TRUE) {
  texts <- vapply(file_df$datapath, function(p) {
    paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  }, character(1))
  full_text <- paste(texts, collapse = " ")
  tokenise(full_text, ignore_case)
}

# Core collocation calculator.
# Returns a data.table with all association measures for every
# (node, collocate) pair that meets the minimum co-occurrence count.
#
# tokens      : character vector (full corpus token sequence)
# node_words  : character vector of node words to analyse
# span_left   : integer, left window size
# span_right  : integer, right window size
# min_freq    : minimum raw co-occurrence count
# ignore_case : logical (already applied to tokens, but used for node matching)
calc_collocations <- function(tokens, node_words, span_left, span_right,
                              min_freq = 2L) {

  N <- length(tokens)
  if (N == 0L) return(NULL)

  # Unigram frequencies (vectorised with data.table)
  freq_dt <- data.table(word = tokens)[, .(freq = .N), by = word]
  setkey(freq_dt, word)

  results <- lapply(node_words, function(node) {

    f_node <- freq_dt[node, freq, nomatch = 0L]
    if (length(f_node) == 0L || f_node == 0L) return(NULL)

    # Positions of node in token vector
    node_pos <- which(tokens == node)

    # For every node occurrence, collect window tokens
    window_tokens <- unlist(lapply(node_pos, function(pos) {
      left_start  <- max(1L,   pos - span_left)
      right_end   <- min(N,    pos + span_right)
      # exclude the node position itself
      idx <- c(seq_len(pos - left_start),          # left indices
               seq(pos + 1L, right_end))            # right indices
      idx <- idx[idx >= 1L & idx <= N & idx != pos]
      tokens[idx]
    }))

    if (length(window_tokens) == 0L) return(NULL)

    # Co-occurrence counts
    cooc_dt <- data.table(word = window_tokens)[, .(O = .N), by = word]
    cooc_dt <- cooc_dt[O >= min_freq]
    if (nrow(cooc_dt) == 0L) return(NULL)

    # Merge with unigram frequencies
    cooc_dt <- freq_dt[cooc_dt, on = "word"]
    setnames(cooc_dt, c("word", "f_collocate", "O"))

    # Window size (total possible co-occurrence slots per node occurrence)
    window_size <- span_left + span_right
    # Expected frequency
    # E = f_node * f_collocate / N  (standard bigram expected)
    cooc_dt[, E := (f_node * f_collocate) / N]

    # ── Compute all measures ────────────────────────────────
    cooc_dt[, `:=`(
      node = node,
      N    = N,
      f_node = f_node
    )]

    # O/E
    cooc_dt[, OE := O / E]

    # MI family
    cooc_dt[, MI  := log2(O / E)]
    cooc_dt[, MI2 := log2(O^2 / E)]
    cooc_dt[, MI3 := log2(O^3 / E)]

    # Log-likelihood G2
    # Observed cell: a=O, b=f_node-O, c=f_collocate-O, d=N-f_node-f_collocate+O
    cooc_dt[, `:=`(
      a = O,
      b = pmax(f_node - O, 0L),
      c = pmax(f_collocate - O, 0L),
      d = pmax(N - f_node - f_collocate + O, 0L)
    )]
    # G2 = 2 * sum(observed * log(observed/expected)) for all 4 cells
    cooc_dt[, G2 := {
      R1 <- a + b; R2 <- c + d
      C1 <- a + c; C2 <- b + d
      E_a <- R1 * C1 / N; E_b <- R1 * C2 / N
      E_c <- R2 * C1 / N; E_d <- R2 * C2 / N
      safe_ll <- function(obs, exp) {
        ifelse(obs == 0 | exp == 0, 0, obs * log(obs / exp))
      }
      2 * (safe_ll(a, E_a) + safe_ll(b, E_b) +
           safe_ll(c, E_c) + safe_ll(d, E_d))
    }]

    # t-score
    cooc_dt[, tscore := (O - E) / sqrt(pmax(O, 1))]

    # Delta P
    # DeltaP12: P(w2|w1) - P(w2|¬w1)
    #   = O/f_node  -  (f_collocate - O)/(N - f_node)
    cooc_dt[, DeltaP12 := (O / f_node) -
              (pmax(f_collocate - O, 0) / pmax(N - f_node, 1))]

    # DeltaP21: P(w1|w2) - P(w1|¬w2)
    cooc_dt[, DeltaP21 := (O / f_collocate) -
              (pmax(f_node - O, 0) / pmax(N - f_collocate, 1))]

    # Fisher's exact test p-value (two-tailed)
    # Apply only if N is not too large; cap at 1e6 for speed
    cooc_dt[, Fisher := {
      mapply(function(aa, bb, cc, dd) {
        tryCatch({
          mat <- matrix(c(aa, bb, cc, dd), nrow = 2L)
          if (any(mat < 0)) return(NA_real_)
          fisher.test(mat, simulate.p.value = (aa + bb + cc + dd > 2000))$p.value
        }, error = function(e) NA_real_)
      }, a, b, c, d)
    }]

    # Round to 4 decimal places for display
    num_cols <- c("OE","MI","MI2","MI3","G2","tscore",
                  "DeltaP12","DeltaP21","Fisher","E")
    cooc_dt[, (num_cols) := lapply(.SD, function(x) round(x, 4L)),
            .SDcols = num_cols]

    # Select and order output columns
    cooc_dt[, .(node, collocate = word,
                O, E, f_node, f_collocate, N,
                OE, MI, MI2, MI3, G2, tscore,
                DeltaP12, DeltaP21, Fisher)]
  })

  rbindlist(Filter(Negate(is.null), results))
}

# ── Helper: parse comma-separated node words ──────────────────
parse_nodes <- function(raw, ignore_case) {
  words <- stri_split_fixed(raw, ",")[[1]]
  words <- stri_trim_both(words)
  words <- words[nchar(words) > 0]
  if (ignore_case) stri_trans_tolower(words) else words
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── UI ────────────────────────────────────────────────────────
# ── Citation footer ─────────────────────────────────────────────────
CITATION_FOOTER <- tags$div(
  style = paste0(
    "border-top:2px solid #e0d4f0;margin-top:28px;padding:20px 28px 16px 28px;",
    "background:#faf7fd;font-family:sans-serif;font-size:.82rem;color:#555;"
  ),
  tags$div(
    style = "display:flex;align-items:center;gap:14px;margin-bottom:10px;",
    tags$span(style = "font-size:1rem;font-weight:700;color:#51247a;",
              "How to cite this tool"),
    tags$a("→ Tutorial", href = "https://ladal.edu.au/tutorials/coll/coll.html", target = "_blank",
           style = "font-size:.78rem;color:#51247a;")
  ),
  tags$blockquote(
    style = "border-left:3px solid #c8b8de;padding-left:12px;margin:0 0 10px 0;color:#444;",
    HTML(paste0(
      "Schweinberger, Martin. (2025). ",
      "<em>CollocationCalculator: A browser-based collocation analysis tool</em>. ",
      "Brisbane: The University of Queensland. ",
      "Language Technology and Data Analysis Laboratory (LADAL). ",
      "Retrieved from https://ladal.edu.au/tools.html"
    ))
  ),
  tags$details(
    tags$summary(style = "cursor:pointer;color:#51247a;font-weight:600;font-size:.8rem;",
                 "BibTeX"),
    tags$pre(
      style = paste0("background:#ece8f5;border-radius:5px;padding:10px;",
                     "font-size:.75rem;overflow-x:auto;margin-top:6px;"),
      paste0(
        "@misc{schweinberger2025collocationcalculator,\n",
        "  author       = {Schweinberger, Martin},\n",
        "  title        = {CollocationCalculator: A browser-based collocation analysis tool},\n",
        "  year         = {2025},\n",
        "  organization = {The University of Queensland},\n",
        "  url          = {https://ladal.edu.au/tools.html}\n",
        "}"
      )
    )
  )
)

ui <- fluidPage(
  title = "CollocationCalculator | LADAL",

  tags$head(tags$style(HTML(paste0("
    @import url('https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;500&family=Fira+Sans:wght@400;600;700&display=swap');

    *, *::before, *::after { box-sizing: border-box; }

    body {
      font-family: 'Fira Sans', sans-serif;
      background: #f5f3f8; color: #1a1a2e;
      margin: 0; font-size: 14px;
    }

    /* ── Banner ── */
    .cc-banner {
      background: ", LADAL_PURPLE, ";
      color: white; padding: 17px 32px 13px 32px;
      display: flex; align-items: center; gap: 16px;
      border-bottom: 4px solid ", LADAL_GOLD, ";
    }
    .cc-banner-icon  { font-size: 2rem; line-height: 1; }
    .cc-banner-title {
      font-size: 1.65rem; font-weight: 700;
      letter-spacing: .2px; margin: 0;
    }
    .cc-banner-sub { font-size: .83rem; opacity: .8; margin: 2px 0 0; }
    .cc-banner a   { color: #f7d97a; text-decoration: none; }

    /* ── Layout ── */
    .cc-body { display: flex; min-height: calc(100vh - 80px); }
    .cc-side {
      width: 320px; min-width: 270px; max-width: 350px;
      background: white;
      border-right: 1px solid #e2dced;
      padding: 20px 18px 32px 18px;
      box-shadow: 2px 0 10px rgba(81,36,122,.05);
      overflow-y: auto;
    }
    .cc-main { flex: 1; padding: 24px 28px; overflow-x: auto; }

    /* ── Sidebar section titles ── */
    .cc-sec {
      font-size: .72rem; font-weight: 700; letter-spacing: 1.3px;
      text-transform: uppercase; color: ", LADAL_PURPLE, ";
      border-bottom: 2px solid ", LADAL_GOLD, ";
      padding-bottom: 4px; margin: 22px 0 11px 0;
    }
    .cc-sec:first-child { margin-top: 0; }

    /* ── Inputs ── */
    .form-control, .selectize-input {
      border: 1.5px solid #d4cce4 !important;
      border-radius: 6px !important;
      font-size: .88rem !important;
      font-family: 'Fira Code', monospace !important;
    }
    .form-control:focus {
      border-color: ", LADAL_PURPLE, " !important;
      box-shadow: 0 0 0 2px rgba(81,36,122,.12) !important;
    }
    label { font-size: .83rem; font-weight: 600; color: #555;
            margin-bottom: 3px; }
    .form-group { margin-bottom: 9px; }

    /* Compact pairs */
    .cc-row { display: flex; gap: 8px; }
    .cc-row .form-group { flex: 1; min-width: 0; }

    /* ── Info / warn / ok boxes ── */
    .cc-info {
      background: #f4f0f8; border-left: 4px solid ", LADAL_PURPLE, ";
      border-radius: 5px; padding: 9px 13px;
      font-size: .83rem; color: #444; margin-bottom: 12px;
    }
    .cc-warn {
      background: #fff4e5; border-left: 4px solid ", LADAL_GOLD, ";
      border-radius: 5px; padding: 9px 13px;
      font-size: .83rem; color: #6b4000; margin-bottom: 10px;
    }
    .cc-ok {
      background: #eafaf1; border-left: 4px solid #27ae60;
      border-radius: 5px; padding: 9px 13px;
      font-size: .83rem; color: #1a6b3c; margin-bottom: 8px;
    }

    /* ── Run button ── */
    #run_btn {
      width: 100%;
      background: ", LADAL_PURPLE, " !important;
      border: none !important; color: white !important;
      font-weight: 700; font-size: .97rem; padding: 11px;
      border-radius: 7px; margin-top: 4px;
      font-family: 'Fira Sans', sans-serif;
      transition: background .2s;
    }
    #run_btn:hover { background: #3d1763 !important; }

    /* ── Download buttons ── */
    .cc-dl-btn {
      display: inline-flex; align-items: center; gap: 6px;
      background: white;
      border: 1.5px solid ", LADAL_PURPLE, ";
      color: ", LADAL_PURPLE, ";
      font-weight: 600; font-size: .83rem;
      padding: 6px 14px; border-radius: 6px;
      margin: 3px 5px 3px 0;
      cursor: pointer; text-decoration: none;
      transition: all .15s;
      font-family: 'Fira Sans', sans-serif;
    }
    .cc-dl-btn:hover { background: ", LADAL_PURPLE, "; color: white; }

    /* ── Stat chips ── */
    .cc-chips { display: flex; gap: 10px; flex-wrap: wrap;
                margin-bottom: 18px; }
    .cc-chip  {
      background: white; border-radius: 20px;
      border: 1.5px solid #e2dced;
      padding: 5px 14px; font-size: .82rem;
      display: flex; align-items: center; gap: 5px;
    }
    .cc-chip b { color: ", LADAL_PURPLE, "; }

    /* ── Tabs ── */
    .nav-tabs > li > a {
      color: ", LADAL_PURPLE, " !important; font-weight: 600;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      border-top: 3px solid ", LADAL_PURPLE, " !important;
      color: ", LADAL_PURPLE, " !important;
    }

    /* ── DT ── */
    .dataTables_wrapper { font-size: .85rem; }
    table.dataTable thead th {
      background: ", LADAL_PURPLE, " !important;
      color: white !important; font-weight: 700;
      border-bottom: 2px solid ", LADAL_GOLD, " !important;
    }
    table.dataTable tbody tr:hover { background: #f4f0f8 !important; }
    .cc-mono { font-family: 'Fira Code', monospace !important; }

    /* ── Plot wrap ── */
    .cc-plot-wrap {
      background: white; border: 1px solid #e2dced;
      border-radius: 8px; padding: 18px 20px;
      box-shadow: 0 1px 6px rgba(81,36,122,.05);
    }

    /* ── Welcome ── */
    .cc-welcome {
      max-width: 540px; margin: 50px auto;
      text-align: center; color: #888;
    }
    .cc-welcome-icon { font-size: 3rem; margin-bottom: 14px; }
    .cc-welcome h3 {
      color: #555; font-size: 1.15rem;
      font-weight: 700; margin-bottom: 8px;
    }

    /* ── Upload btn ── */
    .shiny-input-container .btn {
      background: white !important;
      border: 1.5px dashed ", LADAL_PURPLE, " !important;
      color: ", LADAL_PURPLE, " !important;
      font-weight: 600 !important; width: 100% !important;
    }

    /* ── Footer ── */
    .cc-footer {
      background: #2d1a4a; color: #c8b8de;
      font-size: .77rem; padding: 11px 32px;
      display: flex; gap: 18px; align-items: center;
    }
    .cc-footer a { color: #d4b8f5; }
  ")))),

  # ── Banner ──────────────────────────────────────────────────
  div(class = "cc-banner",
    div(class = "cc-banner-icon", "🔗"),
    div(
      p(class = "cc-banner-title", "CollocationCalculator"),
      p(class = "cc-banner-sub",
        "Association measures for collocations · ",
        tags$a("LADAL", href = "https://ladal.edu.au",
               style = "color:#f7d97a;"))
    )
  ),

  # ── Body ────────────────────────────────────────────────────
  div(class = "cc-body",

    # ── Sidebar ───────────────────────────────────────────────
    div(class = "cc-side",

      # STEP 1 — Upload
      div(class = "cc-sec", "① Upload texts"),
      div(class = "cc-info",
        "Upload one or more ", tags$b(".txt"), " files.
         All files are concatenated into one corpus before analysis."
      ),
      fileInput("files", NULL,
                multiple    = TRUE,
                accept      = ".txt",
                buttonLabel = "📂 Choose .txt files"),
      uiOutput("upload_status"),

      # STEP 2 — Node words
      div(class = "cc-sec", "② Node word(s)"),
      div(class = "cc-info",
        "Enter one or more words separated by commas.
         All node words are analysed together in one combined table."
      ),
      textInput("nodes", "Node word(s)",
                value       = "",
                placeholder = "e.g.  climate, change, policy"),
      checkboxInput("ignore_case", "Ignore case (recommended)",
                    value = TRUE),

      # STEP 3 — Window
      div(class = "cc-sec", "③ Collocation window"),
      div(class = "cc-row",
        numericInput("span_left",  "Left span",  value = 5L,
                     min = 1L, max = 20L, step = 1L),
        numericInput("span_right", "Right span", value = 5L,
                     min = 1L, max = 20L, step = 1L)
      ),

      # STEP 4 — Filters
      div(class = "cc-sec", "④ Filters"),
      numericInput("min_freq", "Min. co-occurrence count",
                   value = 2L, min = 1L, step = 1L),
      numericInput("top_n", "Top N collocates per node (plot)",
                   value = 20L, min = 5L, max = 100L, step = 5L),

      # STEP 5 — Plot measure
      div(class = "cc-sec", "⑤ Plot sort measure"),
      selectInput("plot_measure", "Sort bars by",
                  choices  = setNames(MEASURES$id, MEASURES$label),
                  selected = "DeltaP12"),

      # STEP 6 — Run
      div(class = "cc-sec", "⑥ Calculate"),
      actionButton("run_btn", "🔗  Calculate collocations",
                   class = "btn-primary"),

      # Downloads
      div(class = "cc-sec", "⑦ Download"),
      uiOutput("download_buttons")
    ),

    # ── Main panel ────────────────────────────────────────────
    div(class = "cc-main",
      uiOutput("welcome_or_results")
    )
  ),

  # ── Footer ──────────────────────────────────────────────────
  div(class = "cc-footer",
    span("CollocationCalculator · LADAL · University of Queensland"),
    tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
    tags$a("Collocation Tutorial",
           href = "https://ladal.edu.au/tutorials/coll/coll.html"),
    tags$a("Cite this tool",
           href = "https://ladal.edu.au/about.html#citing")
  ),
  CITATION_FOOTER
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Upload status ──────────────────────────────────────────
  output$upload_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "cc-warn", "⚠ No files uploaded yet.")
    } else {
      n <- nrow(input$files)
      div(class = "cc-ok",
          paste0("✔ ", n, " file", if (n != 1) "s", " loaded"))
    }
  })

  # ── Core reactive: build token vector once per upload ──────
  token_vec <- reactive({
    req(input$files)
    withProgress(message = "Tokenising corpus…", value = 0.3, {
      toks <- build_token_vector(input$files,
                                 ignore_case = isTRUE(input$ignore_case))
      incProgress(0.7)
      toks
    })
  })

  # ── Core reactive: run collocation analysis ────────────────
  coll_result <- eventReactive(input$run_btn, {
    req(input$files, nchar(trimws(input$nodes)) > 0)

    nodes <- parse_nodes(input$nodes, isTRUE(input$ignore_case))
    if (length(nodes) == 0) return(NULL)

    toks <- token_vec()

    withProgress(message = "Calculating association measures…", value = 0.1, {
      res <- tryCatch({
        calc_collocations(
          tokens     = toks,
          node_words = nodes,
          span_left  = as.integer(input$span_left  %||% 5L),
          span_right = as.integer(input$span_right %||% 5L),
          min_freq   = as.integer(input$min_freq   %||% 2L)
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message),
                         type = "error", duration = 12)
        NULL
      })
      incProgress(0.9)
      res
    })
  })

  # ── Build plot reactive ─────────────────────────────────────
  build_plot <- reactive({
    res <- coll_result()
    req(!is.null(res) && nrow(res) > 0)

    measure_id    <- input$plot_measure %||% "DeltaP12"
    measure_label <- MEASURES$label[MEASURES$id == measure_id]
    top_n         <- as.integer(input$top_n %||% 20L)
    nodes         <- unique(res$node)

    # For each node, keep top_n by chosen measure (handle NAs)
    plot_dt <- rbindlist(lapply(nodes, function(nd) {
      sub <- res[res$node == nd, ]
      sub <- sub[order(-get(measure_id), na.last = TRUE), ]
      sub <- sub[!is.na(get(measure_id)), ]
      head(sub, top_n)
    }))

    if (nrow(plot_dt) == 0L) return(NULL)

    n_nodes  <- length(nodes)
    n_cols   <- min(n_nodes, 3L)

    ggplot(plot_dt,
           aes(x    = reorder(paste0(collocate, " [", node, "]"),
                               get(measure_id)),
               y    = get(measure_id),
               fill = node)) +
      geom_col(width = 0.75, colour = "white", linewidth = .25) +
      geom_text(aes(label = round(get(measure_id), 3),
                    hjust = ifelse(get(measure_id) >= 0, -0.1, 1.1)),
                size = 2.9, colour = "gray30") +
      facet_wrap(~ node, scales = "free_y", ncol = n_cols) +
      coord_flip() +
      scale_fill_manual(
        values = setNames(
          colorRampPalette(c(LADAL_PURPLE, "#e07b39", "#27ae60",
                             "#2980b9", "#8e44ad"))(length(nodes)),
          nodes),
        guide = "none"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
      labs(
        title    = paste0("Top ", top_n,
                          " collocates by ", measure_label),
        subtitle = paste0(
          "Window: L", input$span_left, "/R", input$span_right,
          " · min. freq: ", input$min_freq,
          " · corpus: ", format(length(token_vec()), big.mark = ","),
          " tokens"
        ),
        x = NULL,
        y = measure_label
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_text(colour = LADAL_PURPLE,
                                        face = "bold", size = 13),
        plot.subtitle    = element_text(colour = "#777", size = 9),
        strip.text       = element_text(colour = LADAL_PURPLE,
                                        face = "bold", size = 10.5,
                                        family = "Fira Sans"),
        strip.background = element_rect(fill = "#f4f0f8", colour = NA),
        axis.text.y      = element_text(size = 8.5,
                                        family = "Fira Code"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank()
      )
  })

  # ── Plot dimensions ────────────────────────────────────────
  plot_dims <- reactive({
    res <- coll_result()
    req(!is.null(res))
    top_n  <- as.integer(input$top_n %||% 20L)
    n_nodes <- length(unique(res$node))
    n_rows  <- ceiling(n_nodes / min(n_nodes, 3L))
    list(
      h_px = max(400L, n_rows * (top_n * 18L + 100L)),
      w_in = min(n_nodes, 3L) * 5 + 0.5,
      h_in = n_rows * (top_n * 0.28 + 1.8)
    )
  })

  # ── Main area ──────────────────────────────────────────────
  output$welcome_or_results <- renderUI({

    if (input$run_btn == 0 || is.null(input$files)) {
      return(div(class = "cc-welcome",
        div(class = "cc-welcome-icon", "🔗"),
        tags$h3("Calculate collocations from your own texts"),
        tags$p(
          "Upload plain-text files, enter one or more node words
           (comma-separated), set your window and filters,
           then click ", tags$b("Calculate collocations"), ".", br(), br(),
          "Nine association measures are computed for every
           (node, collocate) pair:", br(),
          tags$b("O/E · MI · MI2 · MI3 · G2 · t-score · ΔP12 · ΔP21 · Fisher p"),
          br(), br(),
          "The bar chart and full results table are available as
           PNG, PDF, Excel, and CSV downloads.", br(), br(),
          tags$a("→ Collocation & N-gram Tutorial",
                 href = "https://ladal.edu.au/tutorials/coll/coll.html")
        )
      ))
    }

    res <- coll_result()

    if (is.null(res) || nrow(res) == 0L) {
      return(div(class = "cc-warn",
        "⚠ No collocates found. Try lowering the minimum
         frequency, widening the window, or checking
         that your node word(s) appear in the corpus."))
    }

    n_nodes  <- length(unique(res$node))
    n_pairs  <- nrow(res)
    n_tokens <- length(token_vec())

    tagList(

      # Stat chips
      div(class = "cc-chips",
        div(class = "cc-chip",
          tags$b(format(n_tokens, big.mark = ",")), " tokens"),
        div(class = "cc-chip",
          tags$b(n_nodes), " node word", if (n_nodes != 1) "s"),
        div(class = "cc-chip",
          tags$b(format(n_pairs, big.mark = ",")),
          " collocate pair", if (n_pairs != 1) "s")
      ),

      tabsetPanel(

        # ── Bar chart tab ─────────────────────────────────
        tabPanel("📊 Bar chart",
          br(),
          div(class = "cc-plot-wrap",
            plotOutput("coll_plot",
                       height = paste0(plot_dims()$h_px, "px")),
            br(),
            uiOutput("plot_dl_ui")
          )
        ),

        # ── Results table tab ─────────────────────────────
        tabPanel("📋 Results table",
          br(),
          div(class = "cc-info",
            "All (node, collocate) pairs meeting the minimum
             frequency threshold, with all nine association
             measures. Use the column headers to sort."
          ),
          uiOutput("table_dl_ui"),
          br(),
          DTOutput("coll_dt")
        ),

        # ── Measure guide tab ─────────────────────────────
        tabPanel("ℹ️ Measure guide",
          br(),
          div(style = "max-width:680px;",
            tags$h4(style = paste0("color:", LADAL_PURPLE),
                    "Association Measures"),
            tags$p(style = "color:#555; font-size:.9rem;",
              "All measures are based on a 2×2 contingency table
               of observed and expected co-occurrences within
               the specified window."),
            DT::datatable(
              MEASURES[, c("label","desc")],
              colnames  = c("Measure", "Definition"),
              rownames  = FALSE,
              options   = list(dom = "t", pageLength = 20)
            ),
            tags$hr(),
            tags$h4(style = paste0("color:", LADAL_PURPLE),
                    "Key variables"),
            tags$ul(style = "font-size:.88rem; line-height:2;",
              tags$li(tags$b("O"), " — observed co-occurrence count"),
              tags$li(tags$b("E"), " — expected: (f_node × f_collocate) / N"),
              tags$li(tags$b("f_node"), " — corpus frequency of the node word"),
              tags$li(tags$b("f_collocate"),
                      " — corpus frequency of the collocate"),
              tags$li(tags$b("N"), " — total corpus size (tokens)"),
              tags$li(tags$b("ΔP12"),
                      " — directional: strength of node → collocate"),
              tags$li(tags$b("ΔP21"),
                      " — directional: strength of collocate → node"),
              tags$li(tags$b("Fisher p"),
                      " — two-tailed p-value; lower = stronger association")
            ),
            tags$p(style = "color:#888; font-size:.82rem;",
              "Reference: Gries, S.Th. (2013). ",
              tags$em("Statistics for Linguistics with R"),
              " (2nd ed.). De Gruyter Mouton."
            )
          )
        ),
        tabPanel(
          "⚙️ Parameters",
          br(),
          p(style="font-size:.85rem;color:#555;",
            "Download a record of all parameters used for reproducibility."),
          uiOutput("params_dl_ui"),
          br(),
          verbatimTextOutput("params_preview")
        )
      )
    )
  })

  # ── Render bar chart ───────────────────────────────────────
  output$coll_plot <- renderPlot({
    p <- build_plot()
    validate(need(!is.null(p), "No data to plot."))
    p
  }, bg = "white")

  # ── Plot download UI ───────────────────────────────────────
  output$plot_dl_ui <- renderUI({
    req(!is.null(build_plot()))
    tagList(
      downloadButton("dl_png", "⬇ PNG", class = "cc-dl-btn"),
      downloadButton("dl_pdf", "⬇ PDF", class = "cc-dl-btn")
    )
  })

  output$dl_png <- downloadHandler(
    filename = function()
      paste0("collocations_", Sys.Date(), ".png"),
    content = function(file) {
      d <- plot_dims()
      ggplot2::ggsave(file, plot = build_plot(), device = "png",
                      width = d$w_in, height = d$h_in,
                      dpi = 180, bg = "white")
    }
  )
  output$dl_pdf <- downloadHandler(
    filename = function()
      paste0("collocations_", Sys.Date(), ".pdf"),
    content = function(file) {
      d <- plot_dims()
      ggplot2::ggsave(file, plot = build_plot(), device = "pdf",
                      width = d$w_in, height = d$h_in, bg = "white")
    }
  )

  # ── Results DT ────────────────────────────────────────────
  output$coll_dt <- renderDT({
    res <- coll_result()
    req(!is.null(res) && nrow(res) > 0L)

    # Sort by chosen plot measure descending
    m <- input$plot_measure %||% "DeltaP12"
    res <- res[order(-get(m), na.last = TRUE), ]

    datatable(
      as.data.frame(res),
      rownames   = FALSE,
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 25,
        scrollX    = TRUE,
        columnDefs = list(
          list(className = "cc-mono", targets = c(0, 1))
        )
      )
    ) |>
    formatStyle("node",
                color = LADAL_PURPLE, fontWeight = "bold") |>
    formatStyle("collocate",
                color = "#333", fontFamily = "Fira Code, monospace") |>
    formatStyle(
      c("MI","MI2","MI3","G2","DeltaP12","DeltaP21"),
      background = styleColorBar(c(-5, 5), "#d8c8f0"),
      backgroundSize = "95% 60%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    )
  })

  # ── Table download UI ──────────────────────────────────────
  output$table_dl_ui <- renderUI({
    req(!is.null(coll_result()) && nrow(coll_result()) > 0L)
    tagList(
      downloadButton("dl_xlsx", "⬇ Excel (.xlsx)", class = "cc-dl-btn"),
      downloadButton("dl_csv",  "⬇ CSV (.csv)",    class = "cc-dl-btn")
    )
  })

  output$dl_xlsx <- downloadHandler(
    filename = function()
      paste0("collocations_", Sys.Date(), ".xlsx"),
    content = function(file)
      writexl::write_xlsx(as.data.frame(coll_result()), file)
  )
  output$dl_csv <- downloadHandler(
    filename = function()
      paste0("collocations_", Sys.Date(), ".csv"),
    content = function(file)
      readr::write_csv(as.data.frame(coll_result()), file)
  )

  # ── Sidebar download summary ───────────────────────────────
  output$download_buttons <- renderUI({
    if (input$run_btn == 0 || is.null(coll_result()))
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Run analysis to enable downloads."))
    div(class = "cc-info", style = "font-size:.81rem;",
        "Download buttons appear on each results tab.")
  })
}

# ── Run ───────────────────────────────────────────────────────
  # ── Parameters download ─────────────────────────────────────────
  output$dl_params <- downloadHandler(
    filename = function() paste0("collocationcalculator_params_", Sys.Date(), ".txt"),
    content  = function(file) {
      lines <- c(
        paste0("Tool:                ", "CollocationCalculator — Collocation Analysis"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("---                  ", ""),
        paste0("Node word(s):        ", input$nodes),
        paste0("Span left:           ", as.character(input$span_left)),
        paste0("Span right:          ", as.character(input$span_right)),
        paste0("Min frequency:       ", as.character(input$min_freq)),
        paste0("Top N:               ", as.character(input$top_n)),
        paste0("Plot measure:        ", input$plot_measure),
        paste0("Case-insensitive:    ", as.character(isTRUE(input$ignore_case))),
        paste0("Files:               ", if (!is.null(input$files)) paste(input$files$name, collapse=", ") else "none")
      )
      writeLines(lines, file)
    }
  )

  output$params_dl_ui <- renderUI({
    downloadButton("dl_params", "⬇ Download parameters (.txt)", class = "cc-dl-btn")
  })

  output$params_preview <- renderText({
    paste(c(
        paste0("Tool:                ", "CollocationCalculator — Collocation Analysis"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("---                  ", ""),
        paste0("Node word(s):        ", input$nodes),
        paste0("Span left:           ", as.character(input$span_left)),
        paste0("Span right:          ", as.character(input$span_right)),
        paste0("Min frequency:       ", as.character(input$min_freq)),
        paste0("Top N:               ", as.character(input$top_n)),
        paste0("Plot measure:        ", input$plot_measure),
        paste0("Case-insensitive:    ", as.character(isTRUE(input$ignore_case))),
    ), collapse="\n")
  })

shinyApp(ui, server)

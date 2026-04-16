# ============================================================
#  KeywordExtractor — LADAL Keyword Analysis Tool (Shiny)
#  https://ladal.edu.au
#  
#  Keyness statistics implemented with data.table for
#  efficiency, self-contained (no external script dependencies).
# ============================================================

library(shiny)
library(data.table)
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
library(writexl)
library(DT)
library(ggplot2)

quanteda_options(verbose = FALSE)

# ══════════════════════════════════════════════════════════════
#  KEYNESS ENGINE  (replaces loadkeytxts / prepkeydat /
#  keystats remote scripts, rewritten with data.table)
# ══════════════════════════════════════════════════════════════

# ── 1. Load texts from uploaded file list ─────────────────────
load_corpus <- function(file_df) {
  # file_df is input$files (datapath + name columns)
  texts <- vapply(file_df$datapath, function(p) {
    paste(readLines(p, warn = FALSE), collapse = " ")
  }, character(1))
  names(texts) <- tools::file_path_sans_ext(file_df$name)
  corpus(texts)
}

# ── 2. Build frequency table via data.table ───────────────────
#   Returns a data.table with columns: token, freq_target, freq_ref,
#   n_target, n_ref (corpus totals)
build_freq_dt <- function(corp_target, corp_ref) {

  tok_target <- tokens(corp_target,
                       remove_punct   = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = FALSE) |>
    tokens_tolower()

  tok_ref <- tokens(corp_ref,
                    remove_punct   = TRUE,
                    remove_symbols = TRUE,
                    remove_numbers = FALSE) |>
    tokens_tolower()

  # Convert to data.table frequency tables
  dt_target <- as.data.table(dfm(tok_target) |> convert(to = "data.frame"))
  dt_ref    <- as.data.table(dfm(tok_ref)    |> convert(to = "data.frame"))

  # Sum across documents → one row per token
  target_long <- melt(dt_target, id.vars = "doc_id",
                      variable.name = "token",
                      value.name    = "freq")[
    , .(freq_target = sum(freq)), by = token]

  ref_long <- melt(dt_ref, id.vars = "doc_id",
                   variable.name = "token",
                   value.name    = "freq")[
    , .(freq_ref = sum(freq)), by = token]

  # Full join so all tokens from both corpora are represented
  freq_dt <- merge(target_long, ref_long, by = "token", all = TRUE)
  freq_dt[is.na(freq_target), freq_target := 0L]
  freq_dt[is.na(freq_ref),    freq_ref    := 0L]

  # Corpus totals
  n_target <- sum(freq_dt$freq_target)
  n_ref    <- sum(freq_dt$freq_ref)

  freq_dt[, n_target := n_target]
  freq_dt[, n_ref    := n_ref]

  freq_dt
}

# ── 3. Compute keyness statistics ─────────────────────────────
#
#  All three measures compare:
#    a = freq_target   b = freq_ref
#    c = n_target - a  d = n_ref - b
#
#  G2  (log-likelihood ratio)
#    G2 = 2 * [ a*ln(a/E_a) + b*ln(b/E_b) ]
#    where E_a = n_target * (a+b) / N,  N = n_target + n_ref
#    Sign: positive → over-represented in target
#
#  Chi-squared (Pearson, with Yates correction option)
#    X2 = N * (|ad - bc| - N/2)^2 / [(a+b)(c+d)(a+c)(b+d)]
#
#  Log-ratio (Hardie 2014) — effect-size measure
#    LR = log2( (a / n_target) / (b / n_ref) )
#    (smoothed by adding 0.5 to avoid log(0))

calc_keyness <- function(freq_dt, measure = "G2", min_freq = 1L) {

  dt <- copy(freq_dt)

  # Filter rare tokens
  dt <- dt[freq_target >= min_freq | freq_ref >= min_freq]

  N  <- dt$n_target[1] + dt$n_ref[1]
  nT <- dt$n_target[1]
  nR <- dt$n_ref[1]

  a <- dt$freq_target
  b <- dt$freq_ref

  # ── G2 ──────────────────────────────────────────────────────
  E_a <- nT * (a + b) / N
  E_b <- nR * (a + b) / N

  # Safe log: 0 * log(0) → 0
  safe_log_ratio <- function(obs, exp) {
    ifelse(obs == 0, 0, obs * log(obs / exp))
  }

  g2_raw <- 2 * (safe_log_ratio(a, E_a) + safe_log_ratio(b, E_b))
  dt[, G2 := round(ifelse(a / nT >= b / nR, g2_raw, -g2_raw), 3)]

  # ── Chi-squared (Yates-corrected) ───────────────────────────
  cc <- nT - a        # c
  dd <- nR - b        # d
  ab <- a + b
  cd <- cc + dd       # = N - (a+b), i.e. col totals complement

  chi_raw <- N * (pmax(0, abs(a * dd - b * cc) - N / 2))^2 /
    (ab * cd * (a + cc) * (b + dd))
  dt[, Chi2 := round(ifelse(a / nT >= b / nR, chi_raw, -chi_raw), 3)]

  # ── Log-ratio (effect size) ──────────────────────────────────
  lr <- log2(((a + 0.5) / nT) / ((b + 0.5) / nR))
  dt[, LogRatio := round(lr, 3)]

  # ── Normalised frequencies per million ──────────────────────
  dt[, PerMil_Target := round(a / nT * 1e6, 1)]
  dt[, PerMil_Ref    := round(b / nR * 1e6, 1)]

  # ── Sort by chosen measure ───────────────────────────────────
  sort_col <- measure
  setorderv(dt, sort_col, order = -1L)

  # ── Label direction ─────────────────────────────────────────
  dt[, Direction := fifelse(
    get(sort_col) > 0, "Target keyword", "Reference keyword")]

  # Clean column order for display
  dt[, .(
    Token       = token,
    Direction,
    Freq_Target = freq_target,
    Freq_Ref    = freq_ref,
    PerMil_T    = PerMil_Target,
    PerMil_R    = PerMil_Ref,
    G2,
    Chi2,
    LogRatio
  )]
}

# ══════════════════════════════════════════════════════════════
#  COLOURS & CONSTANTS
# ══════════════════════════════════════════════════════════════

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"
COL_TARGET   <- "#51247a"   # purple for target keywords
COL_REF      <- "#e07b00"   # amber for reference keywords

# ══════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════

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
    tags$a("→ Tutorial", href = "https://ladal.edu.au/tutorials/key/key.html", target = "_blank",
           style = "font-size:.78rem;color:#51247a;")
  ),
  tags$blockquote(
    style = "border-left:3px solid #c8b8de;padding-left:12px;margin:0 0 10px 0;color:#444;",
    HTML(paste0(
      "Schweinberger, Martin. (2026). ",
      "<em>KeywordExtractor: A browser-based keyword analysis tool</em>. ",
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
        "@misc{schweinberger2026keywordextractor,\n",
        "  author       = {Schweinberger, Martin},\n",
        "  title        = {KeywordExtractor: A browser-based keyword analysis tool},\n",
        "  year         = {2026},\n",
        "  organization = {The University of Queensland},\n",
        "  url          = {https://ladal.edu.au/tools.html}\n",
        "}"
      )
    )
  )
)

ui <- fluidPage(
  title = "KeywordExtractor | LADAL",

  tags$head(tags$style(HTML(paste0("
    body { font-family: 'Segoe UI', Arial, sans-serif;
           background:#f7f4fb; color:#222; margin:0; }

    /* Banner */
    .ke-banner {
      background:", LADAL_PURPLE, ";
      color:white; padding:18px 32px 14px 32px;
      display:flex; align-items:center; gap:18px;
      border-bottom:4px solid ", LADAL_GOLD, ";
    }
    .ke-banner .ke-title { font-size:1.7rem; font-weight:700;
                            letter-spacing:.5px; margin:0; }
    .ke-banner .ke-sub   { font-size:.88rem; opacity:.85;
                            margin:2px 0 0 0; }

    /* Layout */
    .ke-body { display:flex; min-height:calc(100vh - 80px); }
    .ke-side  { width:320px; min-width:270px; max-width:350px;
                background:white;
                border-right:1px solid #e0d8ec;
                padding:22px 20px 30px 20px;
                box-shadow:2px 0 8px rgba(81,36,122,.06); }
    .ke-main  { flex:1; padding:24px 28px; overflow-x:auto; }

    /* Section headings in sidebar */
    .ke-sec {
      font-size:.75rem; font-weight:700; letter-spacing:1.2px;
      text-transform:uppercase; color:", LADAL_PURPLE, ";
      border-bottom:2px solid ", LADAL_GOLD, ";
      padding-bottom:4px; margin:20px 0 10px 0;
    }
    .ke-sec:first-child { margin-top:0; }

    /* Inputs */
    .form-control, .selectize-input {
      border:1.5px solid #d0c8e0 !important;
      border-radius:6px !important; font-size:.92rem !important;
    }
    label { font-size:.88rem; font-weight:600; color:#444; }

    /* Run button */
    #run_analysis {
      width:100%; background:", LADAL_PURPLE, " !important;
      border:none !important; color:white !important;
      font-weight:700; font-size:1rem; padding:10px;
      border-radius:7px; margin-top:6px; transition:background .2s;
    }
    #run_analysis:hover { background:#3a1860 !important; }

    /* Upload areas */
    .shiny-input-container .btn {
      background:white;
      border:1.5px dashed ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; width:100%;
    }

    /* Info / tip boxes */
    .ke-info {
      background:#f4f0f8; border-left:4px solid ", LADAL_PURPLE, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#444; margin-bottom:14px;
    }
    .ke-warn {
      background:#fff4e5; border-left:4px solid ", LADAL_GOLD, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#6b4000; margin-bottom:12px;
    }
    .ke-ok {
      background:#eafaf1; border-left:4px solid #27ae60;
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#1a6b3c; margin-bottom:8px;
    }

    /* Stat cards */
    .ke-stats { display:flex; gap:12px; margin-bottom:20px;
                flex-wrap:wrap; }
    .ke-card  { background:white; border-radius:9px;
                border-left:4px solid ", LADAL_PURPLE, ";
                padding:11px 16px; min-width:110px;
                box-shadow:0 1px 6px rgba(81,36,122,.08); }
    .ke-card .ke-val { font-size:1.5rem; font-weight:700;
                       color:", LADAL_PURPLE, "; line-height:1.1; }
    .ke-card .ke-lbl { font-size:.76rem; color:#888;
                       margin-top:2px; }

    /* Download buttons */
    .ke-dl {
      display:inline-block; margin:4px 5px 4px 0;
      background:white; border:1.5px solid ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; font-size:.84rem;
      padding:5px 12px; border-radius:6px; cursor:pointer;
      text-decoration:none; transition:all .15s;
    }
    .ke-dl:hover { background:", LADAL_PURPLE, "; color:white; }

    /* Tabs */
    .nav-tabs > li > a { color:", LADAL_PURPLE, ";
                          font-weight:600; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      border-top:3px solid ", LADAL_PURPLE, " !important;
      color:", LADAL_PURPLE, " !important;
    }

    /* DT header */
    table.dataTable thead th {
      background:", LADAL_PURPLE, " !important;
      color:white !important; font-weight:600;
      border-bottom:2px solid ", LADAL_GOLD, " !important;
    }
    table.dataTable tbody tr:hover { background:#f4f0f8 !important; }

    /* Footer */
    .ke-footer {
      background:#2d1a4a; color:#c8b8de;
      font-size:.78rem; padding:12px 32px;
      display:flex; gap:18px; align-items:center;
    }
    .ke-footer a { color:#d4b8f5; }

    /* Welcome */
    .ke-welcome { color:#888; text-align:center;
                  padding:50px 20px; font-size:.95rem; }
  ")))),

  # ── Banner ────────────────────────────────────────────────────
  div(class = "ke-banner",
    div(style = "font-size:2rem;", "🔑"),
    div(
      p(class = "ke-title", "KeywordExtractor"),
      p(class = "ke-sub",
        "Keyword & keyness analysis · ",
        tags$a("LADAL", href = "https://ladal.edu.au",
               style = "color:#f0c060;"))
    )
  ),

  # ── Body ──────────────────────────────────────────────────────
  div(class = "ke-body",

    # ── Sidebar ───────────────────────────────────────────────
    div(class = "ke-side",

      # STEP 1 — Target corpus
      div(class = "ke-sec", "① Target corpus"),
      div(class = "ke-info",
        "Upload the text(s) you want to analyse for distinctive vocabulary."),
      fileInput("target_files", NULL,
                multiple    = TRUE,
                accept      = ".txt",
                buttonLabel = "📂 Target .txt files"),
      uiOutput("target_status"),

      # STEP 2 — Reference corpus
      div(class = "ke-sec", "② Reference corpus"),
      div(class = "ke-info",
        "Upload the text(s) to compare against. Keywords are words
         over- or under-represented in the target relative to this."),
      fileInput("ref_files", NULL,
                multiple    = TRUE,
                accept      = ".txt",
                buttonLabel = "📂 Reference .txt files"),
      uiOutput("ref_status"),

      # STEP 3 — Settings
      div(class = "ke-sec", "③ Settings"),

      selectInput("measure", "Keyness statistic",
        choices = c(
          "G2 — log-likelihood (recommended)" = "G2",
          "Chi² — chi-squared (Yates)"        = "Chi2",
          "Log-ratio — effect size (Hardie)"  = "LogRatio"
        ),
        selected = "G2"),

      sliderInput("top_n", "Keywords shown in chart",
                  min = 5, max = 30, value = 10, step = 1),

      numericInput("min_freq", "Min. frequency (target or ref)",
                   value = 3, min = 1, step = 1),

      checkboxInput("only_target",
                    "Show only target keywords (positive scores)",
                    value = FALSE),

      actionButton("run_analysis", "🔑  Extract Keywords",
                   class = "btn-primary"),

      # STEP 4 — Download
      div(class = "ke-sec", "④ Download results"),
      uiOutput("download_buttons")
    ),

    # ── Main panel ──────────────────────────────────────────────
    div(class = "ke-main",
      uiOutput("welcome_box"),
      uiOutput("stats_cards"),
      uiOutput("results_ui")
    )
  ),

  # ── Footer ────────────────────────────────────────────────────
  div(class = "ke-footer",
    span("KeywordExtractor · LADAL · University of Queensland"),
    tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
    tags$a("Keyword Analysis Tutorial",
           href = "https://ladal.edu.au/tutorials/key/key.html"),
    tags$a("Cite this tool",
           href = "https://ladal.edu.au/about.html#citing")
  ),
  CITATION_FOOTER
)

# ══════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Corpora ─────────────────────────────────────────────────
  corp_target <- reactive({
    req(input$target_files)
    load_corpus(input$target_files)
  })

  corp_ref <- reactive({
    req(input$ref_files)
    load_corpus(input$ref_files)
  })

  # ── Status badges ───────────────────────────────────────────
  corpus_badge <- function(files_input, label) {
    renderUI({
      if (is.null(files_input)) {
        div(class = "ke-warn",
            paste0("⚠ No ", label, " files uploaded yet."))
      } else {
        n   <- nrow(files_input)
        lbl <- if (n == 1) "1 file" else paste(n, "files")
        div(class = "ke-ok",
            paste0("✔ ", lbl, ": ",
                   paste(tools::file_path_sans_ext(files_input$name),
                         collapse = ", ")))
      }
    })
  }

  output$target_status <- corpus_badge(isolate(input$target_files),
                                        "target")
  output$ref_status    <- corpus_badge(isolate(input$ref_files),
                                        "reference")

  observe({
    output$target_status <- corpus_badge(input$target_files, "target")
    output$ref_status    <- corpus_badge(input$ref_files,    "reference")
  })

  # ── Core computation (triggered by button) ──────────────────
  key_result <- eventReactive(input$run_analysis, {
    req(corp_target(), corp_ref())

    withProgress(message = "Computing keyness…", value = 0, {
      incProgress(0.3, detail = "Building frequency tables")
      freq_dt <- build_freq_dt(corp_target(), corp_ref())

      incProgress(0.5, detail = "Calculating statistics")
      keys    <- calc_keyness(freq_dt,
                               measure  = input$measure,
                               min_freq = input$min_freq)
      incProgress(0.2, detail = "Done")
      keys
    })
  })

  # ── Welcome box ─────────────────────────────────────────────
  output$welcome_box <- renderUI({
    if (input$run_analysis == 0) {
      div(class = "ke-info", style = "font-size:.93rem;",
        tags$b("Welcome to KeywordExtractor."), br(),
        "Upload your ", tags$b("target"), " texts (the corpus you want
         to analyse) and your ", tags$b("reference"), " texts (the
         comparison corpus), then click ",
        tags$b("Extract Keywords"), ".", br(), br(),
        "The tool calculates how over- or under-represented each word
         is in the target relative to the reference, using the keyness
         statistic of your choice.", br(), br(),
        tags$a("→ Learn more about keyword analysis",
               href = "https://ladal.edu.au/tutorials/key/key.html")
      )
    }
  })

  # ── Stat cards ──────────────────────────────────────────────
  output$stats_cards <- renderUI({
    req(input$run_analysis > 0)
    keys <- key_result()
    if (is.null(keys) || nrow(keys) == 0) return(NULL)

    n_target_kw <- nrow(keys[keys$Direction == "Target keyword", ])
    n_ref_kw    <- nrow(keys[keys$Direction == "Reference keyword", ])
    n_total     <- sum(keys$Freq_Target) + sum(keys$Freq_Ref)

    div(class = "ke-stats",
      div(class = "ke-card",
        div(class = "ke-val", nrow(keys)),
        div(class = "ke-lbl", "Total word types")),
      div(class = "ke-card",
        div(class = "ke-val", n_target_kw),
        div(class = "ke-lbl", "Target keywords")),
      div(class = "ke-card",
        div(class = "ke-val", n_ref_kw),
        div(class = "ke-lbl", "Reference keywords")),
      div(class = "ke-card",
        div(class = "ke-val",
            format(sum(keys$Freq_Target), big.mark = ",")),
        div(class = "ke-lbl", "Target tokens")),
      div(class = "ke-card",
        div(class = "ke-val",
            format(sum(keys$Freq_Ref), big.mark = ",")),
        div(class = "ke-lbl", "Reference tokens"))
    )
  })

  # ── Results UI (tabs) ────────────────────────────────────────
  output$results_ui <- renderUI({
    req(input$run_analysis > 0)
    keys <- key_result()
    if (is.null(keys) || nrow(keys) == 0) {
      return(div(class = "ke-welcome",
        "⚠ No results. Try lowering the minimum frequency threshold."))
    }
    tabsetPanel(
      tabPanel("📊 Chart", br(), plotOutput("key_plot", height = "480px")),
      tabPanel("📋 Full table", br(), DTOutput("key_table")),
      tabPanel("ℹ️ Measure guide", br(), uiOutput("measure_guide")),
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
  })

  # ── Helper: prepare plot data ────────────────────────────────
  plot_data <- reactive({
    req(key_result())
    keys <- key_result()
    measure <- input$measure

    if (input$only_target) {
      keys <- keys[keys$Direction == "Target keyword", ]
    }

    # Top N positive + top N negative
    pos <- keys[keys[[measure]] > 0, ]
    pos <- head(pos[order(-pos[[measure]]), ], input$top_n)

    neg <- keys[keys[[measure]] < 0, ]
    neg <- head(neg[order(neg[[measure]]), ], input$top_n)

    rbind(pos, neg)
  })

  # ── Bar chart ────────────────────────────────────────────────
  output$key_plot <- renderPlot({
    pd      <- plot_data()
    measure <- input$measure
    req(nrow(pd) > 0)

    pd$Token     <- factor(pd$Token,
                            levels = pd$Token[order(pd[[measure]])])
    pd$Direction <- factor(pd$Direction,
                            levels = c("Target keyword",
                                       "Reference keyword"))

    measure_label <- switch(measure,
      G2       = "G² (log-likelihood)",
      Chi2     = "χ² (chi-squared, Yates-corrected)",
      LogRatio = "Log-ratio (effect size)"
    )

    ggplot(pd, aes(x = Token,
                   y = .data[[measure]],
                   fill = Direction)) +
      geom_col(width = 0.7) +
      geom_text(aes(
        y     = ifelse(.data[[measure]] > 0,
                       .data[[measure]] - max(abs(.data[[measure]])) * 0.02,
                       .data[[measure]] + max(abs(.data[[measure]])) * 0.02),
        label = round(.data[[measure]], 1),
        hjust = ifelse(.data[[measure]] > 0, 1.1, -0.1)
      ), color = "white", size = 3.2, fontface = "bold") +
      coord_flip() +
      scale_fill_manual(
        values = c("Target keyword"    = COL_TARGET,
                   "Reference keyword" = COL_REF),
        name   = NULL
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
      labs(
        title    = paste0("Top keywords by ", measure_label),
        subtitle = paste0(
          "Purple = over-represented in target  |  ",
          "Amber = over-represented in reference"),
        x        = "Word",
        y        = measure_label
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(color = LADAL_PURPLE,
                                     face  = "bold", size = 14),
        plot.subtitle = element_text(color = "#666", size = 10),
        axis.text.y   = element_text(size = 11),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank()
      )
  })

  # ── Full interactive table ───────────────────────────────────
  output$key_table <- renderDT({
    keys    <- key_result()
    measure <- input$measure
    req(!is.null(keys) && nrow(keys) > 0)

    display <- as.data.frame(keys)

    datatable(
      display,
      rownames   = FALSE,
      filter     = "top",
      extensions = c("Buttons"),
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 25,
        autoWidth  = TRUE,
        order      = list(list(
          which(names(display) == measure) - 1L, "desc"))
      ),
      caption = htmltools::tags$caption(
        style = paste0("color:", LADAL_PURPLE,
                       "; font-weight:bold;"),
        "Keyness results — click any column header to sort"
      )
    ) |>
      formatStyle(
        "Direction",
        color = styleEqual(
          c("Target keyword", "Reference keyword"),
          c(COL_TARGET, COL_REF)
        ),
        fontWeight = "bold"
      ) |>
      formatStyle(
        measure,
        background = styleColorBar(
          c(min(keys[[measure]]), max(keys[[measure]])),
          "#d8c8f0"
        ),
        backgroundSize     = "98% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ── Measure guide tab ────────────────────────────────────────
  output$measure_guide <- renderUI({
    div(style = "max-width:700px; font-size:.92rem; line-height:1.7;",
      tags$h4(style = paste0("color:", LADAL_PURPLE), "G² — Log-likelihood ratio"),
      tags$p("Recommended for most corpus comparisons. Compares observed
              frequencies with expected frequencies under the null hypothesis
              of equal distribution. Robust to corpus size differences.
              Positive = over-represented in target; negative =
              over-represented in reference."),
      tags$p(tags$b("Rule of thumb: "), "G² ≥ 3.84 (p < .05) · ≥ 6.63 (p < .01)
              · ≥ 10.83 (p < .001)"),
      tags$hr(),
      tags$h4(style = paste0("color:", LADAL_PURPLE),
              "χ² — Chi-squared (Yates-corrected)"),
      tags$p("Classic significance test for frequency differences. The Yates
              correction reduces inflated chi-squared values in 2×2 tables.
              Sensitive to small expected frequencies — treat results
              cautiously when any expected cell count is below 5."),
      tags$p(tags$b("Rule of thumb: "), "χ² ≥ 3.84 (p < .05) · ≥ 6.63 (p < .01)"),
      tags$hr(),
      tags$h4(style = paste0("color:", LADAL_PURPLE),
              "Log-ratio — Effect size (Hardie 2014)"),
      tags$p("Measures the", tags$em("size"), "of the frequency difference,
              not its significance. Useful alongside G² or χ²: a word can be
              statistically significant but practically unimportant (common
              words), or have a large log-ratio but appear rarely."),
      tags$p(tags$b("Interpretation: "), "LR = 1 means the word is twice as
              common in the target; LR = −1 means twice as common in the
              reference; LR = 0 means no difference."),
      tags$hr(),
      tags$p(style = "color:#888; font-size:.83rem;",
        "References: Dunning (1993) for G²; Hardie (2014) for log-ratio. ",
        tags$a("LADAL Keyword Analysis Tutorial",
               href = "https://ladal.edu.au/tutorials/key/key.html"))
    )
  })

  # ── Download handlers ────────────────────────────────────────
  output$download_buttons <- renderUI({
    if (input$run_analysis == 0 ||
        is.null(key_result()) ||
        nrow(key_result()) == 0)
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Run an analysis to enable downloads."))

    tagList(
      downloadButton("dl_xlsx",  "⬇ Excel (.xlsx)", class = "ke-dl"),
      downloadButton("dl_csv",   "⬇ CSV (.csv)",    class = "ke-dl"),
      downloadButton("dl_plot",  "⬇ Chart (.png)",  class = "ke-dl")
    )
  })

  output$dl_xlsx <- downloadHandler(
    filename = function()
      paste0("keywords_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(as.data.frame(key_result()), file)
  )

  output$dl_csv <- downloadHandler(
    filename = function()
      paste0("keywords_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(as.data.frame(key_result()), file)
  )

  output$dl_plot <- downloadHandler(
    filename = function()
      paste0("keywords_chart_", Sys.Date(), ".png"),
    content  = function(file) {
      pd      <- plot_data()
      measure <- input$measure
      req(nrow(pd) > 0)

      pd$Token <- factor(pd$Token,
                          levels = pd$Token[order(pd[[measure]])])
      pd$Direction <- factor(pd$Direction,
                              levels = c("Target keyword",
                                         "Reference keyword"))
      measure_label <- switch(measure,
        G2       = "G² (log-likelihood)",
        Chi2     = "χ² (chi-squared)",
        LogRatio = "Log-ratio (effect size)"
      )
      p <- ggplot(pd, aes(x = Token,
                          y = .data[[measure]],
                          fill = Direction)) +
        geom_col(width = 0.7) +
        coord_flip() +
        scale_fill_manual(
          values = c("Target keyword"    = COL_TARGET,
                     "Reference keyword" = COL_REF),
          name   = NULL) +
        labs(title = paste0("Top keywords by ", measure_label),
             x = "Word", y = measure_label) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(color = LADAL_PURPLE,
                                        face = "bold"),
              legend.position = "none")

      ggsave(file, plot = p, width = 8, height = 6, dpi = 200,
             bg = "white")
    }
  )

  # ── Parameters download ─────────────────────────────────────────
  output$dl_params <- downloadHandler(
    filename = function() paste0("keywordextractor_params_", Sys.Date(), ".txt"),
    content  = function(file) {
      lines <- c(
        paste0("Tool:                ", "KeywordExtractor — Keyness Analysis"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("quanteda.textstats:  ", as.character(packageVersion('quanteda.textstats'))),
        paste0("---                  ", ""),
        paste0("Keyness measure:     ", input$measure),
        paste0("Min frequency:       ", as.character(input$min_freq)),
        paste0("Top N:               ", as.character(input$top_n)),
        paste0("Target only:         ", as.character(input$only_target)),
        paste0("Files:               ", if (!is.null(input$files)) paste(input$files$name, collapse=", ") else "none")
      )
      writeLines(lines, file)
    }
  )

  output$params_dl_ui <- renderUI({
    downloadButton("dl_params", "⬇ Download parameters (.txt)", class = "ke-dl")
  })

  output$params_preview <- renderText({
    paste(c(
        paste0("Tool:                ", "KeywordExtractor — Keyness Analysis"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("quanteda.textstats:  ", as.character(packageVersion('quanteda.textstats'))),
        paste0("---                  ", ""),
        paste0("Keyness measure:     ", input$measure),
        paste0("Min frequency:       ", as.character(input$min_freq)),
        paste0("Top N:               ", as.character(input$top_n)),
        paste0("Target only:         ", as.character(input$only_target))
    ), collapse="\n")
  })

}

# ══════════════════════════════════════════════════════════════)

shinyApp(ui, server)

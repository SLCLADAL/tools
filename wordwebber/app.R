# ============================================================
#  WordWebber — LADAL Word Co-occurrence Network Tool
#  https://ladal.edu.au
#
#  Pipeline:
#    .txt files → corpus → tokens (lowercase, non-word removal,
#    optional stopword removal, optional lemmatisation)
#    → DFM (for word frequencies) + FCM tri=FALSE (symmetric,
#    sparse) → fcm_select neighbourhood → MI scores + raw cooc
#    → static textplot_network + interactive visNetwork
#
#  Fixes vs previous version:
#    1. BUG FIX: tri=FALSE — tri=TRUE caused keyword lookup to
#       miss co-occurrences stored in the upper triangle,
#       returning "keyword not found" for words like "australia".
#    2. Explicit non-word character removal (tokens_remove regex).
#    3. Explicit tokens_tolower (was present but now documented).
#    4. Optional English lemmatisation via textstem.
#    5. Node size proportional to unigram corpus frequency.
#    6. Edge width proportional to MI score (log2 O/E), not raw
#       co-occurrence count. Raw count still shown in tooltip.
# ============================================================

library(shiny)
library(quanteda)
library(quanteda.textplots)
library(dplyr)        # replaces tidyverse — only dplyr needed at runtime
library(ggplot2)
library(tibble)
library(readr)
library(writexl)
library(DT)
library(visNetwork)
# igraph removed — not called directly; visNetwork/quanteda handle graph internals
# textstem loaded lazily inside build_tokens() — only when lemmatisation is used

quanteda_options(verbose = FALSE)

# ══════════════════════════════════════════════════════════════
#  CONSTANTS
# ══════════════════════════════════════════════════════════════

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

STOPWORD_LANGS <- c(
  "None (keep all words)" = "none",
  "English"               = "en",
  "German"                = "de",
  "French"                = "fr",
  "Spanish"               = "es",
  "Italian"               = "it",
  "Dutch"                 = "nl",
  "Portuguese"            = "pt",
  "Russian"               = "ru",
  "Arabic"                = "ar"
)

# ══════════════════════════════════════════════════════════════
#  HELPERS
# ══════════════════════════════════════════════════════════════

load_corpus <- function(file_df) {
  texts <- vapply(file_df$datapath, function(p)
    paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = " "),
    character(1))
  names(texts) <- tools::file_path_sans_ext(file_df$name)
  corpus(texts)
}

# Tokenise corpus: lowercase, strip non-word characters, optional
# stopword removal, optional lemmatisation.
build_tokens <- function(corp, stopword_lang, lemmatise = FALSE) {
  
  toks <- tokens(corp,
                 remove_punct   = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE,
                 remove_url     = TRUE) |>
    tokens_tolower() |>
    # Remove any remaining non-alphabetic tokens (e.g. lone hyphens,
    # underscores, tokens that survived remove_punct)
    tokens_remove(pattern   = "^[^a-z]+$",
                  valuetype = "regex") |>
    # Remove very short tokens (single letters add noise)
    tokens_remove(pattern   = "^.{1}$",
                  valuetype = "regex")
  
  if (stopword_lang != "none") {
    toks <- tokens_remove(toks,
                          pattern = stopwords(stopword_lang),
                          padding = FALSE)
  }
  
  if (lemmatise) {
    # Load textstem lazily — it pulls in koRpus/sylly (~15 packages) so we
    # only pay that cost when the user actually requests lemmatisation.
    if (!requireNamespace("textstem", quietly = TRUE)) {
      stop("The 'textstem' package is required for lemmatisation. ",
           "Please ask the administrator to install it.")
    }
    toks <- tokens_replace(
      toks,
      pattern     = types(toks),
      replacement = textstem::lemmatize_words(types(toks))
    )
  }
  
  toks
}

# Build FCM.
# BUGFIX: tri=FALSE — with tri=TRUE, fcm_select() only retrieves
# the row for the keyword, missing all co-occurrences stored in
# OTHER words' rows where the keyword is the column (upper-tri).
# tri=FALSE stores both directions; the sparse matrix is still
# sparse. Given we only materialise a small neighbourhood submatrix
# later, the extra memory is negligible.
build_fcm <- function(toks, window_size) {
  fcm(toks,
      context = "window",
      window  = window_size,
      ordered = FALSE,
      tri     = FALSE)   # BUGFIX: was TRUE — caused "not found" errors
}

# Build DFM for unigram frequencies (used for node sizing).
build_dfm_freq <- function(toks) {
  dfm(toks) |> colSums()
}

# Compute MI score: log2(O / E) where E = f_node * f_collocate / N_pairs
# N_pairs = total number of (token, window-token) pairs in the corpus
# For a symmetric window of size w and corpus of N tokens:
#   N_pairs ≈ 2 * w * N  (approximation; we use sum of FCM as exact count)
compute_mi <- function(O, f_node, f_collocate, N_pairs) {
  E  <- (f_node * f_collocate) / N_pairs
  MI <- ifelse(E > 0 & O > 0, log2(O / E), NA_real_)
  round(MI, 4)
}

# Extract co-occurrence edges for a keyword directly from the sparse FCM.
#
# KEY FIX: do NOT use fcm_select() to extract a keyword row.
# fcm_select() keeps matching COLUMNS and returns a V×1 matrix; indexing
# that by the keyword ROW name then gives a length-1 vector (self only),
# which is why every keyword appeared "not found".
#
# Instead we index the sparse FCM matrix directly by row position, which
# gives the full co-occurrence vector for the keyword in O(k) time where
# k = number of non-zero entries in that row.
keyword_cooc <- function(fcm_obj, freq_vec, keyword, top_n, min_freq) {
  kw        <- tolower(trimws(keyword))
  all_feats <- featnames(fcm_obj)
  
  if (!kw %in% all_feats) return(NULL)
  
  # Direct sparse-row extraction — correct and efficient
  kw_idx <- which(all_feats == kw)
  kw_row <- fcm_obj[kw_idx, , drop = FALSE]          # 1 × V sparse matrix
  kw_vec <- as.numeric(kw_row)                        # dense vector length V
  names(kw_vec) <- all_feats
  
  # With tri=FALSE the matrix is symmetric, so also check the column in case
  # quanteda stores anything asymmetrically (defensive, usually redundant)
  kw_col <- fcm_obj[, kw_idx, drop = FALSE]           # V × 1 sparse matrix
  kw_vec2 <- as.numeric(kw_col)
  names(kw_vec2) <- all_feats
  # Take element-wise max to catch any asymmetric entries
  kw_vec <- pmax(kw_vec, kw_vec2)
  names(kw_vec) <- all_feats
  
  # Remove self and apply minimum frequency threshold
  kw_vec <- kw_vec[names(kw_vec) != kw]
  kw_vec <- kw_vec[kw_vec >= min_freq]
  
  if (length(kw_vec) == 0L) return(NULL)
  
  kw_vec <- sort(kw_vec, decreasing = TRUE)
  kw_vec <- head(kw_vec, top_n)
  
  collocates <- names(kw_vec)
  O_vals     <- as.integer(kw_vec)
  
  # Frequencies for MI computation
  f_node       <- freq_vec[kw]
  f_node       <- ifelse(is.na(f_node), 1L, as.integer(f_node))
  f_collocates <- as.integer(
    ifelse(is.na(freq_vec[collocates]), 1L, freq_vec[collocates])
  )
  
  # N_pairs: total co-occurrence slots in the FCM (exact denominator for MI)
  N_pairs <- sum(fcm_obj)
  if (N_pairs == 0) N_pairs <- 1
  
  MI_vals <- compute_mi(O_vals, f_node, f_collocates, N_pairs)
  
  tibble::tibble(
    from  = kw,
    to    = collocates,
    cooc  = O_vals,
    MI    = MI_vals,
    f_w2  = f_collocates
  )
}

# Build the small neighbourhood sub-FCM for textplot_network.
# Uses direct sparse matrix row/col indexing — NOT fcm_select() —
# for the same reason as keyword_cooc: fcm_select selects columns
# and produces a V×k matrix, whereas we need a k×k submatrix.
neighbourhood_fcm <- function(fcm_obj, edges) {
  if (is.null(edges) || nrow(edges) == 0L) return(NULL)
  
  nodes_keep <- unique(c(edges$from, edges$to))
  all_feats  <- featnames(fcm_obj)
  keep_idx   <- which(all_feats %in% nodes_keep)
  
  if (length(keep_idx) == 0L) return(NULL)
  
  # Direct k×k submatrix — safe to densify at top_n scale
  sub_mat <- as.matrix(fcm_obj[keep_idx, keep_idx, drop = FALSE])
  rownames(sub_mat) <- all_feats[keep_idx]
  colnames(sub_mat) <- all_feats[keep_idx]
  
  # Symmetrise (defensive — should already be symmetric with tri=FALSE)
  sub_mat <- sub_mat + t(sub_mat)
  diag(sub_mat) <- 0
  
  as.fcm(sub_mat)
}

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
    tags$a("→ Tutorial", href = "https://ladal.edu.au/tutorials/net/net.html", target = "_blank",
           style = "font-size:.78rem;color:#51247a;")
  ),
  tags$blockquote(
    style = "border-left:3px solid #c8b8de;padding-left:12px;margin:0 0 10px 0;color:#444;",
    HTML(paste0(
      "Schweinberger, Martin. (2025). ",
      "<em>WordWebber: A browser-based word co-occurrence network tool</em>. ",
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
        "@misc{schweinberger2025wordwebber,\n",
        "  author       = {Schweinberger, Martin},\n",
        "  title        = {WordWebber: A browser-based word co-occurrence network tool},\n",
        "  year         = {2025},\n",
        "  organization = {The University of Queensland},\n",
        "  url          = {https://ladal.edu.au/tools.html}\n",
        "}"
      )
    )
  )
)

ui <- fluidPage(
  title = "WordWebber | LADAL",
  
  tags$head(tags$style(HTML(paste0("
    body { font-family:'Segoe UI',Arial,sans-serif;
           background:#f7f4fb; color:#222; margin:0; }

    /* Banner */
    .ww-banner {
      background:", LADAL_PURPLE, ";
      color:white; padding:18px 32px 14px 32px;
      display:flex; align-items:center; gap:18px;
      border-bottom:4px solid ", LADAL_GOLD, ";
    }
    .ww-banner .ww-title { font-size:1.7rem; font-weight:700;
                            letter-spacing:.5px; margin:0; }
    .ww-banner .ww-sub   { font-size:.88rem; opacity:.85;
                            margin:2px 0 0 0; }

    /* Layout */
    .ww-body { display:flex; min-height:calc(100vh - 80px); }
    .ww-side  { width:320px; min-width:270px; max-width:350px;
                background:white;
                border-right:1px solid #e0d8ec;
                padding:22px 20px 30px 20px;
                box-shadow:2px 0 8px rgba(81,36,122,.06);
                overflow-y:auto; }
    .ww-main  { flex:1; padding:24px 28px; overflow-x:auto; }

    /* Sidebar section headings */
    .ww-sec {
      font-size:.75rem; font-weight:700; letter-spacing:1.2px;
      text-transform:uppercase; color:", LADAL_PURPLE, ";
      border-bottom:2px solid ", LADAL_GOLD, ";
      padding-bottom:4px; margin:20px 0 10px 0;
    }
    .ww-sec:first-child { margin-top:0; }

    /* Inputs */
    .form-control, .selectize-input {
      border:1.5px solid #d0c8e0 !important;
      border-radius:6px !important; font-size:.92rem !important;
    }
    label { font-size:.88rem; font-weight:600; color:#444; }

    /* Run button */
    #run_network {
      width:100%; background:", LADAL_PURPLE, " !important;
      border:none !important; color:white !important;
      font-weight:700; font-size:1rem; padding:10px;
      border-radius:7px; margin-top:6px; transition:background .2s;
    }
    #run_network:hover { background:#3a1860 !important; }

    /* Upload area */
    .shiny-input-container .btn {
      background:white;
      border:1.5px dashed ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; width:100%;
    }

    /* Info boxes */
    .ww-info {
      background:#f4f0f8; border-left:4px solid ", LADAL_PURPLE, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#444; margin-bottom:12px;
    }
    .ww-warn {
      background:#fff4e5; border-left:4px solid ", LADAL_GOLD, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#6b4000; margin-bottom:10px;
    }
    .ww-ok {
      background:#eafaf1; border-left:4px solid #27ae60;
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#1a6b3c; margin-bottom:8px;
    }

    /* Stat cards */
    .ww-stats { display:flex; gap:12px; margin-bottom:20px;
                flex-wrap:wrap; }
    .ww-card  { background:white; border-radius:9px;
                border-left:4px solid ", LADAL_PURPLE, ";
                padding:11px 16px; min-width:110px;
                box-shadow:0 1px 6px rgba(81,36,122,.08); }
    .ww-card .ww-val { font-size:1.5rem; font-weight:700;
                       color:", LADAL_PURPLE, "; line-height:1.1; }
    .ww-card .ww-lbl { font-size:.76rem; color:#888; margin-top:2px; }

    /* Download buttons */
    .ww-dl {
      display:inline-block; margin:4px 5px 4px 0;
      background:white; border:1.5px solid ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; font-size:.84rem;
      padding:5px 12px; border-radius:6px; cursor:pointer;
      text-decoration:none; transition:all .15s;
    }
    .ww-dl:hover { background:", LADAL_PURPLE, "; color:white; }

    /* Tabs */
    .nav-tabs > li > a { color:", LADAL_PURPLE, "; font-weight:600; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      border-top:3px solid ", LADAL_PURPLE, " !important;
      color:", LADAL_PURPLE, " !important;
    }

    /* DT */
    table.dataTable thead th {
      background:", LADAL_PURPLE, " !important;
      color:white !important; font-weight:600;
      border-bottom:2px solid ", LADAL_GOLD, " !important;
    }
    table.dataTable tbody tr:hover { background:#f4f0f8 !important; }

    /* Footer */
    .ww-footer {
      background:#2d1a4a; color:#c8b8de;
      font-size:.78rem; padding:12px 32px;
      display:flex; gap:18px; align-items:center;
    }
    .ww-footer a { color:#d4b8f5; }

    /* visNetwork container */
    .vis-container { border:1px solid #e0d8ec; border-radius:8px;
                     background:white; }
  ")))),
  
  # ── Banner ──────────────────────────────────────────────────
  div(class = "ww-banner",
      div(style = "font-size:2rem;", "🕸️"),
      div(
        p(class = "ww-title", "WordWebber"),
        p(class = "ww-sub",
          "Word co-occurrence network analysis · ",
          tags$a("LADAL", href = "https://ladal.edu.au",
                 style = "color:#f0c060;"))
      )
  ),
  
  # ── Body ────────────────────────────────────────────────────
  div(class = "ww-body",
      
      # ── Sidebar ───────────────────────────────────────────────
      div(class = "ww-side",
          
          # STEP 1 — Upload
          div(class = "ww-sec", "① Upload texts"),
          div(class = "ww-info",
              "Upload one or more ", tags$b(".txt"), " files.
         All files are merged into one corpus."),
          fileInput("files", NULL,
                    multiple    = TRUE,
                    accept      = ".txt",
                    buttonLabel = "📂 Choose .txt files"),
          uiOutput("corpus_status"),
          
          # STEP 2 — Keyword
          div(class = "ww-sec", "② Keyword"),
          div(class = "ww-info",
              "The network shows words that co-occur most with this
         keyword within the context window. Matching is always
         case-insensitive."),
          textInput("keyword", "Keyword", value = "",
                    placeholder = "e.g. climate, australia, …"),
          
          # STEP 3 — Text processing
          div(class = "ww-sec", "③ Text processing"),
          
          selectInput("stopword_lang", "Remove stopwords",
                      choices  = STOPWORD_LANGS,
                      selected = "en"),
          
          checkboxInput("lemmatise",
                        "Lemmatise tokens (English only)",
                        value = FALSE),
          div(class = "ww-info", style = "margin-top:-6px;",
              "Lemmatisation reduces words to their base form
         (e.g. ", tags$em("running → run"), ", ",
              tags$em("better → good"), "). Uses ",
              tags$a("textstem", href = "https://cran.r-project.org/package=textstem"),
              " — English only."
          ),
          
          # STEP 4 — Network settings
          div(class = "ww-sec", "④ Network settings"),
          
          sliderInput("window_size", "Context window (words each side)",
                      min = 1, max = 15, value = 5, step = 1),
          
          sliderInput("top_n", "Max co-occurring words shown",
                      min = 5, max = 50, value = 20, step = 1),
          
          numericInput("min_freq", "Min. co-occurrence count",
                       value = 2, min = 1, step = 1),
          
          numericInput("min_mi", "Min. MI score (edge filter)",
                       value = 0, min = -10, max = 20, step = 0.5),
          
          # STEP 5 — Visual options
          div(class = "ww-sec", "⑤ Visual options"),
          
          selectInput("node_size_by", "Node size proportional to",
                      choices  = c("Word frequency in corpus" = "freq",
                                   "Fixed size (all equal)"   = "fixed"),
                      selected = "freq"),
          
          selectInput("edge_color", "Edge colour",
                      choices  = c("Gray"   = "gray60",
                                   "Purple" = "#8e44ad",
                                   "Blue"   = "steelblue",
                                   "Green"  = "#27ae60",
                                   "Black"  = "gray10"),
                      selected = "gray60"),
          
          sliderInput("edge_alpha", "Edge transparency",
                      min = 0.1, max = 1.0, value = 0.6, step = 0.05),
          
          sliderInput("node_size", "Node size (base / fixed)",
                      min = 0.5, max = 5, value = 2, step = 0.5),
          
          sliderInput("label_size", "Label size",
                      min = 2, max = 10, value = 5, step = 0.5),
          
          actionButton("run_network", "🕸️  Build Network",
                       class = "btn-primary"),
          
          # STEP 6 — Download
          div(class = "ww-sec", "⑥ Download"),
          uiOutput("download_buttons")
      ),
      
      # ── Main panel ──────────────────────────────────────────
      div(class = "ww-main",
          uiOutput("welcome_box"),
          uiOutput("stats_cards"),
          uiOutput("results_ui")
      )
  ),
  
  # ── Footer ──────────────────────────────────────────────────
  div(class = "ww-footer",
      span("WordWebber · LADAL · University of Queensland"),
      tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
      tags$a("Network Analysis Tutorial",
             href = "https://ladal.edu.au/tutorials/net/net.html"),
      tags$a("Cite this tool",
             href = "https://ladal.edu.au/about.html#citing")
  ),
  CITATION_FOOTER
)

# ══════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── Corpus (built once on upload) ────────────────────────
  corp <- reactive({
    req(input$files)
    load_corpus(input$files)
  })
  
  # ── All heavy computation inside one eventReactive ────────
  # Nothing expensive fires until the user clicks the button.
  net_data <- eventReactive(input$run_network, {
    req(corp(), nchar(trimws(input$keyword)) > 0)
    
    kw <- tolower(trimws(input$keyword))
    
    withProgress(message = "Building network…", value = 0, {
      
      incProgress(0.15, detail = "Tokenising & cleaning corpus")
      toks <- build_tokens(corp(),
                           stopword_lang = input$stopword_lang,
                           lemmatise     = isTRUE(input$lemmatise))
      
      incProgress(0.15, detail = "Computing word frequencies")
      freq_vec <- build_dfm_freq(toks)
      
      incProgress(0.25, detail = "Building co-occurrence matrix")
      fcm_obj <- build_fcm(toks, window_size = input$window_size)
      
      incProgress(0.25, detail = "Extracting keyword neighbourhood")
      ed <- keyword_cooc(fcm_obj, freq_vec, kw,
                         top_n    = input$top_n,
                         min_freq = input$min_freq)
      
      if (is.null(ed) || nrow(ed) == 0L) {
        incProgress(0.2)
        return(NULL)
      }
      
      # Apply MI filter
      min_mi <- as.numeric(input$min_mi %||% 0)
      ed <- ed[!is.na(ed$MI) & ed$MI >= min_mi, ]
      
      if (nrow(ed) == 0L) {
        incProgress(0.2)
        return(NULL)
      }
      
      incProgress(0.15, detail = "Preparing graph")
      sub_fcm <- neighbourhood_fcm(fcm_obj, ed)
      
      incProgress(0.05, detail = "Done")
      
      list(
        edges    = ed,
        sub_fcm  = sub_fcm,
        keyword  = kw,
        freq_vec = freq_vec,
        n_tokens = sum(ntoken(corp()))
      )
    })
  })
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # ── Corpus status ────────────────────────────────────────
  output$corpus_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "ww-warn", "⚠ No files uploaded yet.")
    } else {
      n   <- nrow(input$files)
      lbl <- if (n == 1) "1 file loaded" else paste(n, "files loaded")
      div(class = "ww-ok",
          paste0("✔ ", lbl, ": ",
                 paste(tools::file_path_sans_ext(input$files$name),
                       collapse = ", ")))
    }
  })
  
  # ── Welcome box ──────────────────────────────────────────
  output$welcome_box <- renderUI({
    if (input$run_network == 0) {
      div(class = "ww-info", style = "font-size:.93rem;",
          tags$b("Welcome to WordWebber."), br(),
          "Upload your plain-text files, enter a keyword, and click ",
          tags$b("Build Network"), " to visualise which words
         co-occur most frequently with that keyword.", br(), br(),
          "Texts are automatically lowercased and cleaned. You can
         optionally remove stopwords and lemmatise tokens.", br(), br(),
          tags$b("Node size"), " reflects each word's overall corpus
         frequency. ", tags$b("Edge thickness"), " reflects
         MI score — how strongly the word is attracted to the
         keyword beyond chance.", br(), br(),
          tags$a("→ Learn more about network analysis",
                 href = "https://ladal.edu.au/tutorials/net/net.html")
      )
    }
  })
  
  # ── Stat cards ───────────────────────────────────────────
  output$stats_cards <- renderUI({
    req(input$run_network > 0)
    nd <- net_data()
    if (is.null(nd)) return(NULL)
    
    top_mi   <- nd$edges |> dplyr::slice_max(MI, n = 1, with_ties = FALSE)
    top_word <- if (nrow(top_mi) > 0) top_mi$to[1] else "—"
    top_mi_v <- if (nrow(top_mi) > 0) round(top_mi$MI[1], 2) else "—"
    
    div(class = "ww-stats",
        div(class = "ww-card",
            div(class = "ww-val", nrow(nd$edges)),
            div(class = "ww-lbl", "Co-occurring words")),
        div(class = "ww-card",
            div(class = "ww-val", sum(nd$edges$cooc)),
            div(class = "ww-lbl", "Total co-occurrences")),
        div(class = "ww-card",
            div(class = "ww-val", top_word),
            div(class = "ww-lbl", paste0("Top MI: ", top_mi_v))),
        div(class = "ww-card",
            div(class = "ww-val", format(nd$n_tokens, big.mark = ",")),
            div(class = "ww-lbl", "Corpus tokens"))
    )
  })
  
  # ── Results UI ───────────────────────────────────────────
  output$results_ui <- renderUI({
    req(input$run_network > 0)
    nd <- net_data()
    
    if (is.null(nd)) {
      kw <- tolower(trimws(input$keyword))
      return(div(class = "ww-warn",
                 paste0("⚠ No results for keyword '", kw, "'. "),
                 br(),
                 "Possible reasons: the word does not appear in the corpus after
         cleaning and stopword removal; no co-occurrences meet the
         minimum frequency or MI threshold; or the word was removed
         as a stopword.", br(), br(),
                 "Try: lower minimum frequency · lower MI threshold · a
         different stopword language · disabling lemmatisation."
      ))
    }
    
    tabsetPanel(
      tabPanel("🕸️ Interactive network", br(),
               visNetworkOutput("vis_net", height = "560px")),
      tabPanel("📊 Static network",      br(),
               plotOutput("static_net",  height = "520px")),
      tabPanel("📋 Co-occurrence table", br(),
               DTOutput("cooc_table")),
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
  
  # ── Helper: node size vector ─────────────────────────────
  # Scales corpus frequency to a 1–max_size range for visNetwork.
  node_sizes <- function(ids, kw, freq_vec, node_size_by, base_size) {
    if (node_size_by == "fixed") {
      sizes <- rep(base_size * 15, length(ids))
      sizes[ids == kw] <- base_size * 22
      return(sizes)
    }
    # Frequency-proportional
    freqs <- as.numeric(freq_vec[ids])
    freqs[is.na(freqs)] <- 1
    max_f <- max(freqs, 1)
    # Scale to [base_size*8, base_size*30], keyword gets a boost
    sizes <- base_size * 8 + (freqs / max_f) * base_size * 22
    sizes[ids == kw] <- max(sizes) * 1.2
    sizes
  }
  
  # ── Interactive network ──────────────────────────────────
  output$vis_net <- renderVisNetwork({
    nd <- net_data()
    req(!is.null(nd))
    
    ed        <- nd$edges
    kw        <- nd$keyword
    freq_vec  <- nd$freq_vec
    all_nodes <- unique(c(ed$from, ed$to))
    
    # Edge width: MI score, rescaled to [1, 8]
    mi_vals    <- ed$MI
    mi_min     <- min(mi_vals, na.rm = TRUE)
    mi_max     <- max(mi_vals, na.rm = TRUE)
    mi_range   <- max(mi_max - mi_min, 0.001)
    edge_width <- 1 + ((mi_vals - mi_min) / mi_range) * 7
    edge_width[is.na(edge_width)] <- 1
    
    # Build node table row by row to avoid vectorisation issues
    # with named-vector indexing and match() returning NA for keyword
    vis_nodes <- do.call(rbind, lapply(all_nodes, function(node_id) {
      f      <- as.integer(freq_vec[node_id])
      if (is.na(f)) f <- 0L
      is_kw  <- node_id == kw
      cooc_i <- ed$cooc[match(node_id, ed$to)]
      mi_i   <- ed$MI[match(node_id, ed$to)]
      
      tooltip <- if (is_kw) {
        paste0("<b>", node_id, "</b><br>keyword<br>freq: ", f)
      } else {
        paste0("<b>", node_id, "</b>",
               "<br>co-occ: ",   ifelse(is.na(cooc_i), "—", cooc_i),
               "<br>MI: ",       ifelse(is.na(mi_i),   "—",
                                        round(mi_i, 2)),
               "<br>freq: ", f)
      }
      
      data.frame(
        id               = node_id,
        label            = node_id,
        value            = node_sizes(node_id, kw, freq_vec,
                                      input$node_size_by,
                                      input$node_size),
        color.background = if (is_kw) LADAL_PURPLE else "#a585c8",
        color.border     = if (is_kw) LADAL_GOLD   else "#7a5ba8",
        color.highlight  = LADAL_GOLD,
        font.size        = if (is_kw) 18L else 13L,
        font.bold        = is_kw,
        title            = tooltip,
        stringsAsFactors = FALSE
      )
    }))
    
    vis_edges <- data.frame(
      from  = ed$from,
      to    = ed$to,
      width = edge_width,
      title = paste0("Co-occurrences: ", ed$cooc,
                     "<br>MI score: ",   round(ed$MI, 3)),
      color = input$edge_color,
      stringsAsFactors = FALSE
    )
    
    visNetwork::visNetwork(
      nodes  = vis_nodes,
      edges  = vis_edges,
      width  = "100%",
      height = "560px",
      main   = list(
        text  = paste0("Co-occurrence network: '", kw, "'"),
        style = paste0("color:", LADAL_PURPLE,
                       "; font-weight:bold; font-size:15px;")
      )
    ) |>
      visNetwork::visNodes(
        shape       = "dot",
        font        = list(face = "arial"),
        borderWidth = 1.5,
        shadow      = list(enabled = TRUE, size = 4)
      ) |>
      visNetwork::visEdges(
        color  = list(color     = input$edge_color,
                      highlight = LADAL_GOLD,
                      opacity   = input$edge_alpha),
        smooth = list(enabled   = TRUE,
                      type      = "curvedCW",
                      roundness = 0.1),
        shadow = FALSE
      ) |>
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1,
                                hover   = TRUE),
        nodesIdSelection = TRUE
      ) |>
      visNetwork::visLayout(randomSeed = 42) |>
      visNetwork::visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -60,
          centralGravity        = 0.01,
          springLength          = 120,
          springConstant        = 0.08
        ),
        stabilization = list(enabled    = TRUE,
                             iterations = 100,
                             fit        = TRUE)
      ) |>
      visNetwork::visInteraction(
        navigationButtons = TRUE,
        tooltipDelay      = 80
      )
  })
  
  # ── Static network ───────────────────────────────────────
  output$static_net <- renderPlot({
    nd <- net_data()
    req(!is.null(nd) && !is.null(nd$sub_fcm))
    
    kw       <- nd$keyword
    ed       <- nd$edges
    sub_fcm  <- nd$sub_fcm
    freq_vec <- nd$freq_vec
    feats    <- featnames(sub_fcm)
    
    # Vertex sizes: frequency-proportional or fixed
    if (input$node_size_by == "freq") {
      freqs      <- as.numeric(freq_vec[feats])
      freqs[is.na(freqs)] <- 1
      v_sizes    <- input$node_size * 0.5 +
        input$node_size * 1.5 * (freqs / max(freqs, 1))
      v_sizes[feats == kw] <- max(v_sizes) * 1.15
    } else {
      v_sizes <- rep(input$node_size, length(feats))
      v_sizes[feats == kw] <- input$node_size * 1.6
    }
    
    # Label sizes: MI-proportional for neighbours, larger for keyword
    label_sizes <- vapply(feats, function(f) {
      if (f == kw) return(input$label_size * 1.6)
      idx <- match(f, ed$to)
      if (is.na(idx) || is.na(ed$MI[idx])) return(input$label_size * 0.6)
      mi_range <- max(ed$MI, na.rm = TRUE) - min(ed$MI, na.rm = TRUE)
      if (mi_range == 0) return(input$label_size)
      input$label_size * (0.5 + 0.5 *
                            (ed$MI[idx] - min(ed$MI, na.rm = TRUE)) / mi_range)
    }, numeric(1))
    
    # Edge weight for textplot_network: use MI scores
    # The sub_fcm contains raw counts; we scale by MI for display
    # by modifying the matrix values
    mi_mat <- as.matrix(sub_fcm)
    for (i in seq_len(nrow(ed))) {
      w1 <- ed$from[i]; w2 <- ed$to[i]
      mi_val <- max(ed$MI[i], 0.01, na.rm = TRUE)
      if (w1 %in% rownames(mi_mat) && w2 %in% colnames(mi_mat))
        mi_mat[w1, w2] <- mi_val
      if (w2 %in% rownames(mi_mat) && w1 %in% colnames(mi_mat))
        mi_mat[w2, w1] <- mi_val
    }
    mi_mat[mi_mat < 0] <- 0
    diag(mi_mat) <- 0
    mi_fcm <- as.fcm(mi_mat)
    
    quanteda.textplots::textplot_network(
      x              = mi_fcm,
      min_freq       = 0,
      edge_alpha     = input$edge_alpha,
      edge_color     = input$edge_color,
      edge_size      = 3,
      vertex_color   = ifelse(feats == kw, LADAL_PURPLE, "#a585c8"),
      vertex_size    = v_sizes,
      vertex_labelsize = label_sizes
    ) +
      labs(
        title    = paste0("Co-occurrence network: '", kw, "'"),
        subtitle = paste0(
          "Window ±", input$window_size,
          " · min freq ", input$min_freq,
          " · min MI ", input$min_mi,
          " · top ", input$top_n, " words",
          if (isTRUE(input$lemmatise)) " · lemmatised" else "",
          " · edge width = MI score · node size = ",
          if (input$node_size_by == "freq") "corpus freq" else "fixed"
        )
      ) +
      theme(
        plot.title    = element_text(color = LADAL_PURPLE,
                                     face  = "bold", size = 14),
        plot.subtitle = element_text(color = "#666", size = 9.5)
      )
  }, bg = "white")
  
  # ── Co-occurrence table ──────────────────────────────────
  output$cooc_table <- renderDT({
    nd <- net_data()
    req(!is.null(nd))
    
    display <- nd$edges |>
      dplyr::select(
        Keyword      = from,
        Collocate    = to,
        `Co-occ (O)` = cooc,
        `MI score`   = MI,
        `Freq (collocate)` = f_w2
      ) |>
      dplyr::arrange(dplyr::desc(`MI score`))
    
    datatable(
      display,
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 25,
        order      = list(list(3, "desc"))
      ),
      caption = htmltools::tags$caption(
        style = paste0("color:", LADAL_PURPLE, "; font-weight:bold;"),
        paste0("Words co-occurring with '", nd$keyword,
               "' · sorted by MI score · window ±",
               input$window_size)
      )
    ) |>
      formatRound("MI score", digits = 3) |>
      formatStyle(
        "MI score",
        background         = styleColorBar(
          range(nd$edges$MI, na.rm = TRUE), "#d8c8f0"),
        backgroundSize     = "98% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      ) |>
      formatStyle(
        "Co-occ (O)",
        background         = styleColorBar(nd$edges$cooc, "#c8e6d8"),
        backgroundSize     = "98% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
  })
  
  # ── Download buttons ─────────────────────────────────────
  output$download_buttons <- renderUI({
    if (input$run_network == 0 || is.null(net_data()))
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Run a network to enable downloads."))
    tagList(
      downloadButton("dl_png",  "⬇ Network (.png)", class = "ww-dl"),
      downloadButton("dl_xlsx", "⬇ Table (.xlsx)",  class = "ww-dl"),
      downloadButton("dl_csv",  "⬇ Table (.csv)",   class = "ww-dl")
    )
  })
  
  # ── Download handlers ────────────────────────────────────
  output$dl_xlsx <- downloadHandler(
    filename = function()
      paste0("wordwebber_", net_data()$keyword, "_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(as.data.frame(net_data()$edges), file)
  )
  
  output$dl_csv <- downloadHandler(
    filename = function()
      paste0("wordwebber_", net_data()$keyword, "_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(net_data()$edges, file)
  )
  
  output$dl_png <- downloadHandler(
    filename = function()
      paste0("wordwebber_", net_data()$keyword, "_", Sys.Date(), ".png"),
    content  = function(file) {
      nd       <- net_data()
      req(!is.null(nd))
      kw       <- nd$keyword
      ed       <- nd$edges
      sub_fcm  <- nd$sub_fcm
      freq_vec <- nd$freq_vec
      feats    <- featnames(sub_fcm)
      
      if (input$node_size_by == "freq") {
        freqs   <- as.numeric(freq_vec[feats])
        freqs[is.na(freqs)] <- 1
        v_sizes <- input$node_size * 0.5 +
          input$node_size * 1.5 * (freqs / max(freqs, 1))
        v_sizes[feats == kw] <- max(v_sizes) * 1.15
      } else {
        v_sizes <- rep(input$node_size, length(feats))
        v_sizes[feats == kw] <- input$node_size * 1.6
      }
      
      label_sizes <- vapply(feats, function(f) {
        if (f == kw) return(input$label_size * 1.6)
        idx <- match(f, ed$to)
        if (is.na(idx) || is.na(ed$MI[idx])) return(input$label_size * 0.6)
        mi_range <- max(ed$MI, na.rm = TRUE) - min(ed$MI, na.rm = TRUE)
        if (mi_range == 0) return(input$label_size)
        input$label_size * (0.5 + 0.5 *
                              (ed$MI[idx] - min(ed$MI, na.rm = TRUE)) / mi_range)
      }, numeric(1))
      
      mi_mat <- as.matrix(sub_fcm)
      for (i in seq_len(nrow(ed))) {
        w1 <- ed$from[i]; w2 <- ed$to[i]
        mi_val <- max(ed$MI[i], 0.01, na.rm = TRUE)
        if (w1 %in% rownames(mi_mat) && w2 %in% colnames(mi_mat))
          mi_mat[w1, w2] <- mi_val
        if (w2 %in% rownames(mi_mat) && w1 %in% colnames(mi_mat))
          mi_mat[w2, w1] <- mi_val
      }
      mi_mat[mi_mat < 0] <- 0
      diag(mi_mat) <- 0
      mi_fcm <- as.fcm(mi_mat)
      
      p <- quanteda.textplots::textplot_network(
        x              = mi_fcm,
        min_freq       = 0,
        edge_alpha     = input$edge_alpha,
        edge_color     = input$edge_color,
        edge_size      = 3,
        vertex_color   = ifelse(feats == kw, LADAL_PURPLE, "#a585c8"),
        vertex_size    = v_sizes,
        vertex_labelsize = label_sizes
      ) +
        labs(
          title    = paste0("Co-occurrence network: '", kw, "'"),
          subtitle = paste0("Window ±", input$window_size,
                            " · min freq ", input$min_freq,
                            " · edge width = MI score")
        ) +
        theme(
          plot.title = element_text(color = LADAL_PURPLE, face = "bold"),
          plot.subtitle = element_text(color = "#666", size = 9)
        )
      
      ggplot2::ggsave(file, plot = p,
                      width = 10, height = 8, dpi = 200, bg = "white")
    }
  )

  # ── Parameters download ─────────────────────────────────────────
  output$dl_params <- downloadHandler(
    filename = function() paste0("wordwebber_params_", Sys.Date(), ".txt"),
    content  = function(file) {
      lines <- c(
        paste0("Tool:                ", "WordWebber — Word Co-occurrence Networks"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("quanteda:            ", as.character(packageVersion('quanteda'))),
        paste0("---                  ", ""),
        paste0("Keyword:             ", input$keyword),
        paste0("Window size:         ", as.character(input$window_size)),
        paste0("Min frequency:       ", as.character(input$min_freq)),
        paste0("Min MI:              ", as.character(input$min_mi)),
        paste0("Top N:               ", as.character(input$top_n)),
        paste0("Stopword lang:       ", input$stopword_lang),
        paste0("Lemmatise:           ", as.character(input$lemmatise)),
        paste0("Files:               ", if (!is.null(input$files)) paste(input$files$name, collapse=", ") else "none")
      )
      writeLines(lines, file)
    }
  )

  output$params_dl_ui <- renderUI({
    downloadButton("dl_params", "⬇ Download parameters (.txt)", class = "ww-dl")
  })

  output$params_preview <- renderText({
    paste(c(
        paste0("Tool:                ", "WordWebber — Word Co-occurrence Networks"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("quanteda:            ", as.character(packageVersion('quanteda'))),
        paste0("---                  ", ""),
        paste0("Keyword:             ", input$keyword),
        paste0("Window size:         ", as.character(input$window_size)),
        paste0("Min frequency:       ", as.character(input$min_freq)),
        paste0("Min MI:              ", as.character(input$min_mi)),
        paste0("Top N:               ", as.character(input$top_n)),
        paste0("Stopword lang:       ", input$stopword_lang),
        paste0("Lemmatise:           ", as.character(input$lemmatise)),
    ), collapse="\n")
  }

}

# ══════════════════════════════════════════════════════════════)

shinyApp(ui, server)
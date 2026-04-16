# ============================================================
#  TopicDetector — LADAL Topic Modelling Tool (Shiny)
#  https://ladal.edu.au
#
#  Two-stage workflow:
#    Stage 1 — Unsupervised LDA (topicmodels::LDA)
#              Explore topics; top terms auto-populate seed dict
#    Stage 2 — Seeded LDA (seededlda::textmodel_seededlda)
#              User-defined seed dictionary; residual=TRUE
#
#  New packages required in tools-env/install.R:
#    install.packages("topicmodels")
#    install.packages("seededlda")
#    install.packages("SnowballC")
#    install.packages("tidytext")
# ============================================================

library(shiny)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(tibble)
library(readr)
library(writexl)
library(DT)
library(tidytext)    # tidy() for LDA beta/gamma
library(topicmodels) # LDA()
library(seededlda)   # textmodel_seededlda()
library(SnowballC)   # wordStem()

quanteda_options(verbose = FALSE)

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

# Colour palette for topics (up to 20)
TOPIC_COLOURS <- c(
  "#51247a","#e07b39","#27ae60","#2980b9","#c0392b",
  "#f39c12","#8e44ad","#16a085","#d35400","#2c3e50",
  "#1abc9c","#e74c3c","#3498db","#2ecc71","#9b59b6",
  "#f1c40f","#e67e22","#95a5a6","#7f8c8d","#34495e"
)

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

# Build a clean, trimmed DFM ready for LDA.
build_clean_dfm <- function(corp, stopword_lang, stem,
                             min_termfreq, min_docfreq) {
  toks <- tokens(corp,
                 remove_punct   = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE,
                 remove_url     = TRUE) |>
    tokens_tolower() |>
    tokens_remove(pattern   = "^[^a-z]+$", valuetype = "regex") |>
    tokens_remove(pattern   = "^.{1}$",    valuetype = "regex")

  if (stopword_lang != "none")
    toks <- tokens_remove(toks, stopwords(stopword_lang), padding = FALSE)

  if (stem)
    toks <- tokens_wordstem(toks, language = "english")

  dfm(toks) |>
    dfm_trim(min_termfreq = min_termfreq,
             min_docfreq  = min_docfreq)
}

# Run unsupervised LDA and return tidy top-terms data frame.
run_unsup_lda <- function(clean_dfm, k, seed, n_terms = 10) {
  # topicmodels requires a standard matrix — convert from quanteda dfm
  dtm <- convert(clean_dfm, to = "topicmodels")
  # Remove empty documents (LDA will fail on them)
  dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
  if (nrow(dtm) == 0) stop("No documents remain after filtering.")

  lda_model <- topicmodels::LDA(dtm, k = k,
                                 control = list(seed = seed))

  # Top terms per topic as a wide data frame
  beta_df <- tidytext::tidy(lda_model, matrix = "beta")

  top_terms <- beta_df |>
    dplyr::group_by(topic) |>
    dplyr::slice_max(beta, n = n_terms, with_ties = FALSE) |>
    dplyr::arrange(topic, dplyr::desc(beta)) |>
    dplyr::ungroup()

  # Wide format: one column per topic
  wide <- top_terms |>
    dplyr::group_by(topic) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(rank, topic, term) |>
    tidyr::pivot_wider(names_from  = topic,
                       values_from = term,
                       names_prefix = "Topic_") |>
    dplyr::select(-rank)

  list(model = lda_model, top_terms = wide, beta = beta_df)
}

# Build quanteda dictionary from named list of seed-word vectors.
build_seed_dict <- function(topic_names, seed_words_list) {
  # seed_words_list: list of character vectors, one per topic
  valid <- vapply(seed_words_list, function(w) length(w) > 0, logical(1))
  if (!any(valid)) stop("At least one topic must have seed words.")
  dict_list <- setNames(seed_words_list[valid], topic_names[valid])
  quanteda::dictionary(dict_list)
}

# Run seeded LDA and return results list.
run_seeded_lda <- function(clean_dfm, dict, min_termfreq,
                            n_terms = 10, seed = 42) {
  set.seed(seed)
  model <- seededlda::textmodel_seededlda(
    clean_dfm,
    dictionary    = dict,
    residual      = TRUE,
    min_termfreq  = min_termfreq,
    max_iter      = 2000
  )

  # Top terms per topic (wide)
  top_mat  <- seededlda::terms(model, n_terms)   # matrix: terms × topics
  top_df   <- as.data.frame(top_mat, stringsAsFactors = FALSE)
  colnames(top_df) <- colnames(top_mat)

  # Document-topic assignments + probabilities.
  # seededlda stores the doc-topic probability matrix as model$theta directly.
  # seededlda::topics() only returns the MAP topic label (no type argument).
  theta    <- model$theta                # doc × topic probability matrix
  best_top <- seededlda::topics(model)  # named character vector of best topics

  # Use names from best_top as the authoritative document IDs
  doc_names <- names(best_top)
  if (is.null(doc_names)) doc_names <- rownames(theta)
  if (is.null(doc_names)) doc_names <- paste0("doc", seq_len(nrow(theta)))

  doc_df <- tibble::tibble(
    Document  = doc_names,
    BestTopic = as.character(best_top)
  )
  prob_df <- as.data.frame(theta, stringsAsFactors = FALSE)
  prob_df$Document <- doc_names
  doc_full <- dplyr::left_join(doc_df, prob_df, by = "Document") |>
    dplyr::relocate(Document, BestTopic)

  list(model    = model,
       top_df   = top_df,
       doc_full = doc_full,
       theta    = theta,
       topics   = best_top)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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
    tags$a("→ Tutorial", href = "https://ladal.edu.au/tutorials/topicmodels/topicmodels.html", target = "_blank",
           style = "font-size:.78rem;color:#51247a;")
  ),
  tags$blockquote(
    style = "border-left:3px solid #c8b8de;padding-left:12px;margin:0 0 10px 0;color:#444;",
    HTML(paste0(
      "Schweinberger, Martin. (2026). ",
      "<em>TopicDetector: A browser-based LDA topic modelling tool</em>. ",
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
        "@misc{schweinberger2026topicdetector,\n",
        "  author       = {Schweinberger, Martin},\n",
        "  title        = {TopicDetector: A browser-based LDA topic modelling tool},\n",
        "  year         = {2026},\n",
        "  organization = {The University of Queensland},\n",
        "  url          = {https://ladal.edu.au/tools.html}\n",
        "}"
      )
    )
  )
)

ui <- fluidPage(
  title = "TopicDetector | LADAL",

  tags$head(tags$style(HTML(paste0("
    body { font-family:'Segoe UI',Arial,sans-serif;
           background:#f7f4fb; color:#222; margin:0; }

    /* Banner */
    .td-banner {
      background:", LADAL_PURPLE, ";
      color:white; padding:18px 32px 14px 32px;
      display:flex; align-items:center; gap:18px;
      border-bottom:4px solid ", LADAL_GOLD, ";
    }
    .td-banner .td-title { font-size:1.7rem; font-weight:700;
                            letter-spacing:.5px; margin:0; }
    .td-banner .td-sub   { font-size:.88rem; opacity:.85; margin:2px 0 0 0; }

    /* Layout */
    .td-body { display:flex; min-height:calc(100vh - 80px); }
    .td-side  { width:330px; min-width:280px; max-width:360px;
                background:white;
                border-right:1px solid #e0d8ec;
                padding:22px 20px 30px 20px;
                box-shadow:2px 0 8px rgba(81,36,122,.06);
                overflow-y:auto; }
    .td-main  { flex:1; padding:24px 28px; overflow-x:auto; }

    /* Section headings */
    .td-sec {
      font-size:.73rem; font-weight:700; letter-spacing:1.2px;
      text-transform:uppercase; color:", LADAL_PURPLE, ";
      border-bottom:2px solid ", LADAL_GOLD, ";
      padding-bottom:4px; margin:20px 0 10px 0;
    }
    .td-sec:first-child { margin-top:0; }

    /* Inputs */
    .form-control, .selectize-input {
      border:1.5px solid #d0c8e0 !important;
      border-radius:6px !important; font-size:.9rem !important;
    }
    label { font-size:.86rem; font-weight:600; color:#444; }
    .form-group { margin-bottom:8px; }

    /* Buttons */
    .td-run-btn {
      width:100%; background:", LADAL_PURPLE, " !important;
      border:none !important; color:white !important;
      font-weight:700; font-size:.97rem; padding:10px;
      border-radius:7px; margin-top:4px; transition:background .2s;
    }
    .td-run-btn:hover { background:#3a1860 !important; }
    .td-sec-btn {
      width:100%; background:#f4f0f8 !important;
      border:1.5px solid ", LADAL_PURPLE, " !important;
      color:", LADAL_PURPLE, " !important;
      font-weight:700; font-size:.9rem; padding:8px;
      border-radius:7px; margin-top:4px; transition:all .15s;
    }
    .td-sec-btn:hover {
      background:", LADAL_PURPLE, " !important; color:white !important;
    }

    /* Info/warn/ok boxes */
    .td-info {
      background:#f4f0f8; border-left:4px solid ", LADAL_PURPLE, ";
      border-radius:5px; padding:9px 13px; font-size:.83rem;
      color:#444; margin-bottom:12px;
    }
    .td-warn {
      background:#fff4e5; border-left:4px solid ", LADAL_GOLD, ";
      border-radius:5px; padding:9px 13px; font-size:.83rem;
      color:#6b4000; margin-bottom:10px;
    }
    .td-ok {
      background:#eafaf1; border-left:4px solid #27ae60;
      border-radius:5px; padding:9px 13px; font-size:.83rem;
      color:#1a6b3c; margin-bottom:8px;
    }

    /* Seed topic cards */
    .td-seed-card {
      background:#faf8fd; border:1.5px solid #e0d8ec;
      border-radius:8px; padding:10px 12px; margin-bottom:8px;
    }
    .td-seed-card:hover { border-color:", LADAL_PURPLE, "; }
    .td-seed-label {
      font-size:.78rem; font-weight:700; color:", LADAL_PURPLE, ";
      margin-bottom:5px; text-transform:uppercase; letter-spacing:.8px;
    }

    /* Download buttons */
    .td-dl {
      display:inline-block; margin:3px 4px 3px 0;
      background:white; border:1.5px solid ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; font-size:.83rem;
      padding:5px 12px; border-radius:6px; cursor:pointer;
      text-decoration:none; transition:all .15s;
    }
    .td-dl:hover { background:", LADAL_PURPLE, "; color:white; }

    /* Stat chips */
    .td-chips { display:flex; gap:10px; flex-wrap:wrap; margin-bottom:18px; }
    .td-chip  {
      background:white; border-radius:20px;
      border:1.5px solid #e0d8ec;
      padding:5px 14px; font-size:.81rem;
      display:flex; align-items:center; gap:5px;
    }
    .td-chip b { color:", LADAL_PURPLE, "; }

    /* Tabs */
    .nav-tabs > li > a { color:", LADAL_PURPLE, "; font-weight:600; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      border-top:3px solid ", LADAL_PURPLE, " !important;
      color:", LADAL_PURPLE, " !important;
    }

    /* DT */
    .dataTables_wrapper { font-size:.85rem; }
    table.dataTable thead th {
      background:", LADAL_PURPLE, " !important;
      color:white !important; font-weight:600;
      border-bottom:2px solid ", LADAL_GOLD, " !important;
    }
    table.dataTable tbody tr:hover { background:#f4f0f8 !important; }

    /* Upload */
    .shiny-input-container .btn {
      background:white !important;
      border:1.5px dashed ", LADAL_PURPLE, " !important;
      color:", LADAL_PURPLE, " !important;
      font-weight:600 !important; width:100% !important;
    }

    /* Footer */
    .td-footer {
      background:#2d1a4a; color:#c8b8de;
      font-size:.77rem; padding:11px 32px;
      display:flex; gap:18px; align-items:center;
    }
    .td-footer a { color:#d4b8f5; }

    /* Stage divider */
    .td-stage {
      background:linear-gradient(135deg, ", LADAL_PURPLE, " 0%, #7a3ca8 100%);
      color:white; padding:8px 14px; border-radius:7px;
      font-size:.82rem; font-weight:700; margin-bottom:14px;
      letter-spacing:.5px;
    }
  ")))),

  # ── Banner ──────────────────────────────────────────────────
  div(class = "td-banner",
    div(style = "font-size:2rem;", "📊"),
    div(
      p(class = "td-title", "TopicDetector"),
      p(class = "td-sub",
        "Unsupervised & seeded LDA topic modelling · ",
        tags$a("LADAL", href = "https://ladal.edu.au",
               style = "color:#f0c060;"))
    )
  ),

  # ── Body ────────────────────────────────────────────────────
  div(class = "td-body",

    # ── Sidebar ───────────────────────────────────────────────
    div(class = "td-side",

      # STEP 1 — Upload
      div(class = "td-sec", "① Upload texts"),
      div(class = "td-info",
        "Upload one or more ", tags$b(".txt"), " files.
         Each file is treated as one document."),
      fileInput("files", NULL,
                multiple    = TRUE,
                accept      = ".txt",
                buttonLabel = "📂 Choose .txt files"),
      uiOutput("upload_status"),

      # STEP 2 — Preprocessing
      div(class = "td-sec", "② Text preprocessing"),
      selectInput("stopword_lang", "Remove stopwords",
                  choices = STOPWORD_LANGS, selected = "en"),
      checkboxInput("stem", "Stem tokens (SnowballC, English)",
                    value = TRUE),
      div(class = "td-info", style = "margin-top:-4px;",
        "Stemming reduces words to their base form
         (", tags$em("running → run"), ", ", tags$em("topics → topic"),
        "). Improves topic coherence."),
      div(class = "td-row" |> paste0(),
        style = "display:flex; gap:8px;",
        div(style = "flex:1;",
          numericInput("min_termfreq", "Min. term freq.",
                       value = 2L, min = 1L, step = 1L)),
        div(style = "flex:1;",
          numericInput("min_docfreq", "Min. doc. freq.",
                       value = 2L, min = 1L, step = 1L))
      ),

      # STEP 3 — Unsupervised LDA
      div(class = "td-sec", "③ Unsupervised LDA"),
      div(class = "td-info",
        "Run LDA with different ", tags$b("k"), " values to explore
         what topics are in your corpus. Top terms will
         auto-populate the seed dictionary below."),
      div(style = "display:flex; gap:8px;",
        div(style = "flex:1;",
          numericInput("k_unsup", "k (topics)", value = 5L,
                       min = 2L, max = 30L, step = 1L)),
        div(style = "flex:1;",
          numericInput("lda_seed", "Random seed", value = 1234L,
                       min = 1L, step = 1L))
      ),
      numericInput("n_top_terms", "Top terms shown per topic",
                   value = 10L, min = 5L, max = 25L, step = 1L),
      actionButton("run_unsup", "🔍  Run unsupervised LDA",
                   class = "td-run-btn btn-primary"),

      # STEP 4 — Seed dictionary
      div(class = "td-sec", "④ Seed dictionary"),
      div(class = "td-info",
        "Topic names and seed words are pre-filled from the
         unsupervised LDA results. Edit them freely before
         running the seeded model."),
      uiOutput("seed_ui"),

      # STEP 5 — Seeded LDA
      div(class = "td-sec", "⑤ Seeded LDA"),
      div(class = "td-info",
        "Runs seededlda with ", tags$code("residual = TRUE"),
        " — an extra ", tags$em("other"), " topic captures
         content that doesn't match any seed topic."),
      actionButton("run_seeded", "📊  Run seeded LDA",
                   class = "td-run-btn btn-primary"),

      # Downloads
      div(class = "td-sec", "⑥ Download"),
      uiOutput("download_buttons")
    ),

    # ── Main panel ────────────────────────────────────────────
    div(class = "td-main",
      uiOutput("welcome_box"),
      uiOutput("main_results")
    )
  ),

  # ── Footer ────────────────────────────────────────────────
  div(class = "td-footer",
    span("TopicDetector · LADAL · University of Queensland"),
    tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
    tags$a("Topic Modelling Tutorial",
           href = "https://ladal.edu.au/tutorials/topicmodels/topicmodels.html"),
    tags$a("Cite this tool",
           href = "https://ladal.edu.au/about.html#citing")
  ),
  CITATION_FOOTER
)

# ══════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Corpus ────────────────────────────────────────────────
  corp <- reactive({
    req(input$files)
    load_corpus(input$files)
  })

  # ── Upload status ─────────────────────────────────────────
  output$upload_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "td-warn", "⚠ No files uploaded yet.")
    } else {
      n <- nrow(input$files)
      div(class = "td-ok",
          paste0("✔ ", n, " file", if (n != 1) "s", " loaded"))
    }
  })

  # ── Welcome box ───────────────────────────────────────────
  output$welcome_box <- renderUI({
    if (input$run_unsup == 0 && input$run_seeded == 0) {
      div(class = "td-info", style = "font-size:.93rem; max-width:700px;",
        tags$b("Welcome to TopicDetector."), br(), br(),
        tags$b("Stage 1 — Explore (unsupervised LDA):"), br(),
        "Upload your texts, set preprocessing options, choose a number
         of topics (k), and click ", tags$b("Run unsupervised LDA"), ".
         Examine the top terms per topic and adjust k until the topics
         look coherent.", br(), br(),
        tags$b("Stage 2 — Refine (seeded LDA):"), br(),
        "The seed dictionary below the unsupervised results is
         pre-filled with the top terms from each topic. Edit the
         topic names and seed words to reflect your interpretation,
         then click ", tags$b("Run seeded LDA"), " to assign every
         document to a topic.", br(), br(),
        tags$a("→ Topic Modelling Tutorial",
               href = "https://ladal.edu.au/tutorials/topicmodels/topicmodels.html")
      )
    }
  })

  # ── DFM (built fresh on each LDA run — inside eventReactive) ─
  # Not a standalone reactive to avoid rebuilding on every input change.

  # ── Unsupervised LDA ──────────────────────────────────────
  unsup_result <- eventReactive(input$run_unsup, {
    req(corp())
    withProgress(message = "Running unsupervised LDA…", value = 0, {
      incProgress(0.2, detail = "Building DFM")
      dfm_clean <- tryCatch(
        build_clean_dfm(corp(),
                        stopword_lang = input$stopword_lang,
                        stem          = isTRUE(input$stem),
                        min_termfreq  = as.integer(input$min_termfreq %||% 2L),
                        min_docfreq   = as.integer(input$min_docfreq  %||% 2L)),
        error = function(e) {
          showNotification(paste("DFM error:", e$message),
                           type = "error", duration = 12)
          NULL
        }
      )
      req(!is.null(dfm_clean))

      incProgress(0.5, detail = paste("Fitting LDA with k =", input$k_unsup))
      result <- tryCatch(
        run_unsup_lda(dfm_clean,
                      k      = as.integer(input$k_unsup),
                      seed   = as.integer(input$lda_seed %||% 1234L),
                      n_terms = as.integer(input$n_top_terms %||% 10L)),
        error = function(e) {
          showNotification(paste("LDA error:", e$message),
                           type = "error", duration = 12)
          NULL
        }
      )
      incProgress(0.3, detail = "Done")
      if (is.null(result)) return(NULL)
      result$dfm_clean <- dfm_clean
      result
    })
  })

  # ── Seed dictionary UI ────────────────────────────────────
  # Rendered after unsupervised LDA; pre-fills from top terms.
  output$seed_ui <- renderUI({
    # Show placeholder if unsupervised hasn't run yet
    if (input$run_unsup == 0 || is.null(unsup_result())) {
      return(div(class = "td-info",
        "Run the unsupervised LDA above first — the seed
         dictionary will be pre-filled automatically."))
    }

    top <- unsup_result()$top_terms   # wide df: one col per topic
    k   <- ncol(top)
    topic_cols <- colnames(top)       # e.g. "Topic_1", "Topic_2", …

    cards <- lapply(seq_len(k), function(i) {
      col_name  <- topic_cols[i]
      # Default seed words: top terms from this unsupervised topic
      def_words <- paste(top[[col_name]], collapse = ", ")
      # Default topic name
      def_name  <- paste0("Topic", formatC(i, width = 2, flag = "0"))

      div(class = "td-seed-card",
        div(class = "td-seed-label", paste("Topic", i)),
        div(style = "display:flex; gap:8px;",
          div(style = "flex:0 0 110px;",
            textInput(paste0("topic_name_", i),
                      label = "Name",
                      value = def_name,
                      placeholder = "Topic name")
          ),
          div(style = "flex:1;",
            textInput(paste0("seed_words_", i),
                      label = "Seed words (comma-separated)",
                      value = def_words,
                      placeholder = "word1, word2, word3 …")
          )
        )
      )
    })

    tagList(cards)
  })

  # ── Seeded LDA ────────────────────────────────────────────
  seeded_result <- eventReactive(input$run_seeded, {
    req(unsup_result())

    # Collect seed dictionary from dynamic inputs
    k <- ncol(unsup_result()$top_terms)

    topic_names <- vapply(seq_len(k), function(i) {
      trimws(input[[paste0("topic_name_", i)]] %||%
             paste0("Topic", formatC(i, width = 2, flag = "0")))
    }, character(1))

    seed_words_list <- lapply(seq_len(k), function(i) {
      raw <- input[[paste0("seed_words_", i)]] %||% ""
      words <- strsplit(raw, ",")[[1]]
      trimws(words[nchar(trimws(words)) > 0])
    })

    # Validate
    has_words <- vapply(seed_words_list, length, integer(1)) > 0
    if (!any(has_words)) {
      showNotification("Please add seed words to at least one topic.",
                       type = "warning", duration = 8)
      return(NULL)
    }

    withProgress(message = "Running seeded LDA…", value = 0, {
      incProgress(0.2, detail = "Building seed dictionary")

      dict <- tryCatch(
        build_seed_dict(topic_names, seed_words_list),
        error = function(e) {
          showNotification(paste("Dictionary error:", e$message),
                           type = "error", duration = 10)
          NULL
        }
      )
      req(!is.null(dict))

      incProgress(0.5, detail = "Fitting seeded LDA")
      result <- tryCatch(
        run_seeded_lda(
          unsup_result()$dfm_clean,
          dict         = dict,
          min_termfreq = as.integer(input$min_termfreq %||% 2L),
          n_terms      = as.integer(input$n_top_terms %||% 10L),
          seed         = as.integer(input$lda_seed %||% 1234L)
        ),
        error = function(e) {
          showNotification(paste("Seeded LDA error:", e$message),
                           type = "error", duration = 12)
          NULL
        }
      )
      incProgress(0.3, detail = "Done")
      result
    })
  })

  # ── Main results area ─────────────────────────────────────
  output$main_results <- renderUI({
    # Neither has run yet — welcome box handles it
    if (input$run_unsup == 0 && input$run_seeded == 0) return(NULL)

    panels <- list()

    # ── Stage 1 panel ──────────────────────────────────────
    if (input$run_unsup > 0) {
      ur <- unsup_result()
      stage1_content <- if (is.null(ur)) {
        div(class = "td-warn",
          "⚠ Unsupervised LDA failed. Check that your files
           have enough tokens and try adjusting the minimum
           term/document frequency.")
      } else {
        k   <- ncol(ur$top_terms)
        ndc <- nrow(ur$top_terms)
        tagList(
          div(class = "td-stage",
              paste0("STAGE 1 — Unsupervised LDA · k = ", k,
                     " topics · seed = ", input$lda_seed)),
          div(class = "td-chips",
            div(class = "td-chip",
              tags$b(nrow(corp())), " documents"),
            div(class = "td-chip",
              tags$b(k), " topics"),
            div(class = "td-chip",
              tags$b(ndoc(unsup_result()$dfm_clean)),
              " docs in model"),
            div(class = "td-chip",
              tags$b(nfeat(unsup_result()$dfm_clean)),
              " features")
          ),
          DTOutput("unsup_terms_dt"),
          br(),
          div(class = "td-info",
            "👆 These top terms have been used to pre-fill the seed
             dictionary in the ", tags$b("sidebar (left panel)"), ". ",
            "Scroll up in the sidebar to find the ",
            tags$b("② Seed dictionary"), " section — edit the topic
             names and seed words there, then click ",
            tags$b("▶ Run seeded LDA"), " to proceed to Stage 2."
          )
        )
      }
      panels <- c(panels, list(stage1_content))
    }

    # ── Stage 2 panel ──────────────────────────────────────
    if (input$run_seeded > 0) {
      sr <- seeded_result()
      stage2_content <- if (is.null(sr)) {
        div(class = "td-warn",
          "⚠ Seeded LDA failed. Check the seed dictionary
           and ensure seed words appear in the corpus.")
      } else {
        topic_names  <- names(sr$topics)[1] |> (\(x) NULL)()
        all_topics   <- sort(unique(as.character(sr$topics)))
        n_topics     <- length(all_topics)
        n_docs       <- nrow(sr$doc_full)

        tagList(
          tags$hr(style = "border-color:#e0d8ec; margin:20px 0;"),
          div(class = "td-stage",
              paste0("STAGE 2 — Seeded LDA · ",
                     n_topics, " topics (incl. residual) · ",
                     n_docs, " documents")),
          div(class = "td-chips",
            div(class = "td-chip",
              tags$b(n_docs), " documents"),
            div(class = "td-chip",
              tags$b(n_topics), " topics"),
            div(class = "td-chip",
              tags$b(sum(as.character(sr$topics) != "other")),
              " assigned to seed topics")
          ),
          tabsetPanel(
            tabPanel("📋 Top terms",
              br(),
              div(class = "td-info",
                "Top terms for each seeded topic. The ",
                tags$em("other"), " column shows the residual topic."),
              DTOutput("seeded_terms_dt")
            ),
            tabPanel("📄 Document assignments",
              br(),
              div(class = "td-info",
                "One row per document. ",
                tags$b("BestTopic"), " is the dominant topic;
                 remaining columns show the probability of each topic."),
              DTOutput("doc_assign_dt")
            ),
            tabPanel("📊 Topic frequency",
              br(),
              div(class = "td-info",
                "How many documents (and what percentage) are
                 assigned to each topic."),
              plotOutput("topic_freq_plot", height = "420px"),
              br(),
              uiOutput("freq_plot_dl")
            ),
            tabPanel("🌡️ Topic heatmap",
              br(),
              div(class = "td-info",
                "Topic probabilities across all documents. Darker
                 purple = higher probability. Documents are rows;
                 topics are columns."),
              plotOutput("topic_heatmap",
                         height = paste0(
                           max(300, 40 + n_docs * 14), "px")),
              br(),
              uiOutput("heatmap_dl")
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
      }
      panels <- c(panels, list(stage2_content))
    }

    tagList(panels)
  })

  # ── Unsupervised top-terms DT ──────────────────────────
  output$unsup_terms_dt <- renderDT({
    ur <- unsup_result()
    req(!is.null(ur))
    datatable(
      ur$top_terms,
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 10,
        scrollX    = TRUE
      ),
      caption = htmltools::tags$caption(
        style = paste0("color:", LADAL_PURPLE, "; font-weight:bold;"),
        paste0("Top ", nrow(ur$top_terms),
               " terms per topic — unsupervised LDA (k = ",
               input$k_unsup, ")")
      )
    ) |>
    formatStyle(colnames(ur$top_terms),
                fontFamily = "monospace", fontSize = ".87rem")
  })

  # ── Seeded top-terms DT ────────────────────────────────
  output$seeded_terms_dt <- renderDT({
    sr <- seeded_result()
    req(!is.null(sr))
    datatable(
      sr$top_df,
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 10,
        scrollX    = TRUE
      )
    ) |>
    formatStyle(colnames(sr$top_df),
                fontFamily = "monospace", fontSize = ".87rem")
  })

  # ── Document assignments DT ────────────────────────────
  output$doc_assign_dt <- renderDT({
    sr <- seeded_result()
    req(!is.null(sr))

    display <- sr$doc_full |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

    datatable(
      display,
      rownames   = FALSE,
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 25,
        scrollX    = TRUE
      )
    ) |>
    formatStyle("BestTopic",
                color      = LADAL_PURPLE,
                fontWeight = "bold") |>
    formatStyle(
      names(display)[names(display) != "Document" &
                     names(display) != "BestTopic"],
      background         = styleColorBar(c(0, 1), "#d8c8f0"),
      backgroundSize     = "95% 65%",
      backgroundRepeat   = "no-repeat",
      backgroundPosition = "center"
    )
  })

  # ── Topic frequency bar chart ──────────────────────────
  build_freq_plot <- reactive({
    sr <- seeded_result()
    req(!is.null(sr))

    freq_df <- as.data.frame(table(BestTopic = sr$topics),
                              stringsAsFactors = FALSE) |>
      dplyr::rename(n = Freq) |>
      dplyr::mutate(
        pct    = round(n / sum(n) * 100, 1),
        label  = paste0(n, "\n(", pct, "%)"),
        fill   = TOPIC_COLOURS[seq_along(BestTopic) %% length(TOPIC_COLOURS) + 1]
      ) |>
      dplyr::arrange(dplyr::desc(n))

    ggplot(freq_df,
           aes(x = reorder(BestTopic, n), y = n, fill = BestTopic)) +
      geom_col(width = 0.7, colour = "white", linewidth = .3,
               show.legend = FALSE) +
      geom_text(aes(label = label),
                hjust = -0.08, size = 3.3, colour = "gray30",
                lineheight = .9) +
      coord_flip() +
      scale_fill_manual(values = setNames(freq_df$fill, freq_df$BestTopic)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
      labs(
        title    = "Topic frequency across documents",
        subtitle = "Number and percentage of documents assigned to each topic",
        x        = NULL,
        y        = "Documents (n)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(colour = LADAL_PURPLE,
                                     face = "bold", size = 13),
        plot.subtitle = element_text(colour = "#777", size = 9.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank()
      )
  })

  output$topic_freq_plot <- renderPlot({
    build_freq_plot()
  }, bg = "white")

  output$freq_plot_dl <- renderUI({
    req(!is.null(seeded_result()))
    tagList(
      downloadButton("dl_freq_png", "⬇ PNG", class = "td-dl"),
      downloadButton("dl_freq_pdf", "⬇ PDF", class = "td-dl")
    )
  })

  output$dl_freq_png <- downloadHandler(
    filename = function() paste0("topicdetector_freq_", Sys.Date(), ".png"),
    content  = function(file)
      ggplot2::ggsave(file, plot = build_freq_plot(),
                      device = "png", width = 8, height = 5,
                      dpi = 180, bg = "white")
  )
  output$dl_freq_pdf <- downloadHandler(
    filename = function() paste0("topicdetector_freq_", Sys.Date(), ".pdf"),
    content  = function(file)
      ggplot2::ggsave(file, plot = build_freq_plot(),
                      device = "pdf", width = 8, height = 5, bg = "white")
  )

  # ── Topic heatmap ─────────────────────────────────────
  build_heatmap <- reactive({
    sr <- seeded_result()
    req(!is.null(sr))

    # Long format from theta matrix
    heat_df <- as.data.frame(sr$theta, stringsAsFactors = FALSE) |>
      tibble::rownames_to_column("Document") |>
      tidyr::pivot_longer(-Document,
                          names_to  = "Topic",
                          values_to = "Probability")

    # Order documents by dominant topic for readability
    doc_order <- sr$doc_full |>
      dplyr::arrange(BestTopic, dplyr::desc(
        dplyr::across(dplyr::everything(), ~ .x, .names = NULL)
      )) |>
      dplyr::pull(Document)

    heat_df$Document <- factor(heat_df$Document,
                                levels = rev(doc_order))

    ggplot(heat_df,
           aes(x = Topic, y = Document, fill = Probability)) +
      geom_tile(colour = "white", linewidth = .3) +
      scale_fill_gradient(low = "#f4f0f8", high = LADAL_PURPLE,
                          limits = c(0, 1),
                          name   = "Probability") +
      labs(
        title    = "Topic probability heatmap",
        subtitle = "Documents ordered by dominant topic",
        x        = NULL,
        y        = NULL
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title    = element_text(colour = LADAL_PURPLE,
                                     face = "bold", size = 12),
        plot.subtitle = element_text(colour = "#777", size = 9),
        axis.text.x   = element_text(angle = 35, hjust = 1, size = 9),
        axis.text.y   = element_text(size  = max(6, 10 - nrow(sr$doc_full) %/% 20)),
        legend.position = "right"
      )
  })

  output$topic_heatmap <- renderPlot({
    build_heatmap()
  }, bg = "white")

  output$heatmap_dl <- renderUI({
    req(!is.null(seeded_result()))
    tagList(
      downloadButton("dl_heat_png", "⬇ PNG", class = "td-dl"),
      downloadButton("dl_heat_pdf", "⬇ PDF", class = "td-dl")
    )
  })

  heatmap_dims <- reactive({
    sr <- seeded_result()
    req(!is.null(sr))
    list(h = max(4, 1 + nrow(sr$doc_full) * 0.25), w = 8)
  })

  output$dl_heat_png <- downloadHandler(
    filename = function() paste0("topicdetector_heatmap_", Sys.Date(), ".png"),
    content  = function(file) {
      d <- heatmap_dims()
      ggplot2::ggsave(file, plot = build_heatmap(),
                      device = "png", width = d$w, height = d$h,
                      dpi = 180, bg = "white")
    }
  )
  output$dl_heat_pdf <- downloadHandler(
    filename = function() paste0("topicdetector_heatmap_", Sys.Date(), ".pdf"),
    content  = function(file) {
      d <- heatmap_dims()
      ggplot2::ggsave(file, plot = build_heatmap(),
                      device = "pdf", width = d$w, height = d$h,
                      bg = "white")
    }
  )

  # ── Downloads ────────────────────────────────────────────
  output$download_buttons <- renderUI({
    if (input$run_seeded == 0 || is.null(seeded_result()))
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Run seeded LDA to enable downloads."))
    tagList(
      tags$p(style = "font-size:.82rem; font-weight:600; color:#555;
                       margin-bottom:4px;",
             "Top terms:"),
      downloadButton("dl_terms_xlsx", "⬇ Excel", class = "td-dl"),
      downloadButton("dl_terms_csv",  "⬇ CSV",   class = "td-dl"),
      tags$p(style = "font-size:.82rem; font-weight:600; color:#555;
                       margin:10px 0 4px;",
             "Document assignments:"),
      downloadButton("dl_docs_xlsx", "⬇ Excel", class = "td-dl"),
      downloadButton("dl_docs_csv",  "⬇ CSV",   class = "td-dl")
    )
  })

  output$dl_terms_xlsx <- downloadHandler(
    filename = function() paste0("topicdetector_terms_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(seeded_result()$top_df, file)
  )
  output$dl_terms_csv <- downloadHandler(
    filename = function() paste0("topicdetector_terms_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(seeded_result()$top_df, file)
  )
  output$dl_docs_xlsx <- downloadHandler(
    filename = function() paste0("topicdetector_docs_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(as.data.frame(seeded_result()$doc_full), file)
  )
  output$dl_docs_csv <- downloadHandler(
    filename = function() paste0("topicdetector_docs_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(seeded_result()$doc_full, file)
  )

  # ── Parameters download ─────────────────────────────────────────
  output$dl_params <- downloadHandler(
    filename = function() paste0("topicdetector_params_", Sys.Date(), ".txt"),
    content  = function(file) {
      lines <- c(
        paste0("Tool:                ", "TopicDetector — LDA Topic Modelling"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("topicmodels:         ", as.character(packageVersion('topicmodels'))),
        paste0("seededlda:           ", as.character(packageVersion('seededlda'))),
        paste0("---                  ", ""),
        paste0("Number of topics:    ", as.character(input$k_unsup)),
        paste0("Top terms shown:     ", as.character(input$n_top_terms)),
        paste0("Min term freq:       ", as.character(input$min_termfreq)),
        paste0("Min doc freq:        ", as.character(input$min_docfreq)),
        paste0("Stopword lang:       ", input$stopword_lang),
        paste0("Stem words:          ", as.character(isTRUE(input$stem))),
        paste0("Random seed:         ", as.character(input$lda_seed)),
        paste0("Files:               ", if (!is.null(input$files)) paste(input$files$name, collapse=", ") else "none")
      )
      writeLines(lines, file)
    }
  )

  output$params_dl_ui <- renderUI({
    downloadButton("dl_params", "⬇ Download parameters (.txt)", class = "td-dl")
  })

  output$params_preview <- renderText({
    paste(c(
        paste0("Tool:                ", "TopicDetector — LDA Topic Modelling"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("topicmodels:         ", as.character(packageVersion('topicmodels'))),
        paste0("seededlda:           ", as.character(packageVersion('seededlda'))),
        paste0("---                  ", ""),
        paste0("Number of topics:    ", as.character(input$k_unsup)),
        paste0("Top terms shown:     ", as.character(input$n_top_terms)),
        paste0("Min term freq:       ", as.character(input$min_termfreq)),
        paste0("Min doc freq:        ", as.character(input$min_docfreq)),
        paste0("Stopword lang:       ", input$stopword_lang),
        paste0("Stem words:          ", as.character(isTRUE(input$stem))),
        paste0("Random seed:         ", as.character(input$lda_seed))
    ), collapse="\n")
  })

}

# ══════════════════════════════════════════════════════════════

shinyApp(ui, server)

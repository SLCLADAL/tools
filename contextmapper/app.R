# ============================================================
#  NetworkMapper — LADAL Word Co-occurrence Network Tool
#  https://ladal.edu.au
#
#  Pipeline:
#    .txt files → corpus → tokens (optional stopword removal)
#    → DFM → FCM (sliding window) → filter by keyword
#    → static textplot_network + interactive visNetwork
# ============================================================

library(shiny)
library(quanteda)
library(quanteda.textplots)
library(tidyverse)
library(writexl)
library(DT)
library(igraph)
library(visNetwork)
library(ggplot2)

quanteda_options(verbose = FALSE)

# ══════════════════════════════════════════════════════════════
#  CONSTANTS
# ══════════════════════════════════════════════════════════════

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

# Available stopword languages from quanteda::stopwords()
STOPWORD_LANGS <- c(
  "None (keep all words)"       = "none",
  "English"                     = "en",
  "German"                      = "de",
  "French"                      = "fr",
  "Spanish"                     = "es",
  "Italian"                     = "it",
  "Dutch"                       = "nl",
  "Portuguese"                  = "pt",
  "Russian"                     = "ru",
  "Arabic"                      = "ar"
)

# ══════════════════════════════════════════════════════════════
#  HELPERS
# ══════════════════════════════════════════════════════════════

load_corpus <- function(file_df) {
  texts <- vapply(file_df$datapath, function(p)
    paste(readLines(p, warn = FALSE), collapse = " "),
    character(1))
  names(texts) <- tools::file_path_sans_ext(file_df$name)
  corpus(texts)
}

build_fcm <- function(corp, window_size, stopword_lang,
                      remove_punct = TRUE, remove_numbers = TRUE) {
  toks <- tokens(corp,
                 remove_punct   = remove_punct,
                 remove_symbols = TRUE,
                 remove_numbers = remove_numbers) |>
    tokens_tolower()

  if (stopword_lang != "none") {
    toks <- tokens_remove(toks,
                          pattern = stopwords(stopword_lang),
                          padding = FALSE)
  }

  fcm(toks,
      context    = "window",
      window     = window_size,
      ordered    = FALSE,
      tri        = FALSE)
}

# Extract co-occurrence data centred on a keyword
keyword_cooc <- function(fcm_obj, keyword, top_n, min_freq) {
  kw <- tolower(trimws(keyword))

  # Check keyword is in FCM
  all_feats <- featnames(fcm_obj)
  if (!kw %in% all_feats) return(NULL)

  # Get co-occurrence vector for the keyword
  mat <- as.matrix(fcm_obj)

  # symmetrise (FCM upper-tri only when tri=FALSE but let's be safe)
  mat <- mat + t(mat)
  diag(mat) <- 0

  if (!kw %in% rownames(mat)) return(NULL)

  cooc_vec <- mat[kw, ]
  cooc_vec <- cooc_vec[cooc_vec >= min_freq]
  cooc_vec <- cooc_vec[names(cooc_vec) != kw]

  if (length(cooc_vec) == 0) return(NULL)

  # Top N by frequency
  cooc_vec <- sort(cooc_vec, decreasing = TRUE)
  cooc_vec <- head(cooc_vec, top_n)

  tibble::tibble(
    from  = kw,
    to    = names(cooc_vec),
    cooc  = as.integer(cooc_vec)
  )
}

# Build sub-FCM for just the keyword neighbourhood
neighbourhood_fcm <- function(fcm_obj, keyword, top_n, min_freq) {
  ed <- keyword_cooc(fcm_obj, keyword, top_n, min_freq)
  if (is.null(ed)) return(NULL)

  # Keep only keyword + its neighbours in FCM
  nodes_keep <- unique(c(ed$from, ed$to))
  mat        <- as.matrix(fcm_obj)
  mat        <- mat + t(mat); diag(mat) <- 0
  keep_idx   <- intersect(nodes_keep, rownames(mat))
  sub_mat    <- mat[keep_idx, keep_idx, drop = FALSE]

  as.fcm(sub_mat)
}

# ══════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════

ui <- fluidPage(
  title = "NetworkMapper | LADAL",

  tags$head(tags$style(HTML(paste0("
    body { font-family:'Segoe UI',Arial,sans-serif;
           background:#f7f4fb; color:#222; margin:0; }

    /* Banner */
    .nm-banner {
      background:", LADAL_PURPLE, ";
      color:white; padding:18px 32px 14px 32px;
      display:flex; align-items:center; gap:18px;
      border-bottom:4px solid ", LADAL_GOLD, ";
    }
    .nm-banner .nm-title { font-size:1.7rem; font-weight:700;
                            letter-spacing:.5px; margin:0; }
    .nm-banner .nm-sub   { font-size:.88rem; opacity:.85; margin:2px 0 0 0; }

    /* Layout */
    .nm-body { display:flex; min-height:calc(100vh - 80px); }
    .nm-side  { width:310px; min-width:260px; max-width:340px;
                background:white;
                border-right:1px solid #e0d8ec;
                padding:22px 20px 30px 20px;
                box-shadow:2px 0 8px rgba(81,36,122,.06); }
    .nm-main  { flex:1; padding:24px 28px; overflow-x:auto; }

    /* Sidebar section headings */
    .nm-sec {
      font-size:.75rem; font-weight:700; letter-spacing:1.2px;
      text-transform:uppercase; color:", LADAL_PURPLE, ";
      border-bottom:2px solid ", LADAL_GOLD, ";
      padding-bottom:4px; margin:20px 0 10px 0;
    }
    .nm-sec:first-child { margin-top:0; }

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
    .nm-info {
      background:#f4f0f8; border-left:4px solid ", LADAL_PURPLE, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#444; margin-bottom:12px;
    }
    .nm-warn {
      background:#fff4e5; border-left:4px solid ", LADAL_GOLD, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#6b4000; margin-bottom:10px;
    }
    .nm-ok {
      background:#eafaf1; border-left:4px solid #27ae60;
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#1a6b3c; margin-bottom:8px;
    }

    /* Stat cards */
    .nm-stats { display:flex; gap:12px; margin-bottom:20px;
                flex-wrap:wrap; }
    .nm-card  { background:white; border-radius:9px;
                border-left:4px solid ", LADAL_PURPLE, ";
                padding:11px 16px; min-width:110px;
                box-shadow:0 1px 6px rgba(81,36,122,.08); }
    .nm-card .nm-val { font-size:1.5rem; font-weight:700;
                       color:", LADAL_PURPLE, "; line-height:1.1; }
    .nm-card .nm-lbl { font-size:.76rem; color:#888; margin-top:2px; }

    /* Download buttons */
    .nm-dl {
      display:inline-block; margin:4px 5px 4px 0;
      background:white; border:1.5px solid ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; font-size:.84rem;
      padding:5px 12px; border-radius:6px; cursor:pointer;
      text-decoration:none; transition:all .15s;
    }
    .nm-dl:hover { background:", LADAL_PURPLE, "; color:white; }

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
    .nm-footer {
      background:#2d1a4a; color:#c8b8de;
      font-size:.78rem; padding:12px 32px;
      display:flex; gap:18px; align-items:center;
    }
    .nm-footer a { color:#d4b8f5; }

    /* visNetwork container */
    .vis-container { border:1px solid #e0d8ec; border-radius:8px;
                     background:white; }
  ")))),

  # ── Banner ────────────────────────────────────────────────────
  div(class = "nm-banner",
    div(style = "font-size:2rem;", "🕸️"),
    div(
      p(class = "nm-title", "NetworkMapper"),
      p(class = "nm-sub",
        "Word co-occurrence network analysis · ",
        tags$a("LADAL", href = "https://ladal.edu.au",
               style = "color:#f0c060;"))
    )
  ),

  # ── Body ──────────────────────────────────────────────────────
  div(class = "nm-body",

    # ── Sidebar ─────────────────────────────────────────────────
    div(class = "nm-side",

      # STEP 1 — Upload
      div(class = "nm-sec", "① Upload texts"),
      div(class = "nm-info",
        "Upload one or more ", tags$b(".txt"), " files.
         All files are merged into one corpus."),
      fileInput("files", NULL,
                multiple    = TRUE,
                accept      = ".txt",
                buttonLabel = "📂 Choose .txt files"),
      uiOutput("corpus_status"),

      # STEP 2 — Keyword
      div(class = "nm-sec", "② Keyword"),
      div(class = "nm-info",
        "The network will show the words that co-occur most with
         this keyword within the context window."),
      textInput("keyword", "Keyword", value = "",
                placeholder = "e.g. climate, justice, …"),

      # STEP 3 — Settings
      div(class = "nm-sec", "③ Settings"),

      selectInput("stopword_lang", "Remove stopwords",
                  choices  = STOPWORD_LANGS,
                  selected = "en"),

      sliderInput("window_size", "Context window (words each side)",
                  min = 1, max = 15, value = 5, step = 1),

      sliderInput("top_n", "Max co-occurring words shown",
                  min = 5, max = 50, value = 20, step = 1),

      numericInput("min_freq", "Min. co-occurrence frequency",
                   value = 2, min = 1, step = 1),

      # Visual options
      div(class = "nm-sec", "④ Visual options"),

      selectInput("edge_color", "Edge colour",
        choices  = c("Gray"   = "gray60",
                     "Purple" = "#8e44ad",
                     "Blue"   = "steelblue",
                     "Green"  = "#27ae60",
                     "Black"  = "gray10"),
        selected = "gray60"),

      sliderInput("edge_alpha", "Edge transparency",
                  min = 0.1, max = 1.0, value = 0.5, step = 0.05),

      sliderInput("node_size", "Node size",
                  min = 0.5, max = 5, value = 2, step = 0.5),

      sliderInput("label_size", "Label size",
                  min = 2, max = 10, value = 5, step = 0.5),

      actionButton("run_network", "🕸️  Build Network",
                   class = "btn-primary"),

      # STEP 5 — Download
      div(class = "nm-sec", "⑤ Download"),
      uiOutput("download_buttons")
    ),

    # ── Main panel ───────────────────────────────────────────────
    div(class = "nm-main",
      uiOutput("welcome_box"),
      uiOutput("stats_cards"),
      uiOutput("results_ui")
    )
  ),

  # ── Footer ───────────────────────────────────────────────────
  div(class = "nm-footer",
    span("NetworkMapper · LADAL · University of Queensland"),
    tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
    tags$a("Network Analysis Tutorial",
           href = "https://ladal.edu.au/tutorials/net/net.html"),
    tags$a("Cite this tool",
           href = "https://ladal.edu.au/about.html#citing")
  )
)

# ══════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Corpus ──────────────────────────────────────────────────
  corp <- reactive({
    req(input$files)
    load_corpus(input$files)
  })

  # ── FCM (re-built when corpus or window or stopwords change) ─
  fcm_obj <- reactive({
    req(corp())
    build_fcm(corp(),
              window_size   = input$window_size,
              stopword_lang = input$stopword_lang)
  })

  # ── Core computation (triggered by button) ──────────────────
  net_data <- eventReactive(input$run_network, {
    req(fcm_obj(), nchar(trimws(input$keyword)) > 0)

    kw <- tolower(trimws(input$keyword))

    withProgress(message = "Building network…", value = 0, {
      incProgress(0.4, detail = "Computing co-occurrences")

      ed <- keyword_cooc(fcm_obj(), kw,
                          top_n    = input$top_n,
                          min_freq = input$min_freq)
      incProgress(0.4, detail = "Preparing graph")

      if (is.null(ed) || nrow(ed) == 0) return(NULL)

      sub_fcm <- neighbourhood_fcm(fcm_obj(), kw,
                                    top_n    = input$top_n,
                                    min_freq = input$min_freq)
      incProgress(0.2, detail = "Done")

      list(edges = ed, sub_fcm = sub_fcm, keyword = kw)
    })
  })

  # ── Corpus status ────────────────────────────────────────────
  output$corpus_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "nm-warn", "⚠ No files uploaded yet.")
    } else {
      n   <- nrow(input$files)
      lbl <- if (n == 1) "1 file loaded" else paste(n, "files loaded")
      div(class = "nm-ok",
          paste0("✔ ", lbl, ": ",
                 paste(tools::file_path_sans_ext(input$files$name),
                       collapse = ", ")))
    }
  })

  # ── Welcome box ──────────────────────────────────────────────
  output$welcome_box <- renderUI({
    if (input$run_network == 0) {
      div(class = "nm-info", style = "font-size:.93rem;",
        tags$b("Welcome to NetworkMapper."), br(),
        "Upload your plain-text files, enter a keyword, and click ",
        tags$b("Build Network"), " to visualise which words
         co-occur most frequently with that keyword across your texts.", br(), br(),
        "The network is built from a feature co-occurrence matrix (FCM)
         using a sliding context window. Nodes are words; edges connect
         words that appear within the window of each other, with edge
         thickness proportional to co-occurrence frequency.", br(), br(),
        tags$a("→ Learn more about network analysis",
               href = "https://ladal.edu.au/tutorials/net/net.html")
      )
    }
  })

  # ── Stat cards ───────────────────────────────────────────────
  output$stats_cards <- renderUI({
    req(input$run_network > 0)
    nd <- net_data()
    if (is.null(nd)) return(NULL)

    div(class = "nm-stats",
      div(class = "nm-card",
        div(class = "nm-val", nrow(nd$edges)),
        div(class = "nm-lbl", paste0("Co-occurring words"))),
      div(class = "nm-card",
        div(class = "nm-val", sum(nd$edges$cooc)),
        div(class = "nm-lbl", "Total co-occurrences")),
      div(class = "nm-card",
        div(class = "nm-val",
            nd$edges$cooc[1]),
        div(class = "nm-lbl", paste0("Top: '", nd$edges$to[1], "'"))),
      div(class = "nm-card",
        div(class = "nm-val",
            format(ntoken(corp()), big.mark = ",")),
        div(class = "nm-lbl", "Corpus tokens"))
    )
  })

  # ── Results UI ───────────────────────────────────────────────
  output$results_ui <- renderUI({
    req(input$run_network > 0)
    nd <- net_data()

    if (is.null(nd)) {
      kw <- tolower(trimws(input$keyword))
      return(div(class = "nm-warn",
        paste0("⚠ The keyword '", kw,
               "' was not found in the corpus, or no co-occurrences ",
               "met the minimum frequency threshold (", input$min_freq, "). "),
        br(),
        "Try: a different keyword · a lower minimum frequency · ",
        "a wider context window · removing stopwords."
      ))
    }

    tabsetPanel(
      tabPanel("🕸️ Interactive network", br(),
               visNetworkOutput("vis_net", height = "520px")),
      tabPanel("📊 Static network", br(),
               plotOutput("static_net", height = "500px")),
      tabPanel("📋 Co-occurrence table", br(),
               DTOutput("cooc_table"))
    )
  })

  # ── Interactive network (visNetwork) ────────────────────────
  output$vis_net <- renderVisNetwork({
    nd <- net_data()
    req(!is.null(nd))

    ed  <- nd$edges
    kw  <- nd$keyword

    # Node list
    all_nodes <- unique(c(ed$from, ed$to))
    max_cooc  <- max(ed$cooc)

    vis_nodes <- tibble::tibble(id = all_nodes) |>
      dplyr::mutate(
        label      = id,
        value      = dplyr::if_else(id == kw,
                       as.numeric(max_cooc) * 1.5,
                       as.numeric(
                         dplyr::coalesce(
                           ed$cooc[match(id, ed$to)],
                           as.integer(max_cooc)))),
        color.background = dplyr::if_else(
          id == kw, LADAL_PURPLE, "#a585c8"),
        color.border     = dplyr::if_else(
          id == kw, LADAL_GOLD, "#7a5ba8"),
        color.highlight  = LADAL_GOLD,
        font.size        = dplyr::if_else(id == kw, 18L, 14L),
        font.bold        = dplyr::if_else(id == kw, TRUE, FALSE),
        title            = dplyr::if_else(
          id == kw,
          paste0("<b>", id, "</b><br>keyword"),
          paste0("<b>", id, "</b><br>co-occ. with '", kw, "': ",
                 ed$cooc[match(id, ed$to)]))
      )

    vis_edges <- ed |>
      dplyr::mutate(
        width = cooc / max(cooc) * 6,
        title = paste0("Co-occurrences: ", cooc),
        color = list(color     = input$edge_color,
                     highlight = LADAL_GOLD,
                     opacity   = input$edge_alpha)
      )

    visNetwork::visNetwork(
      nodes  = vis_nodes,
      edges  = vis_edges,
      width  = "100%",
      height = "520px",
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
        smooth = list(enabled = TRUE,
                      type    = "curvedCW",
                      roundness = 0.1),
        shadow = FALSE
      ) |>
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE,
                                degree  = 1,
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
        stabilization = list(iterations = 200)
      ) |>
      visNetwork::visInteraction(
        navigationButtons = TRUE,
        tooltipDelay      = 80
      )
  })

  # ── Static network (textplot_network) ────────────────────────
  output$static_net <- renderPlot({
    nd <- net_data()
    req(!is.null(nd) && !is.null(nd$sub_fcm))

    kw      <- nd$keyword
    ed      <- nd$edges
    sub_fcm <- nd$sub_fcm

    # Label sizes proportional to co-occurrence (keyword gets max)
    feats    <- featnames(sub_fcm)
    cooc_sum <- rowSums(as.matrix(sub_fcm))

    label_sizes <- vapply(feats, function(f) {
      if (f == kw) return(input$label_size * 1.6)
      idx <- match(f, ed$to)
      if (is.na(idx)) return(input$label_size * 0.6)
      input$label_size * (0.5 + 0.5 * ed$cooc[idx] / max(ed$cooc))
    }, numeric(1))

    quanteda.textplots::textplot_network(
      x              = sub_fcm,
      min_freq       = input$min_freq / sum(as.matrix(sub_fcm)),
      edge_alpha     = input$edge_alpha,
      edge_color     = input$edge_color,
      edge_size      = 2.5,
      vertex_color   = ifelse(feats == kw, LADAL_PURPLE, "#a585c8"),
      vertex_size    = input$node_size,
      vertex_labelsize = label_sizes
    ) +
      labs(
        title    = paste0("Co-occurrence network: '", kw, "'"),
        subtitle = paste0(
          "Window ±", input$window_size,
          " · min freq ", input$min_freq,
          " · top ", input$top_n, " words")
      ) +
      theme(
        plot.title    = element_text(color = LADAL_PURPLE,
                                     face  = "bold", size = 14),
        plot.subtitle = element_text(color = "#666", size = 10)
      )
  }, bg = "white")

  # ── Co-occurrence table ───────────────────────────────────────
  output$cooc_table <- renderDT({
    nd <- net_data()
    req(!is.null(nd))

    display <- nd$edges |>
      dplyr::rename(
        Keyword   = from,
        Word      = to,
        CoocFreq  = cooc
      ) |>
      dplyr::arrange(dplyr::desc(CoocFreq))

    datatable(
      display,
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 25,
        order      = list(list(2, "desc"))
      ),
      caption = htmltools::tags$caption(
        style = paste0("color:", LADAL_PURPLE, "; font-weight:bold;"),
        paste0("Words co-occurring with '", nd$keyword,
               "' (window ±", input$window_size, ")")
      )
    ) |>
    formatStyle(
      "CoocFreq",
      background = styleColorBar(
        nd$edges$cooc, "#d8c8f0"),
      backgroundSize     = "98% 70%",
      backgroundRepeat   = "no-repeat",
      backgroundPosition = "center"
    )
  })

  # ── Download buttons ────────────────────────────────────────
  output$download_buttons <- renderUI({
    if (input$run_network == 0 || is.null(net_data()))
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Run a network to enable downloads."))
    tagList(
      downloadButton("dl_png",  "⬇ Network (.png)", class = "nm-dl"),
      downloadButton("dl_xlsx", "⬇ Table (.xlsx)",  class = "nm-dl"),
      downloadButton("dl_csv",  "⬇ Table (.csv)",   class = "nm-dl")
    )
  })

  # ── Download handlers ────────────────────────────────────────
  output$dl_xlsx <- downloadHandler(
    filename = function()
      paste0("network_", net_data()$keyword, "_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(as.data.frame(net_data()$edges), file)
  )

  output$dl_csv <- downloadHandler(
    filename = function()
      paste0("network_", net_data()$keyword, "_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(net_data()$edges, file)
  )

  output$dl_png <- downloadHandler(
    filename = function()
      paste0("network_", net_data()$keyword, "_", Sys.Date(), ".png"),
    content  = function(file) {
      nd      <- net_data()
      req(!is.null(nd))
      kw      <- nd$keyword
      ed      <- nd$edges
      sub_fcm <- nd$sub_fcm
      feats   <- featnames(sub_fcm)

      label_sizes <- vapply(feats, function(f) {
        if (f == kw) return(input$label_size * 1.6)
        idx <- match(f, ed$to)
        if (is.na(idx)) return(input$label_size * 0.6)
        input$label_size * (0.5 + 0.5 * ed$cooc[idx] / max(ed$cooc))
      }, numeric(1))

      p <- quanteda.textplots::textplot_network(
        x              = sub_fcm,
        min_freq       = input$min_freq / sum(as.matrix(sub_fcm)),
        edge_alpha     = input$edge_alpha,
        edge_color     = input$edge_color,
        edge_size      = 2.5,
        vertex_color   = ifelse(feats == kw, LADAL_PURPLE, "#a585c8"),
        vertex_size    = input$node_size,
        vertex_labelsize = label_sizes
      ) +
        labs(title = paste0("Co-occurrence network: '", kw, "'"),
             subtitle = paste0("Window ±", input$window_size,
                               " · min freq ", input$min_freq)) +
        theme(plot.title = element_text(color = LADAL_PURPLE,
                                        face = "bold"))

      ggplot2::ggsave(file, plot = p,
                      width = 9, height = 7, dpi = 200, bg = "white")
    }
  )
}

# ══════════════════════════════════════════════════════════════
shinyApp(ui, server)

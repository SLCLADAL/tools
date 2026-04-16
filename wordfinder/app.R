# ============================================================
#  WordFinder — LADAL Concordancing Tool (Shiny)
#  https://ladal.edu.au
# ============================================================

library(shiny)
library(quanteda)
library(quanteda.textplots)
library(tidyverse)
library(writexl)
library(DT)

quanteda_options(verbose = FALSE)

# ── Helpers ──────────────────────────────────────────────────────────

ladal_purple <- "#51247a"
ladal_gold   <- "#f0a500"

make_kwic <- function(corp, pattern, window, valuetype, ignore_case) {
  tryCatch({
    kwic(
      tokens(corp),
      pattern          = phrase(pattern),
      window           = window,
      valuetype        = valuetype,
      separator        = " ",
      case_insensitive = ignore_case
    ) |> as.data.frame()
  }, error = function(e) NULL)
}

# Parse a comma-separated string of patterns into a trimmed character vector,
# dropping blanks.
parse_patterns <- function(raw) {
  raw |>
    strsplit(",") |>
    unlist() |>
    trimws() |>
    (\(x) x[nchar(x) > 0])()
}

# ── UI ───────────────────────────────────────────────────────────────

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
    tags$a("→ Tutorial", href = "https://ladal.edu.au/tutorials/concordancing_tutorial/concordancing_tutorial.html", target = "_blank",
           style = "font-size:.78rem;color:#51247a;")
  ),
  tags$blockquote(
    style = "border-left:3px solid #c8b8de;padding-left:12px;margin:0 0 10px 0;color:#444;",
    HTML(paste0(
      "Schweinberger, Martin. (2026). ",
      "<em>WordFinder: A browser-based KWIC concordancing tool</em>. ",
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
        "@misc{schweinberger2026wordfinder,\n",
        "  author       = {Schweinberger, Martin},\n",
        "  title        = {WordFinder: A browser-based KWIC concordancing tool},\n",
        "  year         = {2026},\n",
        "  organization = {The University of Queensland},\n",
        "  url          = {https://ladal.edu.au/tools.html}\n",
        "}"
      )
    )
  )
)

ui <- fluidPage(
  title = "WordFinder | LADAL",

  tags$head(
    tags$style(HTML(paste0("
      /* ── Reset & base ── */
      body { font-family: 'Segoe UI', Arial, sans-serif;
             background: #f7f4fb; color: #222; margin: 0; }

      /* ── Top banner ── */
      .wf-banner {
        background: ", ladal_purple, ";
        color: white;
        padding: 18px 32px 14px 32px;
        display: flex; align-items: center; gap: 18px;
        border-bottom: 4px solid ", ladal_gold, ";
      }
      .wf-banner .wf-title  { font-size: 1.7rem; font-weight: 700;
                               letter-spacing: .5px; margin: 0; }
      .wf-banner .wf-sub    { font-size: .88rem; opacity: .85; margin: 2px 0 0 0; }
      .wf-banner .wf-logo   { font-size: 2rem; }

      /* ── Layout ── */
      .wf-body  { display: flex; min-height: calc(100vh - 80px); }
      .wf-side  { width: 310px; min-width: 260px; max-width: 340px;
                  background: white;
                  border-right: 1px solid #e0d8ec;
                  padding: 22px 20px 30px 20px;
                  box-shadow: 2px 0 8px rgba(81,36,122,.06); }
      .wf-main  { flex: 1; padding: 24px 28px; overflow-x: auto; }

      /* ── Sidebar sections ── */
      .wf-section-title {
        font-size: .75rem; font-weight: 700; letter-spacing: 1.2px;
        text-transform: uppercase; color: ", ladal_purple, ";
        border-bottom: 2px solid ", ladal_gold, ";
        padding-bottom: 4px; margin: 20px 0 12px 0;
      }
      .wf-section-title:first-child { margin-top: 0; }

      /* ── Inputs ── */
      .form-control, .selectize-input {
        border: 1.5px solid #d0c8e0 !important;
        border-radius: 6px !important;
        font-size: .92rem !important;
      }
      .form-control:focus { border-color: ", ladal_purple, " !important;
                            box-shadow: 0 0 0 2px rgba(81,36,122,.15) !important; }
      label { font-size: .88rem; font-weight: 600; color: #444; }

      /* ── Search button ── */
      #run_search {
        width: 100%;
        background: ", ladal_purple, " !important;
        border: none !important;
        color: white !important;
        font-weight: 700;
        font-size: 1rem;
        padding: 10px;
        border-radius: 7px;
        margin-top: 6px;
        transition: background .2s;
      }
      #run_search:hover { background: #3a1860 !important; }

      /* ── Download button ── */
      .wf-dl-btn {
        display: inline-block; margin: 4px 6px 4px 0;
        background: white; border: 1.5px solid ", ladal_purple, ";
        color: ", ladal_purple, "; font-weight: 600; font-size: .85rem;
        padding: 6px 14px; border-radius: 6px; cursor: pointer;
        text-decoration: none; transition: all .15s;
      }
      .wf-dl-btn:hover { background: ", ladal_purple, "; color: white; }

      /* ── Stat cards ── */
      .wf-stats { display: flex; gap: 14px; margin-bottom: 20px;
                  flex-wrap: wrap; }
      .wf-card  { background: white; border-radius: 9px;
                  border-left: 4px solid ", ladal_purple, ";
                  padding: 12px 18px; min-width: 120px;
                  box-shadow: 0 1px 6px rgba(81,36,122,.08); }
      .wf-card .wf-card-val { font-size: 1.6rem; font-weight: 700;
                               color: ", ladal_purple, "; line-height: 1; }
      .wf-card .wf-card-lbl { font-size: .78rem; color: #888;
                               margin-top: 3px; }

      /* ── Info / tip boxes ── */
      .wf-info {
        background: #f4f0f8; border-left: 4px solid ", ladal_purple, ";
        border-radius: 5px; padding: 10px 14px; font-size: .85rem;
        color: #444; margin-bottom: 18px;
      }
      .wf-info a { color: ", ladal_purple, "; }

      /* ── Results header ── */
      .wf-results-hdr {
        font-size: 1rem; font-weight: 700; color: ", ladal_purple, ";
        margin-bottom: 10px; display: flex; align-items: center; gap: 10px;
      }

      /* ── Keyword highlight ── */
      .kw-highlight { color: #8e44ad; font-weight: 700; }

      /* ── DT table tweaks ── */
      .dataTables_wrapper { font-size: .88rem; }
      table.dataTable thead th {
        background: ", ladal_purple, " !important;
        color: white !important; font-weight: 600;
        border-bottom: 2px solid ", ladal_gold, " !important;
      }
      table.dataTable tbody tr:hover { background: #f4f0f8 !important; }

      /* ── Tab strip ── */
      .nav-tabs > li > a { color: ", ladal_purple, "; font-weight: 600; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        border-top: 3px solid ", ladal_purple, " !important;
        color: ", ladal_purple, " !important;
      }

      /* ── Footer ── */
      .wf-footer {
        background: #2d1a4a; color: #c8b8de;
        font-size: .78rem; padding: 12px 32px;
        display: flex; gap: 18px; align-items: center;
      }
      .wf-footer a { color: #d4b8f5; }

      /* ── Upload area ── */
      .shiny-input-container .btn { background: white;
        border: 1.5px dashed ", ladal_purple, ";
        color: ", ladal_purple, "; font-weight: 600; width: 100%; }

      /* ── Error / warning ── */
      .wf-warn { color: #b34700; background: #fff4e5;
                 border-left: 4px solid #f0a500;
                 border-radius: 5px; padding: 8px 14px;
                 font-size: .87rem; margin-bottom: 12px; }
      .wf-empty { color: #888; text-align: center;
                  padding: 40px 20px; font-size: .95rem; }

      /* ── Dispersion plot container ── */
      .wf-dispersion-wrap {
        background: white; border-radius: 9px;
        border: 1px solid #e0d8ec;
        padding: 18px 20px;
        box-shadow: 0 1px 6px rgba(81,36,122,.06);
      }
      .wf-dispersion-hint {
        font-size: .82rem; color: #888; margin-top: 8px;
      }
    ")))
  ),

  # ── Banner ──────────────────────────────────────────────
  div(class = "wf-banner",
    div(class = "wf-logo", "🔍"),
    div(
      p(class = "wf-title", "WordFinder"),
      p(class = "wf-sub",
        "Keyword-in-Context concordancing · ",
        tags$a("LADAL", href = "https://ladal.edu.au",
               style = "color:#f0c060;"))
    )
  ),

  # ── Body ────────────────────────────────────────────────
  div(class = "wf-body",

    # ── Sidebar ─────────────────────────────────────────
    div(class = "wf-side",

      # STEP 1 — Upload
      div(class = "wf-section-title", "① Upload texts"),
      div(class = "wf-info",
        "Upload one or more ", tags$b(".txt"), " files. Each file becomes
         one document in the concordance."
      ),
      fileInput("files", NULL,
                multiple = TRUE,
                accept   = ".txt",
                placeholder = "No files selected",
                buttonLabel = "📂 Choose .txt files"),
      uiOutput("corpus_status"),

      # STEP 2 — Search
      div(class = "wf-section-title", "② Search"),
      textInput("pattern", "Search term or pattern",
                value = "the",
                placeholder = "word, phrase, or regex…"),
      div(class = "wf-info", style = "margin-top:-8px; margin-bottom:10px;",
        "Examples: ", tags$code("climate"), " · ",
        tags$code("the economy"), " · ",
        tags$code("wom[ae]n"), " (regex)"
      ),

      selectInput("valuetype", "Match type",
        choices  = c("Fixed (exact)"  = "fixed",
                     "Glob (wildcard)" = "glob",
                     "Regex"          = "regex"),
        selected = "regex"),

      sliderInput("window", "Context window (words each side)",
                  min = 1, max = 15, value = 5, step = 1),

      checkboxInput("ignore_case", "Ignore case", value = TRUE),

      actionButton("run_search", "🔍  Search", class = "btn-primary"),

      # STEP 3 — Dispersion-specific options
      div(class = "wf-section-title", "③ Dispersion plot terms"),
      div(class = "wf-info",
        "Enter one or more comma-separated terms to compare in the
         dispersion plot. Defaults to the search term above."
      ),
      textInput("disp_patterns", "Terms (comma-separated)",
                value = "",
                placeholder = "e.g. climate, change, policy"),

      # STEP 4 — Export
      div(class = "wf-section-title", "④ Download results"),
      uiOutput("download_buttons")
    ),

    # ── Main panel ──────────────────────────────────────
    div(class = "wf-main",

      # Welcome / intro box (hidden after first search)
      uiOutput("welcome_box"),

      # Stats cards
      uiOutput("stats_cards"),

      # Results tabs
      uiOutput("results_ui")
    )
  ),

  # ── Footer ──────────────────────────────────────────────
  div(class = "wf-footer",
    span("WordFinder · LADAL · University of Queensland"),
    tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
    tags$a("Concordancing Tutorial",
           href = "https://ladal.edu.au/tutorials/concordancing_tutorial/concordancing_tutorial.html"),
    tags$a("Cite this tool",
           href = "https://ladal.edu.au/about.html#citing")
  ),
  CITATION_FOOTER
)

# ── Server ───────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive: corpus from uploaded files ───────────────
  corp <- reactive({
    req(input$files)
    texts <- map(input$files$datapath, function(p) {
      readLines(p, warn = FALSE) |> paste(collapse = " ")
    })
    names(texts) <- tools::file_path_sans_ext(input$files$name)
    corpus(unlist(texts), docnames = names(texts))
  })

  # ── Reactive: run concordance ───────────────────────────
  kwic_result <- eventReactive(input$run_search, {
    req(corp(), nchar(trimws(input$pattern)) > 0)
    make_kwic(corp(), trimws(input$pattern),
              input$window, input$valuetype, input$ignore_case)
  })

  # ── Reactive: resolve dispersion patterns ──────────────
  # Uses the dispersion-specific field if filled in,
  # otherwise falls back to the main search pattern.
  disp_patterns <- eventReactive(input$run_search, {
    raw <- trimws(input$disp_patterns)
    if (nchar(raw) == 0) {
      trimws(input$pattern)
    } else {
      parse_patterns(raw)
    }
  })

  # ── Corpus status badge ─────────────────────────────────
  output$corpus_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "wf-warn",
          "⚠ No files uploaded yet. Upload at least one .txt file.")
    } else {
      n   <- nrow(input$files)
      lbl <- if (n == 1) "1 file loaded" else paste(n, "files loaded")
      div(style = paste0(
        "background:#eafaf1; border-left:4px solid #27ae60;",
        "border-radius:5px; padding:8px 14px; font-size:.85rem;",
        "color:#1a6b3c; margin-bottom:4px;"),
        paste0("✔ ", lbl, ": ",
               paste(tools::file_path_sans_ext(input$files$name),
                     collapse = ", ")))
    }
  })

  # ── Welcome box (shown before first search) ─────────────
  output$welcome_box <- renderUI({
    if (input$run_search == 0) {
      div(class = "wf-info", style = "font-size:.93rem;",
        tags$b("Welcome to WordFinder."), br(),
        "Upload your plain-text files using the panel on the left,
         enter a search term, and click ", tags$b("Search"),
        " to extract keyword-in-context (KWIC) concordance lines.", br(), br(),
        "Results appear as an interactive concordance table, a frequency
         summary, and a ", tags$b("lexical dispersion plot"),
        " showing where terms occur across each document.", br(), br(),
        tags$a("→ Learn more about concordancing",
               href = "https://ladal.edu.au/tutorials/concordancing_tutorial/concordancing_tutorial.html")
      )
    }
  })

  # ── Stats cards ─────────────────────────────────────────
  output$stats_cards <- renderUI({
    req(input$run_search > 0)
    df <- kwic_result()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    ndocs  <- length(unique(df$docname))
    nhits  <- nrow(df)
    norm   <- if (!is.null(corp())) {
      round(nhits / sum(ntoken(corp())) * 1000, 2)
    } else NA

    div(class = "wf-stats",
      div(class = "wf-card",
        div(class = "wf-card-val", nhits),
        div(class = "wf-card-lbl", "Total hits")),
      div(class = "wf-card",
        div(class = "wf-card-val", ndocs),
        div(class = "wf-card-lbl", "Documents")),
      div(class = "wf-card",
        div(class = "wf-card-val", norm),
        div(class = "wf-card-lbl", "Hits per 1,000 tokens"))
    )
  })

  # ── Results UI (tabs) ────────────────────────────────────
  output$results_ui <- renderUI({
    req(input$run_search > 0)
    df <- kwic_result()

    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "wf-empty",
        "🔎 No matches found for ",
        tags$b(paste0('"', input$pattern, '"')),
        br(), br(),
        "Try a different pattern, or check the Match type setting."))
    }

    tabsetPanel(
      tabPanel("📋 Concordance lines",   br(), DTOutput("kwic_table")),
      tabPanel("📊 Frequency by document", br(), DTOutput("freq_table")),
      tabPanel("📈 Dispersion plot",
        br(),
        div(class = "wf-dispersion-wrap",
          plotOutput("disp_plot", height = "auto"),
          div(class = "wf-dispersion-hint",
            "Each tick mark shows one occurrence of the term.
             Documents appear as separate rows; the x-axis is
             token position within each document."
          ),
          br(),
          uiOutput("disp_download_ui")
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
  })

  # ── KWIC table ───────────────────────────────────────────
  output$kwic_table <- renderDT({
    df <- kwic_result()
    req(!is.null(df) && nrow(df) > 0)

    display <- df |>
      select(Document = docname,
             Left     = pre,
             Keyword  = keyword,
             Right    = post)

    datatable(
      display,
      rownames   = FALSE,
      filter     = "top",
      escape     = FALSE,
      extensions = c("Buttons"),
      options    = list(
        dom       = "Bfrtip",
        buttons   = list("copy"),
        pageLength = 25,
        autoWidth  = TRUE,
        columnDefs = list(
          list(width = "15%",  targets = 0),
          list(width = "35%",  targets = 1,
               className = "dt-right"),
          list(width = "15%",  targets = 2,
               className = "dt-center"),
          list(width = "35%",  targets = 3,
               className = "dt-left")
        )
      )
    ) |>
    formatStyle("Keyword",
                color      = "#8e44ad",
                fontWeight = "bold") |>
    formatStyle("Left",
                textAlign = "right",
                color     = "#555") |>
    formatStyle("Right",
                textAlign = "left",
                color     = "#555")
  })

  # ── Frequency table ──────────────────────────────────────
  output$freq_table <- renderDT({
    df <- kwic_result()
    req(!is.null(df) && nrow(df) > 0)

    tok_counts <- ntoken(corp())

    freq <- df |>
      count(docname, name = "Hits") |>
      left_join(
        tibble(docname = names(tok_counts),
               Tokens  = as.integer(tok_counts)),
        by = "docname"
      ) |>
      mutate(Per1000 = round(Hits / Tokens * 1000, 2)) |>
      arrange(desc(Hits)) |>
      rename(Document = docname)

    datatable(freq, rownames = FALSE, options = list(dom = "t",
              pageLength = 50)) |>
    formatStyle("Hits",
                background = styleColorBar(freq$Hits, "#d8c8f0"),
                backgroundSize = "98% 70%",
                backgroundRepeat = "no-repeat",
                backgroundPosition = "center")
  })

  # ── Dispersion plot (reactive plot object) ───────────────
  #
  # We build the plot as a reactive so both renderPlot and the
  # PNG download handler can share the same code path.
  disp_plot_obj <- reactive({
    req(corp())
    pats <- disp_patterns()
    req(length(pats) > 0)

    toks <- tokens(corp())

    # Build a kwic object for each pattern and pass all of them
    # to textplot_xray, which colour-codes by pattern when multiple
    # kwic objects are supplied.
    kwic_list <- lapply(pats, function(p) {
      tryCatch(
        kwic(toks,
             pattern          = phrase(p),
             valuetype        = input$valuetype,
             case_insensitive = input$ignore_case),
        error = function(e) NULL
      )
    })

    # Drop any patterns that produced no matches
    kwic_list <- Filter(Negate(is.null), kwic_list)
    if (length(kwic_list) == 0) return(NULL)

    # textplot_xray accepts individual kwic objects as separate arguments.
    # Use do.call so we can pass a variable number of them.
    do.call(textplot_xray, c(kwic_list, list(scale = "absolute")))
  })

  # Dynamic plot height: ~120 px per document + 80 px base,
  # so a single document gets a readable band and many documents
  # don't get squeezed.
  disp_plot_height <- reactive({
    req(corp())
    n_docs <- ndoc(corp())
    max(300, 80 + n_docs * 120)
  })

  output$disp_plot <- renderPlot({
    p <- disp_plot_obj()
    validate(need(!is.null(p),
      "No matches found for the dispersion term(s). Try different terms."))
    p
  }, height = function() disp_plot_height())

  # ── Dispersion download UI ───────────────────────────────
  output$disp_download_ui <- renderUI({
    req(!is.null(disp_plot_obj()))
    tagList(
      downloadButton("dl_disp_png", "⬇ PNG",  class = "wf-dl-btn"),
      downloadButton("dl_disp_pdf", "⬇ PDF",  class = "wf-dl-btn")
    )
  })

  output$dl_disp_png <- downloadHandler(
    filename = function() {
      paste0("dispersion_",
             gsub("[^a-zA-Z0-9]", "_",
                  paste(disp_patterns(), collapse = "-")),
             "_", Sys.Date(), ".png")
    },
    content = function(file) {
      p <- disp_plot_obj()
      req(!is.null(p))
      n_docs <- ndoc(corp())
      h_in   <- max(4, 1.2 + n_docs * 1.5)   # inches
      ggplot2::ggsave(file, plot = p,
                      device = "png",
                      width  = 10, height = h_in,
                      dpi    = 150, bg = "white")
    }
  )

  output$dl_disp_pdf <- downloadHandler(
    filename = function() {
      paste0("dispersion_",
             gsub("[^a-zA-Z0-9]", "_",
                  paste(disp_patterns(), collapse = "-")),
             "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      p <- disp_plot_obj()
      req(!is.null(p))
      n_docs <- ndoc(corp())
      h_in   <- max(4, 1.2 + n_docs * 1.5)
      ggplot2::ggsave(file, plot = p,
                      device = "pdf",
                      width  = 10, height = h_in,
                      bg     = "white")
    }
  )

  # ── Download buttons (concordance) ──────────────────────
  output$download_buttons <- renderUI({
    if (is.null(kwic_result()) || nrow(kwic_result()) == 0)
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Run a search to enable downloads."))
    tagList(
      downloadButton("dl_xlsx", "⬇ Excel (.xlsx)", class = "wf-dl-btn"),
      downloadButton("dl_csv",  "⬇ CSV (.csv)",   class = "wf-dl-btn")
    )
  })

  # ── Download handlers (concordance) ─────────────────────
  dl_data <- reactive({
    df <- kwic_result()
    req(!is.null(df) && nrow(df) > 0)
    df |>
      select(Document = docname,
             Position = from,
             Left     = pre,
             Keyword  = keyword,
             Right    = post,
             Pattern  = pattern)
  })

  output$dl_xlsx <- downloadHandler(
    filename = function() {
      paste0("wordfinder_", gsub("[^a-zA-Z0-9]", "_", input$pattern),
             "_", Sys.Date(), ".xlsx")
    },
    content = function(file) writexl::write_xlsx(dl_data(), file)
  )

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("wordfinder_", gsub("[^a-zA-Z0-9]", "_", input$pattern),
             "_", Sys.Date(), ".csv")
    },
    content = function(file) readr::write_csv(dl_data(), file)
  )

  # ── Parameters download ─────────────────────────────────────────
  output$dl_params <- downloadHandler(
    filename = function() paste0("wordfinder_params_", Sys.Date(), ".txt"),
    content  = function(file) {
      lines <- c(
        paste0("Tool:                ", "WordFinder — KWIC Concordancing"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("quanteda:            ", as.character(packageVersion('quanteda'))),
        paste0("---                  ", ""),
        paste0("Search term:         ", input$pattern),
        paste0("Match type:          ", input$valuetype),
        paste0("Case-insensitive:    ", as.character(!input$ignore_case)),
        paste0("Context window:      ", paste0(input$window, ' words each side')),
        paste0("Files:               ", if (!is.null(input$files)) paste(input$files$name, collapse=", ") else "none")
      )
      writeLines(lines, file)
    }
  )

  output$params_dl_ui <- renderUI({
    downloadButton("dl_params", "⬇ Download parameters (.txt)", class = "wf-dl-btn")
  })

  output$params_preview <- renderText({
    paste(c(
        paste0("Tool:                ", "WordFinder — KWIC Concordancing"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("quanteda:            ", as.character(packageVersion('quanteda'))),
        paste0("---                  ", ""),
        paste0("Search term:         ", input$pattern),
        paste0("Match type:          ", input$valuetype),
        paste0("Case-insensitive:    ", as.character(!input$ignore_case)),
        paste0("Context window:      ", paste0(input$window, ' words each side'))
    ), collapse="\n")
  })

}

# ── Run ──────────────────────────────────────────────────────────────)

shinyApp(ui, server)

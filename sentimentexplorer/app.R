# ============================================================
#  SentimentExplorer — LADAL Sentiment Analysis Tool (Shiny)
#  https://ladal.edu.au
#
#  Uses the NRC Word-Emotion Association Lexicon:
#    Mohammad, S.M. & Turney, P.D. (2013). Crowdsourcing a
#    Word-Emotion Association Lexicon. Computational Intelligence,
#    29(3): 436-465. https://doi.org/10.1111/j.1467-8640.2012.00460.x
#
#  The lexicon (nrc_lexicon.csv) must be present in the app
#  directory. Run prepare_nrc.R once to generate it from the
#  raw NRC .txt file (see prepare_nrc.R for download instructions).
# ============================================================

library(shiny)
library(tidyverse)
library(stringi)
library(DT)
library(writexl)
library(ggplot2)

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

# ── Emotion colours ──────────────────────────────────────────
EMOTION_COLOURS <- c(
  anger        = "#c0392b",
  anticipation = "#e67e22",
  disgust      = "#8e44ad",
  fear         = "#2c3e50",
  joy          = "#f1c40f",
  sadness      = "#2980b9",
  surprise     = "#1abc9c",
  trust        = "#27ae60",
  negative     = "#e74c3c",
  positive     = "#2ecc71"
)

NRC_CATEGORIES <- c("anger","anticipation","disgust","fear",
                    "joy","sadness","surprise","trust",
                    "negative","positive")

# ── Load NRC lexicon ─────────────────────────────────────────
# Expected location: nrc_lexicon.csv in same directory as app.R
# Two-column CSV: word, sentiment
load_nrc <- function() {
  # Try app directory first, then working directory
  candidates <- c(
    file.path(dirname(sys.frame(0)$ofile %||% ""), "nrc_lexicon.csv"),
    "nrc_lexicon.csv",
    file.path(getwd(), "nrc_lexicon.csv")
  )
  for (p in candidates) {
    if (file.exists(p)) {
      df <- tryCatch(
        read.csv(p, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df) && all(c("word","sentiment") %in% names(df))) {
        return(df)
      }
    }
  }
  NULL   # returns NULL if not found — app will show informative error
}

NRC <- load_nrc()

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── Core analysis functions ───────────────────────────────────

# Tokenise a character string into lowercase words (letters only).
tokenise <- function(text) {
  words <- stri_extract_all_words(stri_trans_tolower(text))[[1]]
  words[!is.na(words)]
}

# Annotate tokens with NRC categories.
# Returns a data.frame: section, token_index, word, <10 category cols>
annotate_section <- function(words, section_name, nrc_df) {
  if (length(words) == 0) return(NULL)

  # Wide NRC: one row per word, one column per category
  nrc_wide <- nrc_df |>
    mutate(value = 1L) |>
    tidyr::pivot_wider(names_from  = sentiment,
                       values_from = value,
                       values_fill = 0L)

  # Ensure all 10 columns exist
  for (cat in NRC_CATEGORIES) {
    if (!cat %in% names(nrc_wide)) nrc_wide[[cat]] <- 0L
  }

  token_df <- data.frame(
    section     = section_name,
    token_index = seq_along(words),
    word        = words,
    stringsAsFactors = FALSE
  )

  merged <- left_join(token_df, nrc_wide, by = "word")

  # Fill NA (words not in lexicon) with 0
  for (cat in NRC_CATEGORIES) {
    merged[[cat]][is.na(merged[[cat]])] <- 0L
  }

  merged
}

# Build summary: count and % of tokens matching each category, per section.
build_summary <- function(annotated_df) {
  annotated_df |>
    group_by(section) |>
    summarise(
      Total_tokens = n(),
      across(all_of(NRC_CATEGORIES),
             list(n   = ~ sum(.x > 0),
                  pct = ~ round(mean(.x > 0) * 100, 1)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    )
}

# Long-format summary for ggplot.
summary_long <- function(summary_df) {
  summary_df |>
    select(section, ends_with("_n"), ends_with("_pct")) |>
    tidyr::pivot_longer(
      cols      = -section,
      names_to  = c("category", ".value"),
      names_sep = "_(?=[^_]+$)"       # split on last underscore
    ) |>
    rename(count = n, percent = pct) |>
    mutate(category = factor(category, levels = NRC_CATEGORIES))
}

# ── UI ───────────────────────────────────────────────────────

ui <- fluidPage(
  title = "SentimentExplorer | LADAL",

  tags$head(
    tags$style(HTML(paste0("
      @import url('https://fonts.googleapis.com/css2?family=Source+Serif+4:wght@400;600;700&family=Source+Sans+3:wght@400;600;700&display=swap');

      *, *::before, *::after { box-sizing: border-box; }

      body {
        font-family: 'Source Sans 3', sans-serif;
        background: #f6f4f9;
        color: #1c1c2e;
        margin: 0;
        font-size: 14px;
      }

      /* ── Banner ── */
      .se-banner {
        background: ", LADAL_PURPLE, ";
        color: white;
        padding: 17px 32px 14px 32px;
        display: flex; align-items: center; gap: 16px;
        border-bottom: 4px solid ", LADAL_GOLD, ";
      }
      .se-banner-icon  { font-size: 2rem; line-height: 1; }
      .se-banner-title {
        font-family: 'Source Serif 4', serif;
        font-size: 1.65rem; font-weight: 700;
        letter-spacing: .2px; margin: 0;
      }
      .se-banner-sub { font-size: .83rem; opacity: .8; margin: 2px 0 0; }
      .se-banner a   { color: #f7d97a; text-decoration: none; }

      /* ── Layout ── */
      .se-body { display: flex; min-height: calc(100vh - 80px); }
      .se-side {
        width: 320px; min-width: 270px; max-width: 350px;
        background: white;
        border-right: 1px solid #e2dced;
        padding: 20px 18px 32px 18px;
        box-shadow: 2px 0 10px rgba(81,36,122,.05);
        overflow-y: auto;
      }
      .se-main { flex: 1; padding: 24px 28px; overflow-x: auto; }

      /* ── Sidebar section titles ── */
      .se-sec {
        font-size: .72rem; font-weight: 700; letter-spacing: 1.3px;
        text-transform: uppercase; color: ", LADAL_PURPLE, ";
        border-bottom: 2px solid ", LADAL_GOLD, ";
        padding-bottom: 4px; margin: 22px 0 11px 0;
      }
      .se-sec:first-child { margin-top: 0; }

      /* ── Inputs ── */
      .form-control, .selectize-input {
        border: 1.5px solid #d4cce4 !important;
        border-radius: 6px !important;
        font-size: .88rem !important;
        font-family: 'Source Sans 3', sans-serif !important;
      }
      .form-control:focus {
        border-color: ", LADAL_PURPLE, " !important;
        box-shadow: 0 0 0 2px rgba(81,36,122,.12) !important;
      }
      label { font-size: .83rem; font-weight: 600; color: #555;
              margin-bottom: 3px; }
      .form-group { margin-bottom: 9px; }

      /* ── Info / warn / ok boxes ── */
      .se-info {
        background: #f4f0f8; border-left: 4px solid ", LADAL_PURPLE, ";
        border-radius: 5px; padding: 9px 13px;
        font-size: .83rem; color: #444; margin-bottom: 12px;
      }
      .se-warn {
        background: #fff4e5; border-left: 4px solid ", LADAL_GOLD, ";
        border-radius: 5px; padding: 9px 13px;
        font-size: .83rem; color: #6b4000; margin-bottom: 10px;
      }
      .se-ok {
        background: #eafaf1; border-left: 4px solid #27ae60;
        border-radius: 5px; padding: 9px 13px;
        font-size: .83rem; color: #1a6b3c; margin-bottom: 8px;
      }
      .se-error {
        background: #fdecea; border-left: 4px solid #c0392b;
        border-radius: 5px; padding: 12px 16px;
        font-size: .88rem; color: #7b241c; margin-bottom: 14px;
      }

      /* ── Section builder ── */
      .se-section-row {
        background: #faf8fd;
        border: 1.5px solid #e2dced;
        border-radius: 8px;
        padding: 10px 12px;
        margin-bottom: 8px;
      }
      .se-section-row:hover { border-color: ", LADAL_PURPLE, "; }
      .se-section-label {
        font-weight: 700; font-size: .85rem;
        color: ", LADAL_PURPLE, "; margin-bottom: 5px;
      }

      /* ── Action buttons ── */
      .se-run-btn {
        width: 100%;
        background: ", LADAL_PURPLE, " !important;
        border: none !important; color: white !important;
        font-weight: 700; font-size: .97rem; padding: 11px;
        border-radius: 7px; margin-top: 4px;
        font-family: 'Source Sans 3', sans-serif;
        transition: background .2s;
      }
      .se-run-btn:hover { background: #3d1763 !important; }

      .se-add-btn {
        background: white !important;
        border: 1.5px solid ", LADAL_PURPLE, " !important;
        color: ", LADAL_PURPLE, " !important;
        font-weight: 600; font-size: .83rem;
        padding: 5px 12px; border-radius: 6px;
        margin-bottom: 8px;
        font-family: 'Source Sans 3', sans-serif;
        transition: all .15s;
      }
      .se-add-btn:hover {
        background: ", LADAL_PURPLE, " !important;
        color: white !important;
      }

      /* ── Download buttons ── */
      .se-dl-btn {
        display: inline-flex; align-items: center; gap: 6px;
        background: white;
        border: 1.5px solid ", LADAL_PURPLE, ";
        color: ", LADAL_PURPLE, ";
        font-weight: 600; font-size: .83rem;
        padding: 6px 14px; border-radius: 6px;
        margin: 3px 5px 3px 0;
        cursor: pointer; text-decoration: none;
        transition: all .15s;
        font-family: 'Source Sans 3', sans-serif;
      }
      .se-dl-btn:hover {
        background: ", LADAL_PURPLE, "; color: white;
      }

      /* ── Stat chips ── */
      .se-chips { display: flex; gap: 10px; flex-wrap: wrap;
                  margin-bottom: 18px; }
      .se-chip  {
        background: white; border-radius: 20px;
        border: 1.5px solid #e2dced;
        padding: 5px 14px; font-size: .82rem;
        display: flex; align-items: center; gap: 5px;
      }
      .se-chip b { color: ", LADAL_PURPLE, "; font-size: .95rem; }

      /* ── Tab strip ── */
      .nav-tabs > li > a {
        color: ", LADAL_PURPLE, " !important; font-weight: 600;
        font-family: 'Source Sans 3', sans-serif;
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
        font-family: 'Source Sans 3', sans-serif;
      }
      table.dataTable tbody tr:hover { background: #f4f0f8 !important; }

      /* ── Plot container ── */
      .se-plot-wrap {
        background: white;
        border: 1px solid #e2dced;
        border-radius: 8px;
        padding: 18px 20px;
        box-shadow: 0 1px 6px rgba(81,36,122,.05);
      }

      /* ── Citation box ── */
      .se-citation {
        background: #f9f7fd;
        border: 1px solid #e2dced;
        border-radius: 8px;
        padding: 14px 18px;
        font-size: .82rem;
        color: #555;
        margin-top: 20px;
      }
      .se-citation code {
        background: #ece8f5;
        padding: 2px 5px;
        border-radius: 3px;
        font-size: .8rem;
      }

      /* ── Welcome ── */
      .se-welcome {
        max-width: 560px; margin: 40px auto;
        text-align: center; color: #888;
      }
      .se-welcome-icon { font-size: 3.2rem; margin-bottom: 14px; }
      .se-welcome h3 {
        font-family: 'Source Serif 4', serif;
        color: #555; font-size: 1.2rem;
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
      .se-footer {
        background: #2d1a4a; color: #c8b8de;
        font-size: .77rem; padding: 11px 32px;
        display: flex; gap: 18px; align-items: center;
      }
      .se-footer a { color: #d4b8f5; }

      /* ── Emotion colour dots ── */
      .emotion-dot {
        display: inline-block; width: 10px; height: 10px;
        border-radius: 50%; margin-right: 5px;
        vertical-align: middle;
      }
    ")))
  ),

  # ── Banner ────────────────────────────────────────────────
  div(class = "se-banner",
    div(class = "se-banner-icon", "💬"),
    div(
      p(class = "se-banner-title", "SentimentExplorer"),
      p(class = "se-banner-sub",
        "NRC word-emotion analysis · ",
        tags$a("LADAL", href = "https://ladal.edu.au",
               style = "color:#f7d97a;"))
    )
  ),

  # ── Body ──────────────────────────────────────────────────
  div(class = "se-body",

    # ── Sidebar ───────────────────────────────────────────
    div(class = "se-side",

      # STEP 1 — Upload
      div(class = "se-sec", "① Upload texts"),
      div(class = "se-info",
        "Upload one or more ", tags$b(".txt"), " files.
         Each file will appear as an available text below."
      ),
      fileInput("files", NULL,
                multiple    = TRUE,
                accept      = ".txt",
                buttonLabel = "📂 Choose .txt files"),
      uiOutput("upload_status"),

      # STEP 2 — Define sections
      div(class = "se-sec", "② Define text sections"),
      div(class = "se-info",
        "Assign each file to a named section. Files in the same
         section are merged before analysis. You can keep each
         file as its own section, or group files together."
      ),
      uiOutput("section_builder"),

      # STEP 3 — Run
      div(class = "se-sec", "③ Analyse"),
      uiOutput("filter_ui"),
      actionButton("run_btn", "💬  Run sentiment analysis",
                   class = "se-run-btn btn-primary"),

      # STEP 4 — Download
      div(class = "se-sec", "④ Download results"),
      uiOutput("download_buttons")
    ),

    # ── Main panel ──────────────────────────────────────────
    div(class = "se-main",
      uiOutput("lexicon_error"),
      uiOutput("welcome_or_results")
    )
  ),

  # ── Footer ────────────────────────────────────────────────
  div(class = "se-footer",
    span("SentimentExplorer · LADAL · University of Queensland"),
    tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
    tags$a("Sentiment Analysis Tutorial",
           href = "https://ladal.edu.au/tutorials/sentiment/sentiment.html"),
    tags$a("Cite this tool",
           href = "https://ladal.edu.au/about.html#citing")
  )
)

# ── Server ────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Lexicon error banner ─────────────────────────────────
  output$lexicon_error <- renderUI({
    if (is.null(NRC)) {
      div(class = "se-error",
        tags$b("⚠ NRC lexicon not found."), br(),
        "The file ", tags$code("nrc_lexicon.csv"), " is missing from the
         app directory. Please run ", tags$code("prepare_nrc.R"),
        " to generate it from the raw NRC lexicon file.", br(),
        tags$a("Download the NRC lexicon",
               href = "https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",
               target = "_blank"),
        " (free for research use — commercial use requires permission)."
      )
    }
  })

  # ── Upload status ────────────────────────────────────────
  output$upload_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "se-warn", "⚠ No files uploaded yet.")
    } else {
      n <- nrow(input$files)
      div(class = "se-ok",
          paste0("✔ ", n, " file", if (n != 1) "s", " loaded"))
    }
  })

  # ── Section builder ──────────────────────────────────────
  # One text input per uploaded file, pre-filled with the file stem.
  # User can type matching names to group files into the same section.
  output$section_builder <- renderUI({
    req(input$files)
    files <- input$files

    rows <- lapply(seq_len(nrow(files)), function(i) {
      stem    <- tools::file_path_sans_ext(files$name[i])
      input_id <- paste0("section_", i)
      div(class = "se-section-row",
        div(class = "se-section-label",
            paste0("📄 ", files$name[i])),
        textInput(input_id,
                  label       = "Section name",
                  value       = stem,
                  placeholder = "Type a name to group files together")
      )
    })

    tagList(
      div(class = "se-info", style = "margin-bottom:10px;",
        "Give files the ", tags$b("same section name"),
        " to merge them into one section."
      ),
      rows
    )
  })

  # ── Category filter ──────────────────────────────────────
  output$filter_ui <- renderUI({
    checkboxGroupInput(
      "categories",
      label    = "Categories to display",
      choices  = setNames(NRC_CATEGORIES,
                          c("Anger","Anticipation","Disgust","Fear",
                            "Joy","Sadness","Surprise","Trust",
                            "Negative","Positive")),
      selected = NRC_CATEGORIES,
      inline   = TRUE
    )
  })

  # ── Core reactive: run analysis ──────────────────────────
  results <- eventReactive(input$run_btn, {
    req(input$files, !is.null(NRC))

    files <- input$files
    n     <- nrow(files)

    # Collect section names from dynamic inputs
    section_names <- vapply(seq_len(n), function(i) {
      val <- trimws(input[[paste0("section_", i)]] %||% "")
      if (nchar(val) == 0)
        tools::file_path_sans_ext(files$name[i])
      else
        val
    }, character(1))

    # Read and merge files by section
    section_map <- split(seq_len(n), section_names)

    annotated_list <- lapply(names(section_map), function(sec) {
      idx   <- section_map[[sec]]
      texts <- vapply(idx, function(i) {
        paste(readLines(files$datapath[i], warn = FALSE), collapse = " ")
      }, character(1))
      combined <- paste(texts, collapse = " ")
      words    <- tokenise(combined)
      annotate_section(words, sec, NRC)
    })

    annotated <- bind_rows(annotated_list)

    # Order sections as the user defined them (first occurrence)
    sec_order <- unique(section_names)
    annotated$section <- factor(annotated$section, levels = sec_order)

    list(
      annotated  = annotated,
      summary    = build_summary(annotated),
      sec_order  = sec_order
    )
  })

  # ── Welcome / results area ───────────────────────────────
  output$welcome_or_results <- renderUI({

    if (input$run_btn == 0 || is.null(input$files)) {
      return(
        div(class = "se-welcome",
          div(class = "se-welcome-icon", "💬"),
          tags$h3("Explore emotions in your texts"),
          tags$p(
            "Upload plain-text files, assign them to named sections,
             and click ", tags$b("Run sentiment analysis"), ".", br(), br(),
            "The tool annotates every word using the ",
            tags$a("NRC Word-Emotion Association Lexicon",
                   href = "https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",
                   target = "_blank"),
            " (Mohammad & Turney, 2013) — tracking ",
            tags$b("8 emotions"), " and ",
            tags$b("positive/negative sentiment"), ".", br(), br(),
            "Results include a per-token annotation table, a
             section-level summary, and a bar chart.", br(), br(),
            tags$em(style = "font-size:.82rem;",
              "Please cite the NRC lexicon in any published work
               — see the Citation tab for details.")
          )
        )
      )
    }

    res  <- results()
    cats <- input$categories %||% NRC_CATEGORIES
    n_sec <- length(res$sec_order)
    n_tok <- nrow(res$annotated)
    n_ann <- sum(rowSums(res$annotated[, NRC_CATEGORIES]) > 0)

    tagList(

      # ── Stat chips ──────────────────────────────────
      div(class = "se-chips",
        div(class = "se-chip",
          tags$b(n_sec), " section", if (n_sec != 1) "s"),
        div(class = "se-chip",
          tags$b(format(n_tok, big.mark = ",")), " tokens"),
        div(class = "se-chip",
          tags$b(format(n_ann, big.mark = ",")),
          " annotated (",
          round(n_ann / n_tok * 100, 1), "%)")
      ),

      # ── Tabs ─────────────────────────────────────────
      tabsetPanel(
        tabPanel("📊 Bar chart",
          br(),
          div(class = "se-plot-wrap",
            plotOutput("emotion_plot",
                       height = paste0(max(300, 180 + n_sec * 55), "px")),
            br(),
            uiOutput("plot_dl_buttons")
          )
        ),
        tabPanel("📋 Summary table",
          br(),
          uiOutput("summary_dl_buttons"),
          br(),
          DTOutput("summary_dt")
        ),
        tabPanel("🔤 Token table",
          br(),
          div(class = "se-info",
            "One row per token. Columns show whether each word is
             associated with each NRC category (1 = yes, 0 = no).
             Words not in the lexicon have all zeros."
          ),
          uiOutput("token_dl_buttons"),
          br(),
          DTOutput("token_dt")
        ),
        tabPanel("ℹ️ Citation",
          br(),
          div(class = "se-citation",
            tags$h4(style = paste0("color:", LADAL_PURPLE,
                                   "; font-family:'Source Serif 4',serif;"),
                    "NRC Word-Emotion Association Lexicon"),
            tags$p(
              "This tool uses the NRC Word-Emotion Association Lexicon
               (EmoLex). Please cite it in any published work:"
            ),
            tags$blockquote(style = "border-left:3px solid #c8b8de;
                                     padding-left:12px; color:#555;",
              "Mohammad, S.M. & Turney, P.D. (2013). Crowdsourcing a
               Word-Emotion Association Lexicon.",
              tags$em("Computational Intelligence,"),
              " 29(3): 436–465.",
              tags$a("https://doi.org/10.1111/j.1467-8640.2012.00460.x",
                     href = "https://doi.org/10.1111/j.1467-8640.2012.00460.x",
                     target = "_blank")
            ),
            tags$p(tags$b("BibTeX:")),
            tags$pre(
              style = "background:#ece8f5; border-radius:5px;
                       padding:10px; font-size:.78rem; overflow-x:auto;",
'@article{mohammad13,
  author  = {Mohammad, Saif M. and Turney, Peter D.},
  title   = {Crowdsourcing a Word-Emotion Association Lexicon},
  journal = {Computational Intelligence},
  volume  = {29},
  number  = {3},
  pages   = {436--465},
  year    = {2013},
  doi     = {10.1111/j.1467-8640.2012.00460.x}
}'
            ),
            tags$hr(),
            tags$p(style = "color:#888; font-size:.8rem;",
              tags$b("License note:"),
              " The NRC lexicon is free for research and educational use.
               Commercial use requires permission from Saif M. Mohammad
               (saif.mohammad@nrc-cnrc.gc.ca)."
            ),
            tags$hr(),
            tags$h4(style = paste0("color:", LADAL_PURPLE,
                                   "; font-family:'Source Serif 4',serif;
                                    margin-top:14px;"),
                    "Cite this tool"),
            tags$blockquote(style = "border-left:3px solid #c8b8de;
                                     padding-left:12px; color:#555;",
              "Schweinberger, Martin. (2024).",
              tags$em("SentimentExplorer: A browser-based sentiment
                       analysis tool."),
              " Brisbane: The University of Queensland.
               Language Technology and Data Analysis Laboratory (LADAL).
               Retrieved from https://ladal.edu.au/tools.html"
            )
          )
        )
      )
    )
  })

  # ── Bar chart ────────────────────────────────────────────
  build_plot <- reactive({
    req(results())
    res  <- results()
    cats <- input$categories %||% NRC_CATEGORIES

    long <- summary_long(res$summary) |>
      filter(category %in% cats) |>
      mutate(
        section  = factor(section, levels = res$sec_order),
        category = factor(category, levels = cats),
        fill_col = EMOTION_COLOURS[as.character(category)],
        label    = paste0(count, "\n(", percent, "%)")
      )

    n_sec <- length(res$sec_order)
    n_cat <- length(cats)

    ggplot(long,
           aes(x = category, y = percent,
               fill = category, label = label)) +
      geom_col(width = 0.7, colour = "white", linewidth = .3) +
      geom_text(aes(y = percent + max(percent) * 0.03),
                vjust = 0, size = 2.8, colour = "gray30",
                lineheight = .9) +
      facet_wrap(~ section, ncol = min(n_sec, 3)) +
      scale_fill_manual(values = EMOTION_COLOURS, guide = "none") +
      scale_y_continuous(
        labels   = function(x) paste0(x, "%"),
        expand   = expansion(mult = c(0, 0.18))
      ) +
      labs(
        title    = "NRC Emotion & Sentiment by Section",
        subtitle = "Percentage of tokens matching each category · bar labels show count and %",
        x        = NULL,
        y        = "% of tokens"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title      = element_text(colour = LADAL_PURPLE,
                                       face = "bold", size = 13),
        plot.subtitle   = element_text(colour = "#777", size = 9.5),
        strip.text      = element_text(colour = LADAL_PURPLE,
                                       face = "bold", size = 10),
        strip.background = element_rect(fill = "#f4f0f8", colour = NA),
        axis.text.x     = element_text(angle = 35, hjust = 1,
                                       size = 9, colour = "#444"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank()
      )
  })

  output$emotion_plot <- renderPlot({
    build_plot()
  }, bg = "white")

  # ── Plot download buttons ─────────────────────────────────
  output$plot_dl_buttons <- renderUI({
    req(results())
    tagList(
      downloadButton("dl_plot_png", "⬇ PNG", class = "se-dl-btn"),
      downloadButton("dl_plot_pdf", "⬇ PDF", class = "se-dl-btn")
    )
  })

  plot_dims <- reactive({
    req(results())
    n_sec <- length(results()$sec_order)
    n_cat <- length(input$categories %||% NRC_CATEGORIES)
    list(w = min(3, n_sec) * 4 + 1,
         h = ceiling(n_sec / 3) * 3.5 + 1.5)
  })

  output$dl_plot_png <- downloadHandler(
    filename = function()
      paste0("sentimentexplorer_", Sys.Date(), ".png"),
    content = function(file) {
      d <- plot_dims()
      ggplot2::ggsave(file, plot = build_plot(),
                      device = "png",
                      width = d$w, height = d$h,
                      dpi = 180, bg = "white")
    }
  )

  output$dl_plot_pdf <- downloadHandler(
    filename = function()
      paste0("sentimentexplorer_", Sys.Date(), ".pdf"),
    content = function(file) {
      d <- plot_dims()
      ggplot2::ggsave(file, plot = build_plot(),
                      device = "pdf",
                      width = d$w, height = d$h,
                      bg = "white")
    }
  )

  # ── Summary table ─────────────────────────────────────────
  output$summary_dt <- renderDT({
    req(results())
    res  <- results()
    cats <- input$categories %||% NRC_CATEGORIES

    # Build a display-friendly summary with interleaved n / % columns
    df <- res$summary

    # Select only requested categories
    keep_n   <- paste0(cats, "_n")
    keep_pct <- paste0(cats, "_pct")
    col_order <- c("section", "Total_tokens",
                   as.vector(rbind(keep_n, keep_pct)))
    col_order <- col_order[col_order %in% names(df)]
    df <- df[, col_order, drop = FALSE]

    # Prettier column names
    names(df) <- gsub("_n$",   " (n)",   names(df))
    names(df) <- gsub("_pct$", " (%)",   names(df))
    names(df)[1] <- "Section"
    names(df)[2] <- "Total tokens"

    datatable(
      df,
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 20,
        scrollX    = TRUE
      )
    ) |>
    formatStyle("Section",
                color = LADAL_PURPLE, fontWeight = "bold") |>
    formatStyle(grep("\\(%\\)$", names(df), value = TRUE),
                color = "#555")
  })

  output$summary_dl_buttons <- renderUI({
    req(results())
    tagList(
      downloadButton("dl_sum_xlsx", "⬇ Excel (.xlsx)", class = "se-dl-btn"),
      downloadButton("dl_sum_csv",  "⬇ CSV (.csv)",    class = "se-dl-btn")
    )
  })

  output$dl_sum_xlsx <- downloadHandler(
    filename = function()
      paste0("sentiment_summary_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(as.data.frame(results()$summary), file)
  )
  output$dl_sum_csv <- downloadHandler(
    filename = function()
      paste0("sentiment_summary_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(results()$summary, file)
  )

  # ── Token table ───────────────────────────────────────────
  output$token_dt <- renderDT({
    req(results())
    res  <- results()
    cats <- input$categories %||% NRC_CATEGORIES

    display <- res$annotated |>
      select(Section = section,
             `Token #` = token_index,
             Word = word,
             all_of(cats))

    datatable(
      display,
      rownames   = FALSE,
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 50,
        scrollX    = TRUE
      )
    ) |>
    formatStyle("Word",
                fontFamily = "monospace", color = "#333") |>
    formatStyle(
      cats[cats %in% names(display)],
      backgroundColor = styleEqual(c(0, 1), c("white", "#eafaf1")),
      color           = styleEqual(c(0, 1), c("#ccc",  "#1a6b3c")),
      fontWeight      = styleEqual(c(0, 1), c("400",   "700"))
    )
  })

  output$token_dl_buttons <- renderUI({
    req(results())
    tagList(
      downloadButton("dl_tok_xlsx", "⬇ Excel (.xlsx)", class = "se-dl-btn"),
      downloadButton("dl_tok_csv",  "⬇ CSV (.csv)",    class = "se-dl-btn")
    )
  })

  output$dl_tok_xlsx <- downloadHandler(
    filename = function()
      paste0("sentiment_tokens_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(as.data.frame(results()$annotated), file)
  )
  output$dl_tok_csv <- downloadHandler(
    filename = function()
      paste0("sentiment_tokens_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(results()$annotated, file)
  )

  # ── Sidebar download buttons (shown after analysis) ───────
  output$download_buttons <- renderUI({
    if (input$run_btn == 0 || is.null(results()))
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Run analysis to enable downloads."))
    div(class = "se-info", style = "font-size:.81rem;",
      "Download buttons appear on each results tab.")
  })
}

# ── Run ───────────────────────────────────────────────────────
shinyApp(ui, server)

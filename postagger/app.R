# ============================================================
#  POSTagger — LADAL POS Tagging & Dependency Parsing Tool
#  https://ladal.edu.au
#
#  Uses udpipe for tokenisation, POS tagging, lemmatisation
#  and dependency parsing. Pre-bundled models for 11 common
#  languages; all others downloaded on first use.
# ============================================================

library(shiny)
library(udpipe)
library(tidyverse)
library(writexl)
library(DT)

# ══════════════════════════════════════════════════════════════
#  CONSTANTS & MODEL CATALOGUE
# ══════════════════════════════════════════════════════════════

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

# Model directory — pre-bundled models live here inside Binder.
# postBuild downloads to ~/udpipe-models (i.e. /home/jovyan/udpipe-models).
# Resolve HOME explicitly — path.expand() can fail in some Binder envs.
.home <- Sys.getenv("HOME", unset = "/home/jovyan")
MODEL_DIRS <- c(
  file.path(.home, "udpipe-models"),
  "/home/jovyan/udpipe-models",
  "/srv/udpipe-models",
  getwd()
)

# Full model list from udpipe (language → display label)
ALL_MODELS <- c(
  # ── Pre-bundled ─────────────────────────────────────────────
  "arabic-padt"            = "Arabic (PADT)",
  "chinese-gsd"            = "Chinese (GSD)",
  "chinese-gsdsimp"        = "Chinese simplified (GSD)",
  "dutch-alpino"           = "Dutch (Alpino)",
  "dutch-lassysmall"       = "Dutch (LassySmall)",
  "english-ewt"            = "English (EWT) ★",
  "english-gum"            = "English (GUM)",
  "english-lines"          = "English (Lines)",
  "english-partut"         = "English (ParTUT)",
  "french-gsd"             = "French (GSD)",
  "french-partut"          = "French (ParTUT)",
  "french-sequoia"         = "French (Sequoia)",
  "french-spoken"          = "French (Spoken)",
  "german-gsd"             = "German (GSD)",
  "german-hdt"             = "German (HDT)",
  "italian-isdt"           = "Italian (ISDT)",
  "italian-partut"         = "Italian (ParTUT)",
  "italian-postwita"       = "Italian (PoSTWITA)",
  "italian-twittiro"       = "Italian (Twittiro)",
  "italian-vit"            = "Italian (VIT)",
  "japanese-gsd"           = "Japanese (GSD)",
  "portuguese-bosque"      = "Portuguese (Bosque)",
  "portuguese-br"          = "Portuguese BR (GSD)",
  "portuguese-gsd"         = "Portuguese (GSD)",
  "russian-gsd"            = "Russian (GSD)",
  "russian-syntagrus"      = "Russian (SynTagRus)",
  "russian-taiga"          = "Russian (Taiga)",
  "spanish-ancora"         = "Spanish (AnCora)",
  "spanish-gsd"            = "Spanish (GSD)",
  # ── Other (download on first use) ───────────────────────────
  "afrikaans-afribooms"    = "Afrikaans (Afribooms)",
  "ancient_greek-perseus"  = "Ancient Greek (Perseus)",
  "ancient_greek-proiel"   = "Ancient Greek (PROIEL)",
  "armenian-armtdp"        = "Armenian (ArmTDP)",
  "basque-bdt"             = "Basque (BDT)",
  "belarusian-hse"         = "Belarusian (HSE)",
  "bulgarian-btb"          = "Bulgarian (BTB)",
  "buryat-bdt"             = "Buryat (BDT)",
  "catalan-ancora"         = "Catalan (AnCora)",
  "classical_chinese-kyoto"= "Classical Chinese (Kyoto)",
  "coptic-scriptorium"     = "Coptic (Scriptorium)",
  "croatian-set"           = "Croatian (SET)",
  "czech-cac"              = "Czech (CAC)",
  "czech-cltt"             = "Czech (CLTT)",
  "czech-fictree"          = "Czech (FicTree)",
  "czech-pdt"              = "Czech (PDT)",
  "danish-ddt"             = "Danish (DDT)",
  "estonian-edt"           = "Estonian (EDT)",
  "estonian-ewt"           = "Estonian (EWT)",
  "finnish-ftb"            = "Finnish (FTB)",
  "finnish-tdt"            = "Finnish (TDT)",
  "galician-ctg"           = "Galician (CTG)",
  "galician-treegal"       = "Galician (TreeGal)",
  "gothic-proiel"          = "Gothic (PROIEL)",
  "greek-gdt"              = "Greek (GDT)",
  "hebrew-htb"             = "Hebrew (HTB)",
  "hindi-hdtb"             = "Hindi (HDTB)",
  "hungarian-szeged"       = "Hungarian (Szeged)",
  "indonesian-gsd"         = "Indonesian (GSD)",
  "irish-idt"              = "Irish (IDT)",
  "kazakh-ktb"             = "Kazakh (KTB)",
  "korean-gsd"             = "Korean (GSD)",
  "korean-kaist"           = "Korean (KAIST)",
  "kurmanji-mg"            = "Kurmanji (MG)",
  "latin-ittb"             = "Latin (ITTB)",
  "latin-perseus"          = "Latin (Perseus)",
  "latin-proiel"           = "Latin (PROIEL)",
  "latvian-lvtb"           = "Latvian (LVTB)",
  "lithuanian-alksnis"     = "Lithuanian (ALKSNIS)",
  "lithuanian-hse"         = "Lithuanian (HSE)",
  "maltese-mudt"           = "Maltese (MUDT)",
  "marathi-ufal"           = "Marathi (UFAL)",
  "north_sami-giella"      = "North Sami (Giella)",
  "norwegian-bokmaal"      = "Norwegian Bokmål",
  "norwegian-nynorsk"      = "Norwegian Nynorsk",
  "norwegian-nynorsklia"   = "Norwegian Nynorsk (LIA)",
  "old_church_slavonic-proiel" = "Old Church Slavonic",
  "old_french-srcmf"       = "Old French (SRCMF)",
  "old_russian-torot"      = "Old Russian (TOROT)",
  "persian-seraji"         = "Persian (Seraji)",
  "polish-lfg"             = "Polish (LFG)",
  "polish-pdb"             = "Polish (PDB)",
  "polish-sz"              = "Polish (SZ)",
  "romanian-nonstandard"   = "Romanian (Nonstandard)",
  "romanian-rrt"           = "Romanian (RRT)",
  "sanskrit-ufal"          = "Sanskrit (UFAL)",
  "scottish_gaelic-arcosg" = "Scottish Gaelic (ARCOSG)",
  "serbian-set"            = "Serbian (SET)",
  "slovak-snk"             = "Slovak (SNK)",
  "slovenian-ssj"          = "Slovenian (SSJ)",
  "slovenian-sst"          = "Slovenian (SST)",
  "swedish-lines"          = "Swedish (Lines)",
  "swedish-talbanken"      = "Swedish (Talbanken)",
  "tamil-ttb"              = "Tamil (TTB)",
  "telugu-mtg"             = "Telugu (MTG)",
  "turkish-imst"           = "Turkish (IMST)",
  "ukrainian-iu"           = "Ukrainian (IU)",
  "upper_sorbian-ufal"     = "Upper Sorbian (UFAL)",
  "urdu-udtb"              = "Urdu (UDTB)",
  "uyghur-udt"             = "Uyghur (UDT)",
  "vietnamese-vtb"         = "Vietnamese (VTB)",
  "wolof-wtb"              = "Wolof (WTB)"
)

# Pre-bundled model keys (downloaded at Binder build time via postBuild)
BUNDLED_MODELS <- c(
  "arabic-padt", "chinese-gsd", "chinese-gsdsimp",
  "dutch-alpino", "dutch-lassysmall",
  "english-ewt", "english-gum", "english-lines", "english-partut",
  "french-gsd", "french-partut", "french-sequoia", "french-spoken",
  "german-gsd", "german-hdt",
  "italian-isdt", "italian-partut", "italian-postwita",
  "italian-twittiro", "italian-vit",
  "japanese-gsd",
  "portuguese-bosque", "portuguese-br", "portuguese-gsd",
  "russian-gsd", "russian-syntagrus", "russian-taiga",
  "spanish-ancora", "spanish-gsd"
)

# ── Model loading with cache ────────────────────────────────────
model_cache <- new.env(hash = TRUE, parent = emptyenv())

# Search all MODEL_DIRS for a .udpipe file matching the language key.
find_model_file <- function(language) {
  # Build search dirs: MODEL_DIRS plus one level of subdirectories
  all_dirs <- unique(c(
    MODEL_DIRS,
    unlist(lapply(MODEL_DIRS[dir.exists(MODEL_DIRS)], function(d)
      list.dirs(d, recursive = FALSE, full.names = TRUE)
    ))
  ))
  
  for (d in all_dirs) {
    if (!dir.exists(d)) next
    # Match "english-ewt-ud-2.5-191206.udpipe" style filenames
    candidates <- list.files(d,
                             pattern     = paste0("^", language, ".*\.udpipe$"),
                             full.names  = TRUE,
                             ignore.case = TRUE)
    if (length(candidates) > 0) {
      message("Found model '", language, "' at: ", candidates[1])
      return(candidates[1])
    }
  }
  message("No pre-downloaded model for '", language, "'. Dirs searched: ",
          paste(all_dirs[dir.exists(all_dirs)], collapse = ", "))
  NULL
}

get_model <- function(language) {
  # Return cached model if available
  if (exists(language, envir = model_cache)) {
    return(get(language, envir = model_cache))
  }
  
  # Try to find a pre-downloaded model file first
  model_file <- find_model_file(language)
  
  # Fall back to on-demand download into the user's home udpipe-models dir
  if (is.null(model_file)) {
    dl_dir <- path.expand("~/udpipe-models")
    dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)
    
    dl <- tryCatch(
      udpipe_download_model(language = language, model_dir = dl_dir),
      error = function(e) {
        stop("Could not download model '", language, "': ", conditionMessage(e),
             "\nCheck your internet connection or try a pre-bundled language.")
      }
    )
    
    if (isTRUE(dl$download_failed)) {
      stop("Model download failed for '", language, "': ",
           dl$download_message,
           "\nTry a pre-bundled language (marked ★) or check your connection.")
    }
    model_file <- dl$file_model
  }
  
  m <- udpipe_load_model(model_file)
  assign(language, m, envir = model_cache)
  m
}

# ══════════════════════════════════════════════════════════════
#  TAGGING ENGINE
# ══════════════════════════════════════════════════════════════

tag_texts <- function(file_df, language) {
  model <- get_model(language)
  
  results <- purrr::imap(
    setNames(file_df$datapath, file_df$name),
    function(path, fname) {
      txt <- paste(readLines(path, warn = FALSE), collapse = " ")
      txt <- iconv(txt, to = "UTF-8", sub = "byte")
      
      ann <- udpipe_annotate(model, x = txt,
                             doc_id = tools::file_path_sans_ext(fname))
      as.data.frame(ann, detailed = TRUE)
    }
  )
  
  dplyr::bind_rows(results)
}

# ── Build tidy table ───────────────────────────────────────────
build_tidy <- function(tagged_df) {
  tagged_df |>
    dplyr::select(
      doc_id,
      sentence_id,
      token_id,
      token,
      lemma,
      upos,
      xpos,
      dep_rel,
      head_token_id
    ) |>
    dplyr::rename(
      Document    = doc_id,
      Sentence    = sentence_id,
      TokenID     = token_id,
      Token       = token,
      Lemma       = lemma,
      UPOS        = upos,
      XPOS        = xpos,
      DepRel      = dep_rel,
      HeadTokenID = head_token_id
    )
}

# ── Build annotated text (word_TAG format) ─────────────────────
build_annotated_texts <- function(tagged_df) {
  tagged_df |>
    dplyr::filter(!is.na(token), !is.na(upos)) |>
    dplyr::group_by(doc_id) |>
    dplyr::summarise(
      text = paste(paste0(token, "_", upos), collapse = " "),
      .groups = "drop"
    )
}

# ══════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════

ui <- fluidPage(
  title = "POSTagger | LADAL",
  
  tags$head(tags$style(HTML(paste0("
    body { font-family:'Segoe UI',Arial,sans-serif;
           background:#f7f4fb; color:#222; margin:0; }

    /* Banner */
    .ud-banner {
      background:", LADAL_PURPLE, ";
      color:white; padding:18px 32px 14px 32px;
      display:flex; align-items:center; gap:18px;
      border-bottom:4px solid ", LADAL_GOLD, ";
    }
    .ud-banner .ud-title { font-size:1.7rem; font-weight:700;
                            letter-spacing:.5px; margin:0; }
    .ud-banner .ud-sub   { font-size:.88rem; opacity:.85; margin:2px 0 0 0; }

    /* Layout */
    .ud-body { display:flex; min-height:calc(100vh - 80px); }
    .ud-side  { width:320px; min-width:270px; max-width:350px;
                background:white;
                border-right:1px solid #e0d8ec;
                padding:22px 20px 30px 20px;
                box-shadow:2px 0 8px rgba(81,36,122,.06); }
    .ud-main  { flex:1; padding:24px 28px; overflow-x:auto; }

    /* Section headings */
    .ud-sec {
      font-size:.75rem; font-weight:700; letter-spacing:1.2px;
      text-transform:uppercase; color:", LADAL_PURPLE, ";
      border-bottom:2px solid ", LADAL_GOLD, ";
      padding-bottom:4px; margin:20px 0 10px 0;
    }
    .ud-sec:first-child { margin-top:0; }

    /* Inputs */
    .form-control, .selectize-input {
      border:1.5px solid #d0c8e0 !important;
      border-radius:6px !important; font-size:.92rem !important;
    }
    label { font-size:.88rem; font-weight:600; color:#444; }

    /* Run button */
    #run_tag {
      width:100%; background:", LADAL_PURPLE, " !important;
      border:none !important; color:white !important;
      font-weight:700; font-size:1rem; padding:10px;
      border-radius:7px; margin-top:6px; transition:background .2s;
    }
    #run_tag:hover { background:#3a1860 !important; }

    /* Upload */
    .shiny-input-container .btn {
      background:white; border:1.5px dashed ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; width:100%;
    }

    /* Info boxes */
    .ud-info {
      background:#f4f0f8; border-left:4px solid ", LADAL_PURPLE, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#444; margin-bottom:12px;
    }
    .ud-warn {
      background:#fff4e5; border-left:4px solid ", LADAL_GOLD, ";
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#6b4000; margin-bottom:10px;
    }
    .ud-ok {
      background:#eafaf1; border-left:4px solid #27ae60;
      border-radius:5px; padding:9px 13px; font-size:.84rem;
      color:#1a6b3c; margin-bottom:8px;
    }
    .ud-download-note {
      background:#f0edff; border-left:4px solid #8e44ad;
      border-radius:5px; padding:8px 13px; font-size:.82rem;
      color:#4a2080; margin-bottom:8px;
    }

    /* Stat cards */
    .ud-stats { display:flex; gap:12px; margin-bottom:20px;
                flex-wrap:wrap; }
    .ud-card  { background:white; border-radius:9px;
                border-left:4px solid ", LADAL_PURPLE, ";
                padding:11px 16px; min-width:110px;
                box-shadow:0 1px 6px rgba(81,36,122,.08); }
    .ud-card .ud-val { font-size:1.5rem; font-weight:700;
                       color:", LADAL_PURPLE, "; line-height:1.1; }
    .ud-card .ud-lbl { font-size:.76rem; color:#888; margin-top:2px; }

    /* Download buttons */
    .ud-dl {
      display:inline-block; margin:4px 5px 4px 0;
      background:white; border:1.5px solid ", LADAL_PURPLE, ";
      color:", LADAL_PURPLE, "; font-weight:600; font-size:.84rem;
      padding:6px 14px; border-radius:6px; cursor:pointer;
      text-decoration:none; transition:all .15s;
    }
    .ud-dl:hover { background:", LADAL_PURPLE, "; color:white; }

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

    /* UPOS colour coding */
    .upos-NOUN  { color:#1a6b3c; font-weight:600; }
    .upos-VERB  { color:#1a3a6b; font-weight:600; }
    .upos-ADJ   { color:#6b1a1a; font-weight:600; }
    .upos-ADV   { color:#6b4a1a; font-weight:600; }
    .upos-PROPN { color:#4a1a6b; font-weight:600; }

    /* Footer */
    .ud-footer {
      background:#2d1a4a; color:#c8b8de;
      font-size:.78rem; padding:12px 32px;
      display:flex; gap:18px; align-items:center;
    }
    .ud-footer a { color:#d4b8f5; }

    /* Pre-bundled badge */
    .badge-bundled {
      background:#eafaf1; color:#1a6b3c;
      font-size:.72rem; padding:1px 6px; border-radius:10px;
      font-weight:700; margin-left:4px;
    }
  ")))),
  
  # ── Banner ────────────────────────────────────────────────────
  div(class = "ud-banner",
      div(style = "font-size:2rem;", "🏷️"),
      div(
        p(class = "ud-title", "POSTagger"),
        p(class = "ud-sub",
          "POS tagging & dependency parsing · 65+ languages · ",
          tags$a("LADAL", href = "https://ladal.edu.au",
                 style = "color:#f0c060;"))
      )
  ),
  
  # ── Body ──────────────────────────────────────────────────────
  div(class = "ud-body",
      
      # ── Sidebar ─────────────────────────────────────────────────
      div(class = "ud-side",
          
          # STEP 1
          div(class = "ud-sec", "① Upload texts"),
          div(class = "ud-info",
              "Upload one or more ", tags$b(".txt"), " files.
         Each file is tagged and kept as a separate document."),
          fileInput("files", NULL,
                    multiple    = TRUE,
                    accept      = ".txt",
                    buttonLabel = "📂 Choose .txt files"),
          uiOutput("corpus_status"),
          
          # STEP 2
          div(class = "ud-sec", "② Language model"),
          div(class = "ud-info",
              "Models marked ", tags$b("★"), " are pre-loaded and start
         instantly. Others download automatically on first use
         (typically 10–30 seconds)."),
          selectInput("language", "Language / treebank",
                      choices  = ALL_MODELS,
                      selected = "english-ewt"),
          uiOutput("model_status"),
          
          # STEP 3
          div(class = "ud-sec", "③ Tag"),
          actionButton("run_tag", "🏷️  Tag texts",
                       class = "btn-primary"),
          
          # STEP 4
          div(class = "ud-sec", "④ Download results"),
          div(class = "ud-download-note",
              "Both output formats are always available after tagging."),
          uiOutput("download_buttons")
      ),
      
      # ── Main panel ───────────────────────────────────────────────
      div(class = "ud-main",
          uiOutput("welcome_box"),
          uiOutput("stats_cards"),
          uiOutput("results_ui")
      )
  ),
  
  # ── Footer ───────────────────────────────────────────────────
  div(class = "ud-footer",
      span("POSTagger · LADAL · University of Queensland"),
      tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
      tags$a("POS Tagging Tutorial",
             href = "https://ladal.edu.au/tutorials/postag/postag.html"),
      tags$a("Universal Dependencies",
             href = "https://universaldependencies.org"),
      tags$a("Cite this tool",
             href = "https://ladal.edu.au/about.html#citing")
  )
)

# ══════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── Corpus status ────────────────────────────────────────────
  output$corpus_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "ud-warn", "⚠ No files uploaded yet.")
    } else {
      n   <- nrow(input$files)
      lbl <- if (n == 1) "1 file loaded" else paste(n, "files loaded")
      div(class = "ud-ok",
          paste0("✔ ", lbl, ": ",
                 paste(tools::file_path_sans_ext(input$files$name),
                       collapse = ", ")))
    }
  })
  
  # ── Model status badge ───────────────────────────────────────
  output$model_status <- renderUI({
    lang <- input$language
    # Check whether the model file is already on disk
    model_file <- find_model_file(lang)
    if (!is.null(model_file) || lang %in% BUNDLED_MODELS) {
      div(class = "ud-ok",
          "✔ Pre-loaded model — starts instantly")
    } else {
      div(class = "ud-warn",
          "⏳ This model will be downloaded on first use (~10–30s)")
    }
  })
  
  # ── Core computation ─────────────────────────────────────────
  tagged <- eventReactive(input$run_tag, {
    req(input$files)
    
    withProgress(message = "Tagging…", value = 0, {
      incProgress(0.2, detail = paste("Loading model:", input$language))
      
      result <- tryCatch({
        tag_texts(input$files, input$language)
      }, error = function(e) {
        showNotification(paste("Error:", e$message),
                         type = "error", duration = 15)
        NULL
      })
      
      incProgress(0.8, detail = "Building outputs")
      result
    })
  })
  
  tidy_table <- reactive({
    req(tagged())
    build_tidy(tagged())
  })
  
  annotated_texts <- reactive({
    req(tagged())
    build_annotated_texts(tagged())
  })
  
  # ── Welcome box ──────────────────────────────────────────────
  output$welcome_box <- renderUI({
    if (input$run_tag == 0) {
      div(class = "ud-info", style = "font-size:.93rem;",
          tags$b("Welcome to POSTagger."), br(),
          "Upload your plain-text files, select a language model,
         and click ", tags$b("Tag texts"), " to perform
         tokenisation, POS tagging, lemmatisation, and dependency
         parsing using the ", tags$a("UDPipe",
                                     href = "https://ufal.mff.cuni.cz/udpipe"),
          " toolkit.", br(), br(),
          "Results are available in two formats: a ", tags$b("tidy table"),
          " (one row per token with all annotation columns), and ",
          tags$b("annotated text files"), " (word_UPOSTAG format
         ready for corpus tools or further processing).", br(), br(),
          tags$a("→ Learn more: POS Tagging Tutorial",
                 href = "https://ladal.edu.au/tutorials/postag/postag.html"), br(),
          tags$a("→ Universal Dependencies tag set",
                 href = "https://universaldependencies.org/u/pos/")
      )
    }
  })
  
  # ── Stat cards ───────────────────────────────────────────────
  output$stats_cards <- renderUI({
    req(input$run_tag > 0, tidy_table())
    tt <- tidy_table()
    
    n_docs   <- length(unique(tt$Document))
    n_sents  <- length(unique(paste(tt$Document, tt$Sentence)))
    n_tokens <- nrow(tt)
    n_upos   <- length(unique(tt$UPOS[!is.na(tt$UPOS)]))
    
    div(class = "ud-stats",
        div(class = "ud-card",
            div(class = "ud-val", n_docs),
            div(class = "ud-lbl", "Documents")),
        div(class = "ud-card",
            div(class = "ud-val", format(n_sents, big.mark = ",")),
            div(class = "ud-lbl", "Sentences")),
        div(class = "ud-card",
            div(class = "ud-val", format(n_tokens, big.mark = ",")),
            div(class = "ud-lbl", "Tokens")),
        div(class = "ud-card",
            div(class = "ud-val", n_upos),
            div(class = "ud-lbl", "UPOS categories"))
    )
  })
  
  # ── Results UI (tabs) ────────────────────────────────────────
  output$results_ui <- renderUI({
    req(input$run_tag > 0, tidy_table())
    
    tabsetPanel(
      tabPanel("📋 Tidy table",       br(), DTOutput("tidy_dt")),
      tabPanel("📊 POS summary",      br(), plotOutput("pos_plot",
                                                       height = "380px")),
      tabPanel("📄 Annotated texts",  br(), uiOutput("annotated_preview")),
      tabPanel("ℹ️ Tag guide",         br(), uiOutput("tag_guide"))
    )
  })
  
  # ── Tidy table ───────────────────────────────────────────────
  output$tidy_dt <- renderDT({
    tt <- tidy_table()
    req(nrow(tt) > 0)
    
    datatable(
      tt,
      rownames   = FALSE,
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("copy"),
        pageLength = 25,
        scrollX    = TRUE,
        autoWidth  = TRUE
      ),
      caption = htmltools::tags$caption(
        style = paste0("color:", LADAL_PURPLE, "; font-weight:bold;"),
        paste0("Annotation results — ",
               format(nrow(tt), big.mark = ","),
               " tokens · model: ", input$language)
      )
    ) |>
      formatStyle(
        "UPOS",
        color = styleEqual(
          c("NOUN", "VERB", "ADJ", "ADV", "PROPN",
            "DET",  "ADP",  "AUX", "CCONJ", "PUNCT"),
          c("#1a6b3c", "#1a3a6b", "#6b1a1a", "#6b4a1a", "#4a1a6b",
            "#555",    "#555",    "#555",    "#555",     "#999")
        ),
        fontWeight = styleEqual(
          c("NOUN","VERB","ADJ","ADV","PROPN"),
          rep("bold", 5)
        )
      )
  })
  
  # ── POS frequency bar chart ───────────────────────────────────
  output$pos_plot <- renderPlot({
    tt <- tidy_table()
    req(nrow(tt) > 0)
    
    pos_counts <- tt |>
      dplyr::filter(!is.na(UPOS), UPOS != "PUNCT", UPOS != "SPACE") |>
      dplyr::count(UPOS, sort = TRUE)
    
    upos_colours <- c(
      NOUN  = "#1a6b3c", VERB  = "#1a3a6b", ADJ   = "#6b1a1a",
      ADV   = "#6b4a1a", PROPN = "#4a1a6b", DET   = "#888888",
      ADP   = "#aaaaaa", AUX   = "#bbbbbb", CCONJ = "#cccccc",
      PRON  = "#dd8800", SCONJ = "#ddaaaa", NUM   = "#aaddaa",
      PART  = "#aaaadd", INTJ  = "#ddaadd", SYM   = "#ddddaa",
      X     = "#eeeeee", OTHER = "#dddddd"
    )
    
    pos_counts$fill_col <- upos_colours[pos_counts$UPOS]
    pos_counts$fill_col[is.na(pos_counts$fill_col)] <- "#dddddd"
    
    ggplot(pos_counts,
           aes(x = reorder(UPOS, n), y = n, fill = UPOS)) +
      geom_col(fill = pos_counts$fill_col[order(pos_counts$n)],
               width = 0.7) +
      geom_text(aes(label = format(n, big.mark = ",")),
                hjust = -0.1, size = 3.5, colour = "gray30") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title    = "Token frequency by UPOS category",
        subtitle = paste0("Model: ", input$language),
        x        = "Universal POS tag",
        y        = "Token count"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(color = LADAL_PURPLE, face = "bold"),
        plot.subtitle = element_text(color = "#666", size = 10),
        panel.grid.major.y = element_blank(),
        legend.position    = "none"
      )
  }, bg = "white")
  
  # ── Annotated text preview ───────────────────────────────────
  output$annotated_preview <- renderUI({
    at <- annotated_texts()
    req(nrow(at) > 0)
    
    doc_list <- purrr::pmap(at, function(doc_id, text) {
      preview <- substr(text, 1, 500)
      if (nchar(text) > 500) preview <- paste0(preview, "…")
      tagList(
        tags$h5(style = paste0("color:", LADAL_PURPLE,
                               "; font-weight:bold;"),
                paste0("📄 ", doc_id, "_postag.txt")),
        tags$pre(style = paste0(
          "background:#f8f6fb; border:1px solid #e0d8ec;",
          "border-radius:6px; padding:12px; font-size:.82rem;",
          "white-space:pre-wrap; word-break:break-all;",
          "max-height:200px; overflow-y:auto;"),
          preview),
        tags$hr()
      )
    })
    
    tagList(
      div(class = "ud-info",
          "Preview of annotated text files (word_UPOSTAG format). ",
          "Download the full files using the buttons in the sidebar."),
      doc_list
    )
  })
  
  # ── Tag guide ────────────────────────────────────────────────
  output$tag_guide <- renderUI({
    div(style = "max-width:750px;",
        tags$h4(style = paste0("color:", LADAL_PURPLE),
                "Universal POS Tags (UPOS)"),
        tags$p("POSTagger uses the",
               tags$a("Universal Dependencies",
                      href = "https://universaldependencies.org/u/pos/"),
               "tag set, which is consistent across all languages."),
        DT::renderDT(
          data.frame(
            Tag = c("NOUN","VERB","ADJ","ADV","PROPN","PRON",
                    "DET","ADP","AUX","CCONJ","SCONJ","NUM",
                    "PART","INTJ","PUNCT","SYM","X"),
            Description = c(
              "Noun (common noun)",
              "Verb (main verb)",
              "Adjective",
              "Adverb",
              "Proper noun (name)",
              "Pronoun",
              "Determiner (the, a, this)",
              "Adposition (preposition/postposition)",
              "Auxiliary verb (is, will, have)",
              "Coordinating conjunction (and, but, or)",
              "Subordinating conjunction (that, because, if)",
              "Numeral",
              "Particle (not, 's)",
              "Interjection (oh, wow, hmm)",
              "Punctuation",
              "Symbol (%, $, =)",
              "Other / foreign words / typos"
            )
          ),
          rownames = FALSE,
          options  = list(dom = "t", pageLength = 20)
        ),
        tags$h4(style = paste0("color:", LADAL_PURPLE, "; margin-top:20px;"),
                "Dependency Relations (DepRel)"),
        tags$p("Common Universal Dependencies relations:"),
        tags$ul(style = "font-size:.9rem; line-height:1.9;",
                tags$li(tags$b("nsubj"), " — nominal subject"),
                tags$li(tags$b("obj"), " — direct object"),
                tags$li(tags$b("iobj"), " — indirect object"),
                tags$li(tags$b("csubj"), " — clausal subject"),
                tags$li(tags$b("amod"), " — adjectival modifier"),
                tags$li(tags$b("advmod"), " — adverbial modifier"),
                tags$li(tags$b("det"), " — determiner"),
                tags$li(tags$b("case"), " — case marker (preposition)"),
                tags$li(tags$b("nmod"), " — nominal modifier"),
                tags$li(tags$b("conj"), " — conjunct"),
                tags$li(tags$b("root"), " — root of the sentence"),
                tags$li(tags$b("punct"), " — punctuation"),
                tags$li(tags$b("aux"), " — auxiliary"),
                tags$li(tags$b("cop"), " — copula (be)")
        ),
        tags$p(style = "color:#888; font-size:.83rem;",
               "Full list: ",
               tags$a("universaldependencies.org/u/dep/",
                      href = "https://universaldependencies.org/u/dep/"))
    )
  })
  
  # ── Download buttons ────────────────────────────────────────
  output$download_buttons <- renderUI({
    if (input$run_tag == 0 || is.null(tagged()))
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Tag texts to enable downloads."))
    
    tagList(
      tags$p(style = "font-size:.82rem; font-weight:600;
                       color:#444; margin-bottom:4px;",
             "Tidy table:"),
      downloadButton("dl_xlsx", "⬇ Excel (.xlsx)", class = "ud-dl"),
      downloadButton("dl_csv",  "⬇ CSV (.csv)",    class = "ud-dl"),
      tags$p(style = "font-size:.82rem; font-weight:600;
                       color:#444; margin:10px 0 4px 0;",
             "Annotated text files:"),
      downloadButton("dl_txts", "⬇ Tagged .txt files (.zip)",
                     class = "ud-dl")
    )
  })
  
  # ── Download: tidy table ─────────────────────────────────────
  output$dl_xlsx <- downloadHandler(
    filename = function()
      paste0("postagger_", input$language, "_", Sys.Date(), ".xlsx"),
    content  = function(file)
      writexl::write_xlsx(as.data.frame(tidy_table()), file)
  )
  
  output$dl_csv <- downloadHandler(
    filename = function()
      paste0("postagger_", input$language, "_", Sys.Date(), ".csv"),
    content  = function(file)
      readr::write_csv(tidy_table(), file)
  )
  
  # ── Download: annotated .txt files as ZIP ───────────────────
  output$dl_txts <- downloadHandler(
    filename = function()
      paste0("postagger_tagged_", Sys.Date(), ".zip"),
    content  = function(file) {
      at      <- annotated_texts()
      tmp_dir <- tempfile()
      dir.create(tmp_dir)
      
      txt_files <- purrr::pmap_chr(at, function(doc_id, text) {
        out_path <- file.path(tmp_dir,
                              paste0(doc_id, "_postag.txt"))
        writeLines(text, out_path)
        out_path
      })
      
      zip::zip(zipfile = file,
               files   = basename(txt_files),
               root    = tmp_dir)
    }
  )
}

# ══════════════════════════════════════════════════════════════
shinyApp(ui, server)
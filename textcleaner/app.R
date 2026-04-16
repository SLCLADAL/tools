# -*- coding: utf-8 -*-
# ============================================================
#  TextCleaner - LADAL Text Cleaning Tool (Shiny)
#  https://ladal.edu.au
#
#  Speed strategy:
#  - stringi for all regex ops (C-level ICU, fastest R string lib)
#  - data.table for tabular state management
#  - All operations applied as a single vectorised pass per file
#  - No loops over individual characters or tokens
# ============================================================

library(shiny)
library(stringi)      # C-level ICU regex - faster than gsub/stringr
library(data.table)   # fast tabular state
library(readr)        # fast file I/O
library(zip)          # ZIP download

# ==============================================================
#  CONSTANTS
# ==============================================================

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

# Pre-built removal options
# pattern = stringi / ICU regex
PREBUILT <- list(
  tags = list(
    id      = "tags",
    label   = "XML / HTML tags  (e.g. <b>, </p>, <br/>)",
    pattern = "<[^>]*>",
    desc    = "Removes all XML and HTML tags including attributes."
  ),
  nonalpha = list(
    id      = "nonalpha",
    label   = "Non-alphanumeric characters except spaces",
    pattern = "[^\\p{L}\\p{N} ]",
    desc    = "Keeps only letters, digits and spaces. Produces clean plain text."
  ),
  punct = list(
    id      = "punct",
    label   = "Punctuation  (e.g. . , ; : ! ? \" ')",
    pattern = "[^\\p{L}\\p{N}\\s]",
    desc    = "Removes all characters that are not letters, digits, or whitespace."
  ),
  numbers = list(
    id      = "numbers",
    label   = "Numbers  (e.g. 1, 42, 2024)",
    pattern = "\\d+",
    desc    = "Removes all digit sequences."
  ),
  urls = list(
    id      = "urls",
    label   = "URLs  (e.g. https://example.com)",
    pattern = "https?://\\S+|www[.]\\S+",
    desc    = "Removes http://, https://, and www. URLs."
  ),
  emails = list(
    id      = "emails",
    label   = "Email addresses  (e.g. user@example.com)",
    pattern = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+[.][a-zA-Z]{2,}",
    desc    = "Removes email addresses."
  ),
  speaker = list(
    id      = "speaker",
    label   = "Speaker labels  (e.g. [SPEAKER_A]:, <S1A-001$A>)",
    pattern = "(\\[[A-Z0-9_]+\\]\\s*:?|<[A-Z0-9$_-]+>\\s*(<#>\\s*)?)",
    desc    = "Removes common spoken corpus speaker turn labels."
  ),
  whitespace = list(
    id      = "whitespace",
    label   = "Extra whitespace  (collapses to single space)",
    pattern = NULL,   # handled specially - always applied last
    desc    = "Collapses runs of spaces/tabs to one space; trims each line."
  )
)

PREBUILT_IDS <- names(PREBUILT)

# ==============================================================
#  CLEANING ENGINE
# ==============================================================

#' Apply the full cleaning pipeline to a single character string.
#' All stringi calls are vectorised - no R-level loops.
#'
#' @param txt         Single character string (full file content)
#' @param selected    Character vector of PREBUILT ids to apply
#' @param lowercase   Logical
#' @param custom_pats Character vector of custom removal patterns
#' @param repl_dt     data.table with cols: find, replace, is_regex, ignore_case
#' @return Cleaned character string
clean_one <- function(txt, selected, lowercase, custom_pats, repl_dt) {
  
  # Guard: nothing to do
  if (!nzchar(txt)) return(txt)
  
  # -- Pre-built removals (order matters - whitespace always last) --
  ordered_ids <- c(
    intersect(c("tags","nonalpha","punct","numbers",
                "urls","emails","speaker"), selected)
  )
  
  for (id in ordered_ids) {
    pat <- PREBUILT[[id]]$pattern
    if (!is.null(pat)) {
      txt <- stringi::stri_replace_all_regex(txt, pat, "")
    }
  }
  
  # -- Custom removal patterns ----------------------------------
  for (pat in custom_pats) {
    pat <- trimws(pat)
    if (!nzchar(pat)) next
    txt <- tryCatch(
      stringi::stri_replace_all_regex(txt, pat, ""),
      error = function(e) txt   # skip invalid regex
    )
  }
  
  # -- Find -> Replace table -------------------------------------
  if (!is.null(repl_dt) && nrow(repl_dt) > 0) {
    for (i in seq_len(nrow(repl_dt))) {
      find    <- repl_dt$find[i]
      replace <- repl_dt$replace[i]
      is_rx   <- isTRUE(repl_dt$is_regex[i])
      ic      <- isTRUE(repl_dt$ignore_case[i])
      
      if (!nzchar(trimws(find))) next
      
      txt <- tryCatch({
        if (is_rx) {
          stringi::stri_replace_all_regex(
            txt, find, replace,
            opts_regex = stringi::stri_opts_regex(case_insensitive = ic)
          )
        } else {
          stringi::stri_replace_all_fixed(
            txt, find, replace,
            opts_fixed = stringi::stri_opts_fixed(case_insensitive = ic)
          )
        }
      }, error = function(e) txt)
    }
  }
  
  # -- Lowercase -------------------------------------------------
  if (isTRUE(lowercase)) {
    txt <- stringi::stri_trans_tolower(txt)
  }
  
  # -- Extra whitespace (always last) ---------------------------
  if ("whitespace" %in% selected) {
    txt <- stringi::stri_replace_all_regex(txt, "[ \\t]+", " ")
    txt <- stringi::stri_replace_all_regex(txt, "\\n{3,}", "\n\n")
  }
  
  # -- Trim each line --------------------------------------------
  lines <- stringi::stri_split_lines(txt)[[1]]
  lines <- stringi::stri_trim_both(lines)
  stringi::stri_join(lines, collapse = "\n")
}

#' Read a file efficiently as a single string
read_file_fast <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n")
}

#' Count tokens (whitespace-split words) in a string
count_tokens <- function(txt) {
  length(stringi::stri_split_regex(
    stringi::stri_trim_both(txt), "\\s+")[[1]])
}

# ==============================================================
#  UI
# ==============================================================

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
    tags$a("→ Tutorial", href = "https://ladal.edu.au/tutorials/string/string.html", target = "_blank",
           style = "font-size:.78rem;color:#51247a;")
  ),
  tags$blockquote(
    style = "border-left:3px solid #c8b8de;padding-left:12px;margin:0 0 10px 0;color:#444;",
    HTML(paste0(
      "Schweinberger, Martin. (2026). ",
      "<em>TextCleaner: A browser-based text cleaning tool</em>. ",
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
        "@misc{schweinberger2026textcleaner,\n",
        "  author       = {Schweinberger, Martin},\n",
        "  title        = {TextCleaner: A browser-based text cleaning tool},\n",
        "  year         = {2026},\n",
        "  organization = {The University of Queensland},\n",
        "  url          = {https://ladal.edu.au/tools.html}\n",
        "}"
      )
    )
  )
)

ui <- fluidPage(
  title = "TextCleaner | LADAL",
  
  tags$head(
    tags$style(HTML(paste0("
      body{font-family:'Segoe UI',Arial,sans-serif;
           background:#f7f4fb;color:#222;margin:0;}

      /* Banner */
      .tc-banner{background:", LADAL_PURPLE, ";color:white;
        padding:18px 32px 14px 32px;display:flex;
        align-items:center;gap:18px;
        border-bottom:4px solid ", LADAL_GOLD, ";}
      .tc-title{font-size:1.7rem;font-weight:700;
                letter-spacing:.5px;margin:0;}
      .tc-sub{font-size:.88rem;opacity:.85;margin:2px 0 0 0;}

      /* Layout */
      .tc-body{display:flex;min-height:calc(100vh - 80px);}
      .tc-side{width:340px;min-width:290px;max-width:370px;
        background:white;border-right:1px solid #e0d8ec;
        padding:22px 20px 30px 20px;
        box-shadow:2px 0 8px rgba(81,36,122,.06);
        overflow-y:auto;}
      .tc-main{flex:1;padding:24px 28px;overflow-x:auto;}

      /* Section headings */
      .tc-sec{font-size:.74rem;font-weight:700;letter-spacing:1.2px;
        text-transform:uppercase;color:", LADAL_PURPLE, ";
        border-bottom:2px solid ", LADAL_GOLD, ";
        padding-bottom:4px;margin:20px 0 10px 0;}
      .tc-sec:first-child{margin-top:0;}

      /* Inputs */
      .form-control,.selectize-input{
        border:1.5px solid #d0c8e0 !important;
        border-radius:6px !important;font-size:.88rem !important;}
      label{font-size:.87rem;font-weight:600;color:#444;}
      .checkbox label{font-weight:400 !important;
                      font-size:.86rem;color:#333;}

      /* Buttons */
      #run_preview{width:100%;background:#7d3c98 !important;
        border:none !important;color:white !important;
        font-weight:700;font-size:.95rem;padding:9px;
        border-radius:7px;margin-top:4px;transition:background .2s;}
      #run_preview:hover{background:#6c3483 !important;}
      #run_clean{width:100%;background:", LADAL_PURPLE, " !important;
        border:none !important;color:white !important;
        font-weight:700;font-size:.95rem;padding:9px;
        border-radius:7px;margin-top:6px;transition:background .2s;}
      #run_clean:hover{background:#3a1860 !important;}

      /* Upload */
      .shiny-input-container .btn{background:white;
        border:1.5px dashed ", LADAL_PURPLE, ";
        color:", LADAL_PURPLE, ";font-weight:600;width:100%;}

      /* Info boxes */
      .tc-info{background:#f4f0f8;
        border-left:4px solid ", LADAL_PURPLE, ";
        border-radius:5px;padding:9px 13px;font-size:.83rem;
        color:#444;margin-bottom:12px;}
      .tc-warn{background:#fff4e5;
        border-left:4px solid ", LADAL_GOLD, ";
        border-radius:5px;padding:9px 13px;font-size:.83rem;
        color:#6b4000;margin-bottom:10px;}
      .tc-ok{background:#eafaf1;border-left:4px solid #27ae60;
        border-radius:5px;padding:9px 13px;font-size:.83rem;
        color:#1a6b3c;margin-bottom:8px;}

      /* Stat cards */
      .tc-stats{display:flex;gap:12px;margin-bottom:18px;
                flex-wrap:wrap;}
      .tc-card{background:white;border-radius:9px;
        border-left:4px solid ", LADAL_PURPLE, ";
        padding:10px 15px;min-width:105px;
        box-shadow:0 1px 6px rgba(81,36,122,.08);}
      .tc-val{font-size:1.35rem;font-weight:700;
              color:", LADAL_PURPLE, ";line-height:1.1;}
      .tc-lbl{font-size:.74rem;color:#888;margin-top:2px;}

      /* Download buttons */
      .tc-dl{display:inline-block;margin:4px 5px 4px 0;
        background:white;border:1.5px solid ", LADAL_PURPLE, ";
        color:", LADAL_PURPLE, ";font-weight:600;font-size:.84rem;
        padding:6px 14px;border-radius:6px;cursor:pointer;
        text-decoration:none;transition:all .15s;}
      .tc-dl:hover{background:", LADAL_PURPLE, ";color:white;}

      /* Tabs */
      .nav-tabs > li > a{color:", LADAL_PURPLE, ";font-weight:600;}
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover{
        border-top:3px solid ", LADAL_PURPLE, " !important;
        color:", LADAL_PURPLE, " !important;}

      /* Preview boxes */
      .preview-box{background:#fafafa;border:1px solid #e0d8ec;
        border-radius:6px;padding:14px 16px;
        font-family:monospace;font-size:.82rem;
        white-space:pre-wrap;word-break:break-all;
        max-height:340px;overflow-y:auto;line-height:1.6;}
      .preview-orig{border-left:4px solid #aaa;}
      .preview-clean{border-left:4px solid #27ae60;}

      /* Replace table */
      .repl-wrap{overflow-x:auto;}
      table.repl-tbl{width:100%;border-collapse:collapse;
                     font-size:.83rem;}
      table.repl-tbl th{background:", LADAL_PURPLE, ";color:white;
        padding:6px 8px;text-align:left;font-weight:600;}
      table.repl-tbl td{padding:3px 4px;vertical-align:middle;}
      table.repl-tbl input[type='text']{width:100%;box-sizing:border-box;
        border:1px solid #d0c8e0;border-radius:4px;
        padding:3px 6px;font-size:.82rem;}
      table.repl-tbl input[type='checkbox']{
        display:block;margin:0 auto;}
      .del-btn{background:none;border:none;color:#cc0000;
               cursor:pointer;font-size:1rem;padding:2px 6px;}

      /* Footer */
      .tc-footer{background:#2d1a4a;color:#c8b8de;
        font-size:.78rem;padding:12px 32px;
        display:flex;gap:18px;align-items:center;}
      .tc-footer a{color:#d4b8f5;}
    "))),
    
    # JavaScript for dynamic replace table
    tags$script(HTML("
      var replRows = 0;

      function addReplRow() {
        replRows++;
        var id = replRows;
        var tr = document.createElement('tr');
        tr.id = 'repl_row_' + id;
        tr.innerHTML =
          '<td><input type=\"text\" id=\"repl_find_' + id + '\"' +
          ' onchange=\"updateRepl()\" placeholder=\"find...\"></td>' +
          '<td><input type=\"text\" id=\"repl_replace_' + id + '\"' +
          ' onchange=\"updateRepl()\" placeholder=\"replace with...\"></td>' +
          '<td style=\"text-align:center\">' +
          '<input type=\"checkbox\" id=\"repl_regex_' + id + '\"' +
          ' checked onchange=\"updateRepl()\"></td>' +
          '<td style=\"text-align:center\">' +
          '<input type=\"checkbox\" id=\"repl_ic_' + id + '\"' +
          ' onchange=\"updateRepl()\"></td>' +
          '<td><button class=\"del-btn\" onclick=\"delRow(' + id + ')\"' +
          ' title=\"Remove row\">X</button></td>';
        document.getElementById('repl_tbody').appendChild(tr);
        updateRepl();
      }

      function delRow(id) {
        var row = document.getElementById('repl_row_' + id);
        if (row) row.remove();
        updateRepl();
      }

      function updateRepl() {
        var rows = document.querySelectorAll('#repl_tbody tr');
        var data = [];
        rows.forEach(function(row) {
          var id = row.id.replace('repl_row_', '');
          var f  = document.getElementById('repl_find_'    + id);
          var r  = document.getElementById('repl_replace_' + id);
          var rx = document.getElementById('repl_regex_'   + id);
          var ic = document.getElementById('repl_ic_'      + id);
          if (f && r) {
            data.push(
              (f.value  || '') + '|||' +
              (r.value  || '') + '|||' +
              (rx && rx.checked ? '1' : '0') + '|||' +
              (ic && ic.checked ? '1' : '0')
            );
          }
        });
        Shiny.setInputValue('repl_table_raw', data.join('~~~'), {priority: 'event'});
      }
    "))
  ),
  
  # -- Banner ----------------------------------------------------
  div(class = "tc-banner",
      div(style = "font-size:2rem;", "TC"),
      div(
        p(class = "tc-title", "TextCleaner"),
        p(class = "tc-sub",
          "Remove and replace text elements | regex-powered | ",
          tags$a("LADAL", href = "https://ladal.edu.au",
                 style = "color:#f0c060;"))
      )
  ),
  
  # -- Body ------------------------------------------------------
  div(class = "tc-body",
      
      # -- Sidebar -------------------------------------------------
      div(class = "tc-side",
          
          # STEP 1
          div(class = "tc-sec", "Step 1: Upload texts"),
          div(class = "tc-info",
              "Upload one or more ", tags$b(".txt"), " files.
           Each file is cleaned independently."),
          fileInput("files", NULL,
                    multiple    = TRUE,
                    accept      = ".txt",
                    buttonLabel = "Choose .txt files"),
          uiOutput("corpus_status"),
          
          # STEP 2 - Pre-built removals
          div(class = "tc-sec", "Step 2: Remove"),
          div(class = "tc-info",
              "Tick any elements to remove from all texts."),
          
          checkboxInput("cb_tags",       PREBUILT$tags$label,       FALSE),
          checkboxInput("cb_nonalpha",   PREBUILT$nonalpha$label,   FALSE),
          checkboxInput("cb_punct",      PREBUILT$punct$label,      FALSE),
          checkboxInput("cb_numbers",    PREBUILT$numbers$label,    FALSE),
          checkboxInput("cb_urls",       PREBUILT$urls$label,       FALSE),
          checkboxInput("cb_emails",     PREBUILT$emails$label,     FALSE),
          checkboxInput("cb_speaker",    PREBUILT$speaker$label,    FALSE),
          checkboxInput("cb_whitespace", PREBUILT$whitespace$label, FALSE),
          checkboxInput("cb_lowercase",  "Convert to lowercase",    FALSE),
          
          tags$hr(style = "border-color:#e0d8ec; margin:10px 0;"),
          
          # Custom removal patterns
          tags$label("Custom removal patterns (one per line, regex):"),
          tags$textarea(
            id          = "custom_removes",
            class       = "form-control",
            rows        = 3,
            placeholder = "e.g.  <.*?>\nor  \\bACT\\s+[IVX]+\\b",
            style       = "font-family:monospace; font-size:.82rem;"
          ),
          
          # STEP 3 - Replace table
          div(class = "tc-sec", "Step 3: Replace"),
          div(class = "tc-info",
              "Add find->replace pairs. Tick ", tags$b("Regex"),
              " to use regular expressions; tick ",
              tags$b("IC"), " for case-insensitive matching."),
          
          div(class = "repl-wrap",
              tags$table(class = "repl-tbl",
                         tags$thead(
                           tags$tr(
                             tags$th("Find"),
                             tags$th("Replace with"),
                             tags$th(title = "Regular expression", "Regex"),
                             tags$th(title = "Case insensitive", "IC"),
                             tags$th("")
                           )
                         ),
                         tags$tbody(id = "repl_tbody")
              )
          ),
          tags$br(),
          tags$button("+ Add row", onclick = "addReplRow()",
                      style = paste0(
                        "background:white;border:1.5px solid ", LADAL_PURPLE, ";",
                        "color:", LADAL_PURPLE, ";font-size:.82rem;",
                        "font-weight:600;padding:4px 12px;border-radius:5px;",
                        "cursor:pointer;")),
          
          # STEP 4 - Preview & Apply
          div(class = "tc-sec", "Step 4: Preview & Apply"),
          div(class = "tc-info",
              "Preview on the first uploaded file before applying
           to all files."),
          selectInput("preview_file", "Preview file",
                      choices = NULL),
          actionButton("run_preview", "Preview",
                       class = "btn-default"),
          actionButton("run_clean",   "Clean all files",
                       class = "btn-primary"),
          
          # STEP 5 - Download
          div(class = "tc-sec", "Step 5: Download"),
          uiOutput("download_buttons")
      ),
      
      # -- Main panel -----------------------------------------------
      div(class = "tc-main",
          uiOutput("welcome_box"),
          uiOutput("stats_cards"),
          uiOutput("results_ui")
      )
  ),
  
  # -- Footer ---------------------------------------------------
  div(class = "tc-footer",
      span("TextCleaner * LADAL * University of Queensland"),
      tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
      tags$a("String Processing Tutorial",
             href = "https://ladal.edu.au/tutorials/string/string.html"),
      tags$a("Cite this tool",
             href = "https://ladal.edu.au/about.html#citing")
  ),
  CITATION_FOOTER
)

# ==============================================================
#  SERVER
# ==============================================================

server <- function(input, output, session) {
  
  # -- Raw file texts (read once, cached) -----------------------
  raw_texts <- reactive({
    req(input$files)
    setNames(
      lapply(input$files$datapath, read_file_fast),
      input$files$name
    )
  })
  
  # -- Update preview file dropdown -----------------------------
  observe({
    req(input$files)
    updateSelectInput(session, "preview_file",
                      choices = input$files$name,
                      selected = input$files$name[1])
  })
  
  # -- Corpus status badge --------------------------------------
  output$corpus_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "tc-warn", "No files uploaded yet.")
    } else {
      n <- nrow(input$files)
      div(class = "tc-ok",
          paste0(n, " file",
                 if (n > 1) "s" else "", " loaded: ",
                 paste(input$files$name, collapse = ", ")))
    }
  })
  
  # -- Parse cleaning spec from UI inputs -----------------------
  # Returns a list: selected, lowercase, custom_pats, repl_dt
  cleaning_spec <- reactive({
    selected <- character(0)
    if (isTRUE(input$cb_tags))       selected <- c(selected, "tags")
    if (isTRUE(input$cb_nonalpha))   selected <- c(selected, "nonalpha")
    if (isTRUE(input$cb_punct))      selected <- c(selected, "punct")
    if (isTRUE(input$cb_numbers))    selected <- c(selected, "numbers")
    if (isTRUE(input$cb_urls))       selected <- c(selected, "urls")
    if (isTRUE(input$cb_emails))     selected <- c(selected, "emails")
    if (isTRUE(input$cb_speaker))    selected <- c(selected, "speaker")
    if (isTRUE(input$cb_whitespace)) selected <- c(selected, "whitespace")
    
    # Custom removal patterns (one per line)
    custom_pats <- character(0)
    raw_custom <- input$custom_removes
    if (!is.null(raw_custom) && nzchar(trimws(raw_custom))) {
      custom_pats <- Filter(nzchar,
                            trimws(strsplit(raw_custom, "\n")[[1]]))
    }
    
    # Replace table (sent from JS as encoded string)
    repl_dt <- data.table(find        = character(),
                          replace     = character(),
                          is_regex    = logical(),
                          ignore_case = logical())
    
    raw_repl <- input$repl_table_raw
    if (!is.null(raw_repl) && nzchar(raw_repl)) {
      rows <- strsplit(raw_repl, "~~~")[[1]]
      rows <- Filter(nzchar, rows)
      if (length(rows) > 0) {
        parsed <- lapply(rows, function(r) {
          parts <- strsplit(r, "\\|\\|\\|")[[1]]
          while (length(parts) < 4) parts <- c(parts, "")
          list(find        = parts[1],
               replace     = parts[2],
               is_regex    = parts[3] == "1",
               ignore_case = parts[4] == "1")
        })
        repl_dt <- rbindlist(parsed)
        repl_dt <- repl_dt[nzchar(trimws(find))]
      }
    }
    
    list(
      selected    = selected,
      lowercase   = isTRUE(input$cb_lowercase),
      custom_pats = custom_pats,
      repl_dt     = repl_dt
    )
  })
  
  # -- Preview (single file) -------------------------------------
  preview_result <- eventReactive(input$run_preview, {
    req(raw_texts(), input$preview_file)
    spec <- cleaning_spec()
    txt  <- raw_texts()[[input$preview_file]]
    
    cleaned <- clean_one(txt,
                         selected    = spec$selected,
                         lowercase   = spec$lowercase,
                         custom_pats = spec$custom_pats,
                         repl_dt     = spec$repl_dt)
    list(original = txt, cleaned = cleaned,
         filename = input$preview_file)
  })
  
  # -- Clean all files -------------------------------------------
  cleaned_texts <- eventReactive(input$run_clean, {
    req(raw_texts())
    spec  <- cleaning_spec()
    texts <- raw_texts()
    
    withProgress(message = "Cleaning texts...", value = 0, {
      result <- lapply(seq_along(texts), function(i) {
        incProgress(1 / length(texts),
                    detail = paste("File", i, "of", length(texts)))
        clean_one(texts[[i]],
                  selected    = spec$selected,
                  lowercase   = spec$lowercase,
                  custom_pats = spec$custom_pats,
                  repl_dt     = spec$repl_dt)
      })
    })
    
    setNames(result, names(texts))
  })
  
  # -- Welcome box ----------------------------------------------
  output$welcome_box <- renderUI({
    if (input$run_preview == 0 && input$run_clean == 0) {
      div(class = "tc-info", style = "font-size:.93rem;",
          tags$b("Welcome to TextCleaner."), br(),
          "Upload your plain-text files, choose what to remove or
         replace in the sidebar, then click ",
          tags$b("Preview"), " to see the effect on one file before
         clicking ", tags$b("Clean all files"), " to process
         everything.", br(), br(),
          "Cleaned files are named ",
          tags$code("originalname_cleaned.txt"),
          " and can be downloaded as individual files or as a
         single ZIP archive.", br(), br(),
          tags$a("-> String Processing Tutorial",
                 href = "https://ladal.edu.au/tutorials/string/string.html")
      )
    }
  })
  
  # -- Stat cards (after cleaning) ------------------------------
  output$stats_cards <- renderUI({
    req(input$run_clean > 0, cleaned_texts())
    ct  <- cleaned_texts()
    rt  <- raw_texts()
    
    orig_chars    <- sum(nchar(unlist(rt)))
    cleaned_chars <- sum(nchar(unlist(ct)))
    removed_chars <- orig_chars - cleaned_chars
    pct <- if (orig_chars > 0)
      round(removed_chars / orig_chars * 100, 1) else 0
    
    div(class = "tc-stats",
        div(class = "tc-card",
            div(class = "tc-val", length(ct)),
            div(class = "tc-lbl", "Files cleaned")),
        div(class = "tc-card",
            div(class = "tc-val",
                format(orig_chars, big.mark = ",")),
            div(class = "tc-lbl", "Original chars")),
        div(class = "tc-card",
            div(class = "tc-val",
                format(cleaned_chars, big.mark = ",")),
            div(class = "tc-lbl", "Cleaned chars")),
        div(class = "tc-card",
            div(class = "tc-val", paste0(pct, "%")),
            div(class = "tc-lbl", "Chars removed"))
    )
  })
  
  # -- Results UI -----------------------------------------------
  output$results_ui <- renderUI({
    show_preview <- input$run_preview > 0
    show_clean   <- input$run_clean   > 0
    
    if (!show_preview && !show_clean) return(NULL)
    
    tabs <- list()
    
    if (show_preview) {
      tabs <- c(tabs, list(
        tabPanel("Preview", br(), uiOutput("preview_ui"))
      ))
    }
    
    if (show_clean) {
      tabs <- c(tabs, list(
        tabPanel("Cleaned texts", br(), uiOutput("cleaned_ui")),
        tabPanel("Change summary", br(), uiOutput("summary_ui")),
        tabPanel(
          "⚙️ Parameters",
          br(),
          p(style="font-size:.85rem;color:#555;",
            "Download a record of all parameters used for reproducibility."),
          uiOutput("params_dl_ui"),
          br(),
          verbatimTextOutput("params_preview")
        )
      ))
    }
    
    do.call(tabsetPanel, tabs)
  })
  
  # -- Preview panel ---------------------------------------------
  output$preview_ui <- renderUI({
    req(preview_result())
    pr <- preview_result()
    
    orig_prev    <- substr(pr$original, 1, 1500)
    cleaned_prev <- substr(pr$cleaned,  1, 1500)
    
    if (nchar(pr$original) > 1500)
      orig_prev    <- paste0(orig_prev, "\n...[truncated]")
    if (nchar(pr$cleaned)  > 1500)
      cleaned_prev <- paste0(cleaned_prev, "\n...[truncated]")
    
    orig_chars    <- nchar(pr$original)
    cleaned_chars <- nchar(pr$cleaned)
    removed       <- orig_chars - cleaned_chars
    pct <- if (orig_chars > 0) round(removed / orig_chars * 100, 1) else 0
    
    tagList(
      div(class = "tc-info",
          tags$b("File: "), pr$filename, tags$br(),
          tags$b("Original: "), format(orig_chars, big.mark = ","),
          " chars -> ",
          tags$b("Cleaned: "), format(cleaned_chars, big.mark = ","),
          " chars (", pct, "% removed)"
      ),
      fluidRow(
        column(6,
               tags$h5(style = paste0("color:", LADAL_PURPLE,
                                      "; font-weight:bold;"),
                       "Original"),
               div(class = "preview-box preview-orig", orig_prev)
        ),
        column(6,
               tags$h5(style = "color:#27ae60; font-weight:bold;",
                       "Cleaned"),
               div(class = "preview-box preview-clean", cleaned_prev)
        )
      )
    )
  })
  
  # -- Cleaned texts panel ---------------------------------------
  output$cleaned_ui <- renderUI({
    req(cleaned_texts())
    ct <- cleaned_texts()
    
    items <- lapply(names(ct), function(fname) {
      out_name <- paste0(tools::file_path_sans_ext(fname),
                         "_cleaned.txt")
      preview  <- substr(ct[[fname]], 1, 600)
      if (nchar(ct[[fname]]) > 600)
        preview <- paste0(preview, "\n...[truncated]")
      
      tagList(
        tags$h5(style = paste0("color:", LADAL_PURPLE,
                               "; font-weight:bold;"),
                paste0(out_name)),
        div(class = "preview-box preview-clean",
            style = "max-height:180px;",
            preview),
        tags$hr(style = "border-color:#e0d8ec;")
      )
    })
    
    tagList(
      div(class = "tc-info",
          "Previews of cleaned files (first 600 characters).
           Download all files using the buttons in the sidebar."),
      items
    )
  })
  
  # -- Change summary table --------------------------------------
  output$summary_ui <- renderUI({
    req(cleaned_texts(), raw_texts())
    ct <- cleaned_texts()
    rt <- raw_texts()
    
    dt <- data.table(
      File            = names(rt),
      Orig_chars      = vapply(rt, nchar, integer(1)),
      Cleaned_chars   = vapply(ct, nchar, integer(1))
    )
    dt[, Removed_chars := Orig_chars - Cleaned_chars]
    dt[, Pct_removed   := round(Removed_chars / Orig_chars * 100, 1)]
    dt[, Output_file   := paste0(
      tools::file_path_sans_ext(File), "_cleaned.txt")]
    
    setnames(dt, c("File", "Original chars", "Cleaned chars",
                   "Removed chars", "% removed", "Output filename"))
    
    DT::renderDT(
      as.data.frame(dt),
      rownames = FALSE,
      options  = list(dom = "t", pageLength = 50)
    )
  })
  
  # -- Download buttons -----------------------------------------
  output$download_buttons <- renderUI({
    if (input$run_clean == 0 || is.null(cleaned_texts()))
      return(div(style = "color:#aaa; font-size:.82rem;",
                 "Clean files to enable downloads."))
    
    ct <- cleaned_texts()
    
    # Individual file buttons
    ind_btns <- lapply(names(ct), function(fname) {
      out_name <- paste0(tools::file_path_sans_ext(fname),
                         "_cleaned.txt")
      safe_id  <- paste0("dl_", gsub("[^a-zA-Z0-9]", "_", fname))
      downloadButton(safe_id, paste0("Download: ", out_name),
                     class = "tc-dl",
                     style = "display:block; margin-bottom:4px;
                              width:100%; text-align:left;")
    })
    
    tagList(
      downloadButton("dl_zip", "Download all files (.zip)",
                     class = "tc-dl",
                     style = "margin-bottom:10px;"),
      tags$hr(style = "border-color:#e0d8ec; margin:8px 0;"),
      tags$p(style = "font-size:.78rem; color:#888;
                       margin-bottom:6px;",
             "Or download individually:"),
      ind_btns
    )
  })
  
  # -- ZIP download ----------------------------------------------
  output$dl_zip <- downloadHandler(
    filename = function()
      paste0("textcleaner_", Sys.Date(), ".zip"),
    content  = function(file) {
      ct      <- cleaned_texts()
      tmp_dir <- tempfile()
      dir.create(tmp_dir)
      
      txt_files <- vapply(names(ct), function(fname) {
        out_name <- paste0(tools::file_path_sans_ext(fname),
                           "_cleaned.txt")
        out_path <- file.path(tmp_dir, out_name)
        writeLines(ct[[fname]], out_path, useBytes = FALSE)
        out_path
      }, character(1))
      
      zip::zip(zipfile = file,
               files   = basename(txt_files),
               root    = tmp_dir)
    }
  )
  
  # -- Individual file downloads (generated dynamically) ---------
  observe({
    req(cleaned_texts())
    ct <- cleaned_texts()
    
    lapply(names(ct), function(fname) {
      safe_id  <- paste0("dl_", gsub("[^a-zA-Z0-9]", "_", fname))
      out_name <- paste0(tools::file_path_sans_ext(fname),
                         "_cleaned.txt")
      txt      <- ct[[fname]]
      
      output[[safe_id]] <- downloadHandler(
        filename = function() out_name,
        content  = function(file) writeLines(txt, file)
      )
    })
  })

  # ── Parameters download ─────────────────────────────────────────
  output$dl_params <- downloadHandler(
    filename = function() paste0("textcleaner_params_", Sys.Date(), ".txt"),
    content  = function(file) {
      lines <- c(
        paste0("Tool:                ", "TextCleaner — Text Cleaning"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("stringi:             ", as.character(packageVersion('stringi'))),
        paste0("---                  ", ""),
        paste0("Remove XML tags:     ", as.character(isTRUE(input$cb_tags))),
        paste0("Remove URLs:         ", as.character(isTRUE(input$cb_urls))),
        paste0("Remove emails:       ", as.character(isTRUE(input$cb_emails))),
        paste0("Remove numbers:      ", as.character(isTRUE(input$cb_numbers))),
        paste0("Remove punct:        ", as.character(isTRUE(input$cb_punct))),
        paste0("Lowercase:           ", as.character(isTRUE(input$cb_lowercase))),
        paste0("Collapse spaces:     ", as.character(isTRUE(input$cb_whitespace))),
        paste0("Files:               ", if (!is.null(input$files)) paste(input$files$name, collapse=", ") else "none")
      )
      writeLines(lines, file)
    }
  )

  output$params_dl_ui <- renderUI({
    downloadButton("dl_params", "⬇ Download parameters (.txt)")
  })

  output$params_preview <- renderText({
    paste(c(
        paste0("Tool:                ", "TextCleaner — Text Cleaning"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("stringi:             ", as.character(packageVersion('stringi'))),
        paste0("---                  ", ""),
        paste0("Remove XML tags:     ", as.character(isTRUE(input$cb_tags))),
        paste0("Remove URLs:         ", as.character(isTRUE(input$cb_urls))),
        paste0("Remove emails:       ", as.character(isTRUE(input$cb_emails))),
        paste0("Remove numbers:      ", as.character(isTRUE(input$cb_numbers))),
        paste0("Remove punct:        ", as.character(isTRUE(input$cb_punct))),
        paste0("Lowercase:           ", as.character(isTRUE(input$cb_lowercase))),
        paste0("Collapse spaces:     ", as.character(isTRUE(input$cb_whitespace)))
    ), collapse="\n")
  })

}

# ==============================================================)

shinyApp(ui, server)
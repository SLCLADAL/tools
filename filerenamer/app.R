# ============================================================
#  FileRenamer — LADAL File Renaming Tool (Shiny)
#  https://ladal.edu.au
# ============================================================

library(shiny)
library(stringi)
library(DT)
library(zip)

LADAL_PURPLE <- "#51247a"
LADAL_GOLD   <- "#f0a500"

# ── Helpers ──────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (is.null(a)) b else a

# Apply all active renaming operations to a single filename stem.
# Operations run top-to-bottom in the order shown in the UI.
apply_operations <- function(stem, ops) {

  # 1. Find & Replace
  if (isTRUE(ops$use_findreplace) && nchar(ops$find) > 0) {
    stem <- stri_replace_all(
      stem,
      replacement = ops$replace,
      regex       = if (ops$fr_regex) ops$find
                    else              stri_escape_regex(ops$find),
      opts_regex  = list(case_insensitive = !ops$fr_case)
    )
  }

  # 2. Remove characters / substrings
  if (isTRUE(ops$use_remove) && nchar(ops$remove_pattern) > 0) {
    stem <- stri_replace_all(
      stem,
      replacement = "",
      regex       = if (ops$rm_regex) ops$remove_pattern
                    else              stri_escape_regex(ops$remove_pattern),
      opts_regex  = list(case_insensitive = !ops$rm_case)
    )
  }

  # 3. Change case
  if (isTRUE(ops$use_case)) {
    stem <- switch(ops$case_type,
      "lower"    = stri_trans_tolower(stem),
      "upper"    = stri_trans_toupper(stem),
      "title"    = stri_trans_totitle(stem),
      "sentence" = paste0(stri_trans_toupper(stri_sub(stem, 1, 1)),
                          stri_trans_tolower(stri_sub(stem, 2))),
      stem
    )
  }

  # 4. Strip / reformat date patterns
  if (isTRUE(ops$use_date)) {
    date_regex <- paste0(
      "(\\d{4}[-_.]\\d{2}[-_.]\\d{2})",   # YYYY-MM-DD / YYYY_MM_DD
      "|(\\d{2}[-_.]\\d{2}[-_.]\\d{4})",   # DD-MM-YYYY
      "|(\\d{8})"                           # YYYYMMDD
    )
    if (ops$date_action == "remove") {
      stem <- stri_replace_all_regex(stem, date_regex, "")
      stem <- stri_replace_all_regex(stem, "^[-_.\\s]+|[-_.\\s]+$", "")
    } else if (ops$date_action == "replace" && nchar(ops$date_reformat) > 0) {
      stem <- stri_replace_all_regex(stem, date_regex, ops$date_reformat)
    }
  }

  # 5. Add prefix / suffix  (applied after transforms so they are not mutated)
  if (isTRUE(ops$use_affix)) {
    if (nchar(ops$prefix) > 0) stem <- paste0(ops$prefix, stem)
    if (nchar(ops$suffix) > 0) stem <- paste0(stem, ops$suffix)
  }

  stem
}

# Build new full filenames for every uploaded file.
# Sequential numbering is applied last (after all per-name transforms).
build_new_names <- function(original_names, ops) {
  exts      <- tools::file_ext(original_names)
  stems     <- tools::file_path_sans_ext(original_names)
  new_stems <- vapply(stems, apply_operations, character(1), ops = ops)

  # 6. Sequential numbering
  if (isTRUE(ops$use_seq)) {
    n     <- length(new_stems)
    width <- max(nchar(as.character(n)), ops$seq_width)
    nums  <- formatC(seq_along(new_stems) + ops$seq_start - 1,
                     width = width, flag = "0")
    sep   <- ops$seq_sep
    new_stems <- switch(ops$seq_pos,
      "prefix" = paste0(nums, sep, new_stems),
      "suffix" = paste0(new_stems, sep, nums)
    )
  }

  # Sanitise: illegal file-system characters → underscore
  new_stems <- stri_replace_all_regex(new_stems, '[\\\\/:*?"<>|]', "_")
  # Collapse multiple consecutive separators left by removals
  new_stems <- stri_replace_all_regex(new_stems, "[ _-]{2,}", "_")
  # Strip leading / trailing separators
  new_stems <- stri_replace_all_regex(new_stems, "^[ _.-]+|[ _.-]+$", "")
  # Guard against empty stems
  new_stems <- ifelse(nchar(new_stems) == 0, paste0("file_", seq_along(new_stems)), new_stems)

  ifelse(nchar(exts) > 0,
         paste0(new_stems, ".", exts),
         new_stems)
}

# Collect all input values into a plain list for easier passing.
collect_ops <- function(input) {
  list(
    use_findreplace = input$use_findreplace,
    find            = input$find          %||% "",
    replace         = input$replace       %||% "",
    fr_regex        = isTRUE(input$fr_regex),
    fr_case         = isTRUE(input$fr_case),

    use_remove      = input$use_remove,
    remove_pattern  = input$remove_pattern %||% "",
    rm_regex        = isTRUE(input$rm_regex),
    rm_case         = isTRUE(input$rm_case),

    use_case        = input$use_case,
    case_type       = input$case_type %||% "lower",

    use_date        = input$use_date,
    date_action     = input$date_action   %||% "remove",
    date_reformat   = input$date_reformat %||% "",

    use_affix       = input$use_affix,
    prefix          = input$prefix %||% "",
    suffix          = input$suffix %||% "",

    use_seq         = input$use_seq,
    seq_pos         = input$seq_pos   %||% "suffix",
    seq_sep         = input$seq_sep   %||% "_",
    seq_start       = as.integer(input$seq_start %||% 1L),
    seq_width       = as.integer(input$seq_width %||% 3L)
  )
}

# ── UI ───────────────────────────────────────────────────────────────

ui <- fluidPage(
  title = "FileRenamer | LADAL",

  tags$head(
    tags$style(HTML(paste0("
      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=IBM+Plex+Sans:wght@400;600;700&display=swap');

      *, *::before, *::after { box-sizing: border-box; }

      body {
        font-family: 'IBM Plex Sans', sans-serif;
        background: #f5f3f8;
        color: #1a1a2e;
        margin: 0;
        font-size: 14px;
      }

      /* ── Banner ── */
      .fr-banner {
        background: ", LADAL_PURPLE, ";
        color: white;
        padding: 16px 32px 13px 32px;
        display: flex; align-items: center; gap: 16px;
        border-bottom: 4px solid ", LADAL_GOLD, ";
      }
      .fr-banner-icon  { font-size: 1.9rem; line-height: 1; }
      .fr-banner-title { font-size: 1.6rem; font-weight: 700;
                         letter-spacing: .3px; margin: 0; }
      .fr-banner-sub   { font-size: .83rem; opacity: .8; margin: 2px 0 0 0; }
      .fr-banner a     { color: #f7d97a; text-decoration: none; }

      /* ── Layout ── */
      .fr-body { display: flex; min-height: calc(100vh - 78px); }
      .fr-side {
        width: 330px; min-width: 280px; max-width: 360px;
        background: white;
        border-right: 1px solid #e0d8ec;
        padding: 20px 18px 32px 18px;
        box-shadow: 2px 0 10px rgba(81,36,122,.05);
        overflow-y: auto;
      }
      .fr-main { flex: 1; padding: 24px 28px; overflow-x: auto; }

      /* ── Sidebar section titles ── */
      .fr-sec {
        font-size: .72rem; font-weight: 700; letter-spacing: 1.3px;
        text-transform: uppercase; color: ", LADAL_PURPLE, ";
        border-bottom: 2px solid ", LADAL_GOLD, ";
        padding-bottom: 4px; margin: 22px 0 11px 0;
      }
      .fr-sec:first-child { margin-top: 0; }

      /* ── Op cards ── */
      .fr-op-card {
        background: #faf8fd;
        border: 1.5px solid #e0d8ec;
        border-radius: 8px;
        padding: 11px 13px 13px 13px;
        margin-bottom: 10px;
        transition: border-color .15s, box-shadow .15s;
      }
      .fr-op-card.active {
        border-color: ", LADAL_PURPLE, ";
        box-shadow: 0 0 0 3px rgba(81,36,122,.08);
      }
      .fr-op-header {
        display: flex; align-items: center; gap: 8px;
      }
      .fr-op-label {
        font-weight: 700; font-size: .88rem; color: #333;
        cursor: pointer; user-select: none; margin: 0;
      }
      .fr-op-body { margin-top: 10px; }

      /* ── Inputs ── */
      .form-control, .selectize-input {
        border: 1.5px solid #d0c8e0 !important;
        border-radius: 6px !important;
        font-size: .88rem !important;
        font-family: 'IBM Plex Mono', monospace !important;
      }
      .form-control:focus {
        border-color: ", LADAL_PURPLE, " !important;
        box-shadow: 0 0 0 2px rgba(81,36,122,.13) !important;
      }
      label { font-size: .83rem; font-weight: 600; color: #555;
              margin-bottom: 3px; }
      .form-group { margin-bottom: 8px; }

      /* Compact inline pairs */
      .fr-row          { display: flex; gap: 8px; }
      .fr-row .form-group { flex: 1; min-width: 0; }

      /* Checkbox row */
      .fr-check {
        font-size: .81rem; color: #666; margin-top: 5px;
        display: flex; gap: 14px; flex-wrap: wrap;
        align-items: center;
      }
      .fr-check label { font-weight: 400; color: #666;
                        font-size: .81rem; margin-bottom: 0; }
      .fr-check .form-group { margin-bottom: 0; }

      /* ── Main action button ── */
      #apply_btn {
        width: 100%;
        background: ", LADAL_PURPLE, " !important;
        border: none !important; color: white !important;
        font-weight: 700; font-size: .97rem; padding: 11px;
        border-radius: 7px; margin-top: 4px;
        font-family: 'IBM Plex Sans', sans-serif;
        letter-spacing: .2px;
        transition: background .2s;
      }
      #apply_btn:hover { background: #3d1763 !important; }

      /* ── Download button ── */
      .fr-dl-btn {
        display: inline-flex; align-items: center; gap: 7px;
        background: white;
        border: 2px solid ", LADAL_PURPLE, ";
        color: ", LADAL_PURPLE, ";
        font-weight: 700; font-size: .88rem;
        padding: 8px 18px; border-radius: 7px;
        cursor: pointer; text-decoration: none;
        transition: all .15s;
        font-family: 'IBM Plex Sans', sans-serif;
      }
      .fr-dl-btn:hover { background: ", LADAL_PURPLE, "; color: white; }

      /* ── Info / warn / ok boxes ── */
      .fr-info {
        background: #f4f0f8; border-left: 4px solid ", LADAL_PURPLE, ";
        border-radius: 5px; padding: 9px 13px;
        font-size: .83rem; color: #444; margin-bottom: 12px;
      }
      .fr-warn {
        background: #fff4e5; border-left: 4px solid ", LADAL_GOLD, ";
        border-radius: 5px; padding: 9px 13px;
        font-size: .83rem; color: #6b4000; margin-bottom: 10px;
      }
      .fr-ok {
        background: #eafaf1; border-left: 4px solid #27ae60;
        border-radius: 5px; padding: 9px 13px;
        font-size: .83rem; color: #1a6b3c; margin-bottom: 8px;
      }
      .fr-conflict-badge {
        background: #fdecea; border-left: 4px solid #c0392b;
        border-radius: 5px; padding: 9px 13px;
        font-size: .83rem; color: #922b21; margin-bottom: 12px;
      }

      /* ── Preview header ── */
      .fr-preview-header {
        font-size: 1rem; font-weight: 700; color: ", LADAL_PURPLE, ";
        margin-bottom: 12px;
        display: flex; align-items: center;
        justify-content: space-between; flex-wrap: wrap; gap: 10px;
      }

      /* ── DT table ── */
      .dataTables_wrapper { font-size: .86rem; }
      table.dataTable thead th {
        background: ", LADAL_PURPLE, " !important;
        color: white !important; font-weight: 700;
        border-bottom: 2px solid ", LADAL_GOLD, " !important;
        font-family: 'IBM Plex Sans', sans-serif;
      }
      table.dataTable tbody tr:hover { background: #f4f0f8 !important; }

      /* ── Stat chips ── */
      .fr-chips { display: flex; gap: 10px; flex-wrap: wrap;
                  margin-bottom: 18px; }
      .fr-chip  {
        background: white; border-radius: 20px;
        border: 1.5px solid #e0d8ec;
        padding: 6px 14px; font-size: .82rem;
        display: flex; align-items: center; gap: 5px;
      }
      .fr-chip b { color: ", LADAL_PURPLE, "; font-size: .95rem; }

      /* ── Welcome ── */
      .fr-welcome {
        max-width: 540px; margin: 50px auto; text-align: center;
        color: #888;
      }
      .fr-welcome-icon { font-size: 3.5rem; margin-bottom: 14px; }
      .fr-welcome h3 { color: #555; font-weight: 700;
                       font-size: 1.2rem; margin-bottom: 8px; }
      .fr-welcome p  { line-height: 1.7; }

      /* ── Order note ── */
      .fr-order-note {
        font-size: .76rem; color: #999; margin: -6px 0 10px 0;
        font-style: italic;
      }

      /* ── Footer ── */
      .fr-footer {
        background: #2d1a4a; color: #c8b8de;
        font-size: .77rem; padding: 11px 32px;
        display: flex; gap: 18px; align-items: center;
      }
      .fr-footer a { color: #d4b8f5; }

      /* ── Upload button ── */
      .shiny-input-container .btn {
        background: white !important;
        border: 1.5px dashed ", LADAL_PURPLE, " !important;
        color: ", LADAL_PURPLE, " !important;
        font-weight: 600 !important; width: 100% !important;
        font-family: 'IBM Plex Sans', sans-serif !important;
      }
    ")))
  ),

  # ── Banner ──────────────────────────────────────────────
  div(class = "fr-banner",
    div(class = "fr-banner-icon", "✏️"),
    div(
      p(class = "fr-banner-title", "FileRenamer"),
      p(class = "fr-banner-sub",
        "Batch file renaming · ",
        tags$a("LADAL", href = "https://ladal.edu.au"))
    )
  ),

  # ── Body ────────────────────────────────────────────────
  div(class = "fr-body",

    # ── Sidebar ─────────────────────────────────────────
    div(class = "fr-side",

      # STEP 1 — Upload
      div(class = "fr-sec", "① Upload files"),
      div(class = "fr-info",
        "Any file type accepted. Only file ", tags$b("names"),
        " are modified — file contents are never read or changed."
      ),
      fileInput("files", NULL,
                multiple    = TRUE,
                accept      = NULL,
                buttonLabel = "📂 Choose files (any type)"),
      uiOutput("upload_status"),

      # STEP 2 — Operations
      div(class = "fr-sec", "② Rename operations"),
      p(class = "fr-order-note",
        "Operations applied top-to-bottom in this order."),

      # ── Op 1: Find & Replace ────────────────────────
      div(id = "card_fr", class = "fr-op-card",
        div(class = "fr-op-header",
          checkboxInput("use_findreplace", NULL, value = FALSE),
          tags$label(`for` = "use_findreplace",
                     class = "fr-op-label", "🔁  Find & replace")
        ),
        conditionalPanel("input.use_findreplace",
          div(class = "fr-op-body",
            div(class = "fr-row",
              textInput("find",    "Find",
                        placeholder = "text to find"),
              textInput("replace", "Replace with",
                        placeholder = "replacement")
            ),
            div(class = "fr-check",
              div(checkboxInput("fr_regex", "Regex",           value = FALSE)),
              div(checkboxInput("fr_case",  "Case-sensitive",  value = FALSE))
            )
          )
        )
      ),

      # ── Op 2: Remove ──────────────────────────────
      div(id = "card_rm", class = "fr-op-card",
        div(class = "fr-op-header",
          checkboxInput("use_remove", NULL, value = FALSE),
          tags$label(`for` = "use_remove",
                     class = "fr-op-label", "🗑  Remove substrings")
        ),
        conditionalPanel("input.use_remove",
          div(class = "fr-op-body",
            textInput("remove_pattern", "Pattern to remove",
                      placeholder = "e.g.  _draft  or  -v\\d+"),
            div(class = "fr-check",
              div(checkboxInput("rm_regex", "Regex",           value = FALSE)),
              div(checkboxInput("rm_case",  "Case-sensitive",  value = FALSE))
            )
          )
        )
      ),

      # ── Op 3: Case ────────────────────────────────
      div(id = "card_case", class = "fr-op-card",
        div(class = "fr-op-header",
          checkboxInput("use_case", NULL, value = FALSE),
          tags$label(`for` = "use_case",
                     class = "fr-op-label", "🔤  Change case")
        ),
        conditionalPanel("input.use_case",
          div(class = "fr-op-body",
            selectInput("case_type", "Convert to",
              choices = c(
                "lowercase"     = "lower",
                "UPPERCASE"     = "upper",
                "Title Case"    = "title",
                "Sentence case" = "sentence"
              ),
              selected = "lower"
            )
          )
        )
      ),

      # ── Op 4: Dates ───────────────────────────────
      div(id = "card_date", class = "fr-op-card",
        div(class = "fr-op-header",
          checkboxInput("use_date", NULL, value = FALSE),
          tags$label(`for` = "use_date",
                     class = "fr-op-label", "📅  Date patterns")
        ),
        conditionalPanel("input.use_date",
          div(class = "fr-op-body",
            div(class = "fr-info", style = "margin-bottom:8px; font-size:.8rem;",
              "Detects ", tags$code("YYYY-MM-DD"), ", ",
              tags$code("DD-MM-YYYY"), ", ", tags$code("YYYYMMDD"),
              " with ", tags$code("-"), " ", tags$code("_"),
              " or ", tags$code("."), " separators."
            ),
            selectInput("date_action", "Action",
              choices = c(
                "Remove date entirely" = "remove",
                "Replace date with…"   = "replace"
              ),
              selected = "remove"
            ),
            conditionalPanel("input.date_action === 'replace'",
              textInput("date_reformat", "Replacement text",
                        placeholder = "e.g.  2024  or  DATE")
            )
          )
        )
      ),

      # ── Op 5: Prefix / Suffix ─────────────────────
      div(id = "card_affix", class = "fr-op-card",
        div(class = "fr-op-header",
          checkboxInput("use_affix", NULL, value = FALSE),
          tags$label(`for` = "use_affix",
                     class = "fr-op-label", "➕  Add prefix / suffix")
        ),
        conditionalPanel("input.use_affix",
          div(class = "fr-op-body",
            div(class = "fr-row",
              textInput("prefix", "Prefix",
                        placeholder = "e.g. LADAL_"),
              textInput("suffix", "Suffix",
                        placeholder = "e.g. _final")
            )
          )
        )
      ),

      # ── Op 6: Sequential numbering ────────────────
      div(id = "card_seq", class = "fr-op-card",
        div(class = "fr-op-header",
          checkboxInput("use_seq", NULL, value = FALSE),
          tags$label(`for` = "use_seq",
                     class = "fr-op-label", "🔢  Sequential numbering")
        ),
        conditionalPanel("input.use_seq",
          div(class = "fr-op-body",
            div(class = "fr-row",
              selectInput("seq_pos", "Position",
                          choices  = c("Suffix" = "suffix",
                                       "Prefix" = "prefix"),
                          selected = "suffix"),
              textInput("seq_sep", "Separator",
                        value = "_", placeholder = "_")
            ),
            div(class = "fr-row",
              numericInput("seq_start", "Start at",
                           value = 1, min = 0, step = 1),
              numericInput("seq_width", "Min digits",
                           value = 3, min = 1, max = 6, step = 1)
            )
          )
        )
      ),

      # STEP 3
      div(class = "fr-sec", "③ Preview & download"),
      actionButton("apply_btn", "👁  Preview new names",
                   class = "btn-primary")
    ),

    # ── Main panel ──────────────────────────────────────
    div(class = "fr-main",
      uiOutput("welcome_or_results")
    )
  ),

  # ── Footer ──────────────────────────────────────────────
  div(class = "fr-footer",
    span("FileRenamer · LADAL · University of Queensland"),
    tags$a("ladal.edu.au", href = "https://ladal.edu.au"),
    tags$a("String Processing Tutorial",
           href = "https://ladal.edu.au/tutorials/string/string.html"),
    tags$a("Cite this tool",
           href = "https://ladal.edu.au/about.html#citing")
  ),
  CITATION_FOOTER,

  # JS: highlight op cards when their checkbox is ticked
  tags$script(HTML("
    $(document).on('shiny:inputchanged', function(e) {
      var map = {
        'use_findreplace': 'card_fr',
        'use_remove':      'card_rm',
        'use_case':        'card_case',
        'use_date':        'card_date',
        'use_affix':       'card_affix',
        'use_seq':         'card_seq'
      };
      if (map[e.name] !== undefined) {
        var card = $('#' + map[e.name]);
        if (e.value) card.addClass('active');
        else         card.removeClass('active');
      }
    });
  "))
)

# ── Server ───────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive: build preview on button click ────────────
  preview_data <- eventReactive(input$apply_btn, {
    req(input$files)
    orig  <- input$files$name
    ops   <- collect_ops(input)
    new_n <- build_new_names(orig, ops)

    changed  <- new_n != orig
    dup_new  <- duplicated(new_n) | duplicated(new_n, fromLast = TRUE)

    data.frame(
      `Original name`  = orig,
      `New name`       = new_n,
      Changed          = changed,
      Conflict         = dup_new,
      datapath         = input$files$datapath,
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  })

  # ── Upload status badge ────────────────────────────────
  output$upload_status <- renderUI({
    if (is.null(input$files)) {
      div(class = "fr-warn", "⚠ No files uploaded yet.")
    } else {
      n <- nrow(input$files)
      div(class = "fr-ok",
          paste0("✔ ", n, " file", if (n != 1) "s" else "", " loaded"))
    }
  })

  # ── Main area: welcome or results ─────────────────────
  output$welcome_or_results <- renderUI({

    if (input$apply_btn == 0 || is.null(input$files)) {
      return(
        div(class = "fr-welcome",
          div(class = "fr-welcome-icon", "✏️"),
          tags$h3("Rename your files in seconds"),
          tags$p(
            "Upload any files using the panel on the left,
             switch on one or more rename operations, then
             click ", tags$b("Preview new names"), ".", br(), br(),
            tags$b("Green"), " = renamed · ",
            tags$b("Grey"),  " = unchanged · ",
            tags$b(style = "color:#c0392b;", "Red"),
            " = duplicate conflict.", br(), br(),
            "Once the preview looks right, click ",
            tags$b("Download renamed files (.zip)"), ".",
            br(), br(),
            tags$em(style = "font-size:.82rem;",
              "File contents are never read or modified.
               Only names change.")
          )
        )
      )
    }

    df          <- preview_data()
    n_changed   <- sum(df$Changed)
    n_unchanged <- sum(!df$Changed)
    n_conflicts <- sum(df$Conflict)

    tagList(

      # ── Stat chips ────────────────────────────────
      div(class = "fr-chips",
        div(class = "fr-chip",
          tags$b(nrow(df)), " file", if (nrow(df) != 1) "s"),
        div(class = "fr-chip",
          tags$b(style = "color:#27ae60;", n_changed), " renamed"),
        div(class = "fr-chip",
          tags$b(n_unchanged), " unchanged"),
        if (n_conflicts > 0)
          div(class = "fr-chip",
            tags$b(style = "color:#c0392b;", n_conflicts),
            " conflict", if (n_conflicts != 1) "s")
      ),

      # ── Conflict warning ──────────────────────────
      if (n_conflicts > 0)
        div(class = "fr-conflict-badge",
          tags$b("⚠ Duplicate name conflict: "),
          n_conflicts,
          " file", if (n_conflicts != 1) "s",
          " would share the same name (shown in red). ",
          "Adjust your settings and re-preview before downloading.")
      ,

      # ── Preview header + download button ──────────
      div(class = "fr-preview-header",
        span("Preview"),
        if (n_conflicts == 0)
          downloadButton("dl_zip",
                         "⬇  Download renamed files (.zip)",
                         class = "fr-dl-btn")
        else
          tags$span(style = "font-size:.82rem; color:#c0392b;
                             font-weight:700;",
                    "Resolve conflicts before downloading")
      ),

      # ── Preview DT ────────────────────────────────
      DTOutput("preview_table")
    )
  })

  # ── Render preview table ───────────────────────────────
  output$preview_table <- renderDT({
    df <- preview_data()

    display <- df[, c("Original name", "New name"), drop = FALSE]

    # Build per-row colour vector for "New name" column
    name_colour <- ifelse(
      df$Conflict,  "#c0392b",
      ifelse(df$Changed, "#1a6b3c", "#aaaaaa")
    )
    name_weight <- ifelse(df$Changed | df$Conflict, "600", "400")

    dt <- datatable(
      display,
      rownames   = FALSE,
      escape     = FALSE,
      options    = list(
        dom        = "frtip",
        pageLength = 50,
        autoWidth  = FALSE,
        columnDefs = list(
          list(width = "50%", targets = 0),
          list(width = "50%", targets = 1)
        )
      )
    )

    # Style columns
    dt <- dt |>
      formatStyle(
        "Original name",
        color      = "#888",
        fontFamily = "IBM Plex Mono, monospace",
        fontSize   = ".85rem"
      ) |>
      formatStyle(
        "New name",
        fontFamily = "IBM Plex Mono, monospace",
        fontSize   = ".85rem"
      )

    # Apply per-row colours via JS callback
    dt$x$options$rowCallback <- DT::JS(sprintf("
      function(row, data, index) {
        var colours = %s;
        var weights = %s;
        $('td:eq(1)', row)
          .css('color',       colours[index])
          .css('font-weight', weights[index]);
      }
    ",
      jsonlite::toJSON(name_colour),
      jsonlite::toJSON(name_weight)
    ))

    dt
  })

  # ── ZIP download ──────────────────────────────────────
  output$dl_zip <- downloadHandler(
    filename = function() {
      paste0("renamed_files_", Sys.Date(), ".zip")
    },
    content = function(file) {
      df      <- preview_data()
      tmp_dir <- tempfile("filerename_")
      dir.create(tmp_dir)

      # Copy each file to the temp dir under its new name
      mapply(function(src, dest_name) {
        file.copy(src, file.path(tmp_dir, dest_name))
      }, df$datapath, df$`New name`)

      zip::zip(
        zipfile = file,
        files   = df$`New name`,
        root    = tmp_dir
      )
    }
  )
}

# ── Run ──────────────────────────────────────────────────────────────
  # ── Parameters download ─────────────────────────────────────────
  output$dl_params <- downloadHandler(
    filename = function() paste0("filerenamer_params_", Sys.Date(), ".txt"),
    content  = function(file) {
      lines <- c(
        paste0("Tool:                ", "FileRenamer — Batch File Renaming"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("---                  ", ""),
        paste0("Find & replace:      ", as.character(isTRUE(input$use_findreplace))),
        paste0("  Find:              ", input$find),
        paste0("  Replace with:      ", input$replace),
        paste0("Remove pattern:      ", as.character(isTRUE(input$use_remove))),
        paste0("  Pattern:           ", input$remove_pattern),
        paste0("Change case:         ", as.character(isTRUE(input$use_case))),
        paste0("  Case type:         ", input$case_type),
        paste0("Add prefix:          ", input$prefix),
        paste0("Add suffix:          ", input$suffix),
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
        paste0("Tool:                ", "FileRenamer — Batch File Renaming"),
        paste0("Date:                ", as.character(Sys.time())),
        paste0("R version:           ", R.version$version.string),
        paste0("---                  ", ""),
        paste0("Find & replace:      ", as.character(isTRUE(input$use_findreplace))),
        paste0("  Find:              ", input$find),
        paste0("  Replace with:      ", input$replace),
        paste0("Remove pattern:      ", as.character(isTRUE(input$use_remove))),
        paste0("  Pattern:           ", input$remove_pattern),
        paste0("Change case:         ", as.character(isTRUE(input$use_case))),
        paste0("  Case type:         ", input$case_type),
        paste0("Add prefix:          ", input$prefix),
        paste0("Add suffix:          ", input$suffix),
    ), collapse="\n")
  })

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
      "Schweinberger, Martin. (2025). ",
      "<em>FileRenamer: A browser-based batch file renaming tool</em>. ",
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
        "@misc{schweinberger2025filerenamer,\n",
        "  author       = {Schweinberger, Martin},\n",
        "  title        = {FileRenamer: A browser-based batch file renaming tool},\n",
        "  year         = {2025},\n",
        "  organization = {The University of Queensland},\n",
        "  url          = {https://ladal.edu.au/tools.html}\n",
        "}"
      )
    )
  )
)

shinyApp(ui, server)

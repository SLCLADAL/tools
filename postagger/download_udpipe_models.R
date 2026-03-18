#!/usr/bin/env Rscript
# ============================================================
#  download_udpipe_models.R
#
#  Pre-downloads the 11 bundled language groups (29 models)
#  for UDPTagger into /srv/udpipe-models at Binder build time.
#
#  Add to your postBuild file:
#    Rscript tools/udptagger/download_udpipe_models.R
# ============================================================

library(udpipe)

MODEL_DIR <- "/srv/udpipe-models"
dir.create(MODEL_DIR, showWarnings = FALSE, recursive = TRUE)

# Models to pre-download
MODELS <- c(
  # Arabic
  "arabic-padt",
  # Chinese
  "chinese-gsd", "chinese-gsdsimp",
  # Dutch
  "dutch-alpino", "dutch-lassysmall",
  # English
  "english-ewt", "english-gum", "english-lines", "english-partut",
  # French
  "french-gsd", "french-partut", "french-sequoia", "french-spoken",
  # German
  "german-gsd", "german-hdt",
  # Italian
  "italian-isdt", "italian-partut", "italian-postwita",
  "italian-twittiro", "italian-vit",
  # Japanese
  "japanese-gsd",
  # Portuguese
  "portuguese-bosque", "portuguese-br", "portuguese-gsd",
  # Russian
  "russian-gsd", "russian-syntagrus", "russian-taiga",
  # Spanish
  "spanish-ancora", "spanish-gsd"
)

cat("Downloading", length(MODELS), "udpipe models to", MODEL_DIR, "\n\n")

results <- lapply(MODELS, function(lang) {
  # Skip if already downloaded
  existing <- list.files(MODEL_DIR,
                         pattern = paste0("^", lang, ".*\\.udpipe$"))
  if (length(existing) > 0) {
    cat("  [skip] ", lang, "— already present\n")
    return(invisible(NULL))
  }

  cat("  [download] ", lang, "... ")
  tryCatch({
    dl <- udpipe_download_model(
      language  = lang,
      model_dir = MODEL_DIR
    )
    if (dl$download_failed) {
      cat("FAILED:", dl$download_message, "\n")
    } else {
      cat("OK\n")
    }
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
})

downloaded <- list.files(MODEL_DIR, pattern = "\\.udpipe$")
cat("\n✔ Models available in", MODEL_DIR, ":", length(downloaded), "\n")
cat(paste0("  ", downloaded, collapse = "\n"), "\n")

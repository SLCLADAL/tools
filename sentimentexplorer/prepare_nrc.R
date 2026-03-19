#!/usr/bin/env Rscript
# ============================================================
#  prepare_nrc.R
#
#  Converts the raw NRC Word-Emotion Association Lexicon text
#  file into the nrc_lexicon.csv used by SentimentExplorer.
#
#  Run ONCE before deploying:
#    Rscript sentimentexplorer/prepare_nrc.R
#
#  The raw lexicon file must be downloaded from:
#    https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#  or requested from: saif.mohammad@nrc-cnrc.gc.ca
#
#  Citation:
#    Mohammad, S.M. & Turney, P.D. (2013). Crowdsourcing a
#    Word-Emotion Association Lexicon. Computational Intelligence,
#    29(3): 436-465. https://doi.org/10.1111/j.1467-8640.2012.00460.x
# ============================================================

# Expected raw file location (tab-separated, no header):
#   Column 1: word
#   Column 2: emotion/sentiment category
#   Column 3: association (1 = yes, 0 = no)
raw_file <- file.path(dirname(sys.frame(1)$ofile),
                      "NRC-Emotion-Lexicon-Wordlevel-v0.92.txt")

if (!file.exists(raw_file)) {
  stop(
    "Raw NRC lexicon file not found at: ", raw_file, "\n",
    "Please download it from https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm\n",
    "and place it in the sentimentexplorer/ directory."
  )
}

cat("Reading raw NRC lexicon...\n")
raw <- read.table(raw_file,
                  header    = FALSE,
                  sep       = "\t",
                  quote     = "",
                  col.names = c("word", "sentiment", "value"),
                  encoding  = "UTF-8")

cat("Raw rows:", nrow(raw), "\n")

# Keep only positive associations
nrc <- raw[raw$value == 1, c("word", "sentiment")]

cat("Positive associations:", nrow(nrc), "\n")
cat("Categories:", paste(sort(unique(nrc$sentiment)), collapse=", "), "\n")

out_file <- file.path(dirname(sys.frame(1)$ofile), "nrc_lexicon.csv")
write.csv(nrc, out_file, row.names = FALSE)
cat("Written:", out_file, "\n")
cat("Done.\n")

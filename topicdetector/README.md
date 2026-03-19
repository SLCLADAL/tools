# 📊 TopicDetector

**Unsupervised & seeded LDA topic modelling — LADAL**  
[https://ladal.edu.au](https://ladal.edu.au) · University of Queensland

---

## Overview

TopicDetector is a browser-based Shiny tool for discovering and labelling topics in a collection of plain-text documents. It implements a two-stage iterative workflow: first an **unsupervised LDA** to explore what topics are present in the corpus, then a **seeded LDA** that uses a user-defined seed dictionary to produce interpretable, labelled topic assignments for every document.

No R installation is required — the tool runs entirely in the browser via Binder.

> **Note:** TopicDetector works best when each uploaded file represents a coherent unit of text — a paragraph, an interview excerpt, a document section, or a short article. Very short texts (single sentences) and very long texts (entire books) tend to produce less coherent topics.

---

## Workflow

TopicDetector is designed to be used **iteratively** across two stages.

### Stage 1 — Explore with unsupervised LDA

The goal of this stage is to discover what topics are present and how many make sense for the corpus. You will typically run this several times with different values of *k* before moving to Stage 2.

1. Upload your `.txt` files
2. Set preprocessing options (stopword language, stemming, frequency thresholds)
3. Choose a number of topics (*k*) and click **Run unsupervised LDA**
4. Examine the top-terms table — do the terms within each topic cluster around a coherent theme?
5. Adjust *k* and re-run until the topics look stable and meaningful

The top terms from each topic are automatically used to pre-fill the seed dictionary for Stage 2.

### Stage 2 — Label with seeded LDA

Once you have a good sense of the topics, use the seed dictionary to guide the model toward interpretable, named topics.

1. Review the seed dictionary cards in the sidebar — each card shows the topic name and the seed words pre-filled from Stage 1
2. Edit topic names to reflect your interpretation (e.g. `Topic01` → `Technology`)
3. Add, remove, or replace seed words to refine each topic
4. Click **Run seeded LDA**
5. Explore results across four output tabs
6. Download tables and plots

The seeded model always includes an additional **other** topic (residual) for content that does not match any seed topic well.

---

## Text preprocessing

All text is preprocessed automatically before LDA. The following steps are applied in order:

1. **Lowercasing** — all tokens converted to lowercase
2. **Punctuation, symbols, numbers, URLs removed**
3. **Non-alphabetic tokens removed** — tokens containing no letters are discarded
4. **Single-character tokens removed**
5. **Stopword removal** — optional; choose a language from the dropdown (English default)
6. **Stemming** — optional; reduces words to their base form using the Snowball algorithm (`SnowballC`, English). For example: *running* → *run*, *topics* → *topic*, *government* → *govern*. Stemming improves topic coherence by merging inflected forms.
7. **Frequency trimming** — terms appearing fewer than *min. term freq.* times in the corpus, or in fewer than *min. doc. freq.* documents, are removed from the DFM before modelling.

### Frequency thresholds

| Setting | Default | Effect |
|---------|---------|--------|
| Min. term frequency | 2 | Removes very rare terms that add noise |
| Min. doc. frequency | 2 | Removes terms that appear in only one document |

Raising these thresholds reduces the vocabulary size and speeds up LDA, but may remove meaningful low-frequency terms. Lower thresholds keep more vocabulary but increase computation time.

---

## Unsupervised LDA settings

| Setting | Default | Description |
|---------|---------|-------------|
| **k (topics)** | 5 | Number of topics to extract. Try values from 3 to 15 and look for stable, non-overlapping topic clusters. |
| **Random seed** | 1234 | Controls LDA initialisation for reproducibility. Change this to check whether topics are stable across different random starts. |
| **Top terms shown** | 10 | How many top terms per topic appear in the results table. |

### Choosing k

There is no single correct value of *k* — it depends on the corpus and the analytical goals. Practical guidance:

- Start with `k = 5` and look at whether the top terms within each topic are thematically coherent
- If topics overlap heavily (same words appear in multiple topics), try reducing *k*
- If topics seem too broad, try increasing *k*
- Run the same *k* with different random seeds — if topics look completely different, the model is unstable; try adjusting preprocessing or *k*
- Typical useful range for small-to-medium corpora: *k* = 3–10

---

## Seed dictionary

After the unsupervised LDA, the sidebar shows one card per topic. Each card contains:

- **Name** — an editable label for the topic (e.g. `Technology`, `Education`, `Finance`)
- **Seed words** — a comma-separated list of words that define the topic

The seed words are pre-filled with the top terms from the corresponding unsupervised topic. You should:

1. Rename each topic to reflect the theme you see in the top terms
2. Review the seed words — remove any that seem off-topic or ambiguous
3. Add your own words based on domain knowledge (these do not need to appear in the top terms)
4. Ensure each topic has at least one seed word

The seeded model will be more interpretable when seed words are distinctive — words that appear in one topic should not appear in others.

---

## Seeded LDA outputs

### 📋 Top terms tab

A table showing the top *N* terms for each seeded topic, including the residual **other** topic. Terms are ranked by their probability within each topic (the *beta* parameter).

This table is downloadable as Excel or CSV.

### 📄 Document assignments tab

One row per uploaded document. Columns:

| Column | Description |
|--------|-------------|
| **Document** | File name (without extension) |
| **BestTopic** | The topic with the highest probability for this document |
| **[topic name]** | Probability (0–1) of each topic for this document |

The probabilities in each row sum to 1. The colour bars in the probability columns give a quick visual overview of how strongly each document is associated with each topic. Documents assigned to the **other** topic are those whose content did not match any seed topic sufficiently.

This table is downloadable as Excel or CSV.

### 📊 Topic frequency chart

A horizontal bar chart showing how many documents (and what percentage) are assigned to each topic as their best-fitting topic. Bars are sorted by frequency (most common topic at top). Each bar is labelled with both the count and the percentage.

Downloadable as PNG or PDF.

### 🌡️ Topic heatmap

A heatmap with documents as rows and topics as columns. Cell colour encodes the topic probability — light purple = low probability, dark purple = high probability. Documents are sorted by their dominant topic, so rows with similar topic profiles cluster together.

The heatmap height scales automatically with the number of documents. Downloadable as PNG or PDF (dimensions also scale with corpus size).

---

## Downloads

| Output | Formats |
|--------|---------|
| Top terms table (seeded LDA) | Excel (.xlsx), CSV (.csv) |
| Document assignments table | Excel (.xlsx), CSV (.csv) |
| Topic frequency bar chart | PNG, PDF |
| Topic probability heatmap | PNG, PDF |

---

## Tips for good results

- **Use paragraph-level documents.** LDA works best when each file represents a coherent chunk of text — not a single sentence, and not an entire book. Paragraphs, interview turns, or document sections tend to work well.
- **Start with English stopword removal.** Most corpora benefit from removing common function words before modelling.
- **Enable stemming.** For most English corpora, stemming meaningfully improves topic coherence by merging *run*, *runs*, *running*, and *ran* into a single feature.
- **Iterate on k.** There is no shortcut — run the unsupervised LDA at several values of *k* and compare. Look for the *k* where top terms feel most thematically tight.
- **Make seed words distinctive.** The seeded model performs best when each topic's seed words are unique to that topic. If the same word appears as a seed term for two topics, remove it from both.
- **Use the residual topic diagnostically.** A large *other* topic suggests your seed topics are not covering the corpus well — consider adding more topics or broadening their seed words.

---

## Deployment

### Repository placement

```
SLCLADAL/tools/
└── topicdetector/
    └── app.R
```

### Port

TopicDetector runs on port **3846**.

### Launcher notebook

Use `topicdetector_launcher.ipynb` in the `SLCLADAL/tools` root. The polling timeout is set to **120 seconds** (rather than the standard 90) because `topicmodels` takes slightly longer to initialise on first launch.

**ARDC BinderHub (recommended for AU/NZ institutions):**
```
https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ftopicdetector_launcher.ipynb%26branch%3Dmain
```

**MyBinder.org (open access):**
```
https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ftopicdetector_launcher.ipynb%26branch%3Dmain
```

### R dependencies

Add the following to `tools-env/install.R` (not yet present):

| Package | Role |
|---------|------|
| `topicmodels` | Unsupervised LDA via `LDA()` |
| `seededlda` | Seeded/semi-supervised LDA via `textmodel_seededlda()` |
| `SnowballC` | English stemming via `wordStem()` |
| `tidytext` | Tidying LDA output (beta/gamma matrices) via `tidy()` |

Packages already in `install.R` used by this tool: `shiny`, `quanteda`, `quanteda.textstats`, `dplyr`, `ggplot2`, `tibble`, `readr`, `writexl`, `DT`.

---

## Technical notes

- **No `tidyverse` at startup.** Only the individual packages actually needed (`dplyr`, `ggplot2`, `tibble`, `readr`) are loaded, keeping startup time short.
- **All computation on button click.** Neither the DFM build nor the LDA runs until the user explicitly clicks the run button — slider and dropdown changes do not trigger recomputation.
- **Empty document handling.** Documents that become empty after preprocessing (all tokens removed by stopword or frequency filters) are silently dropped before LDA. A stat chip shows how many documents entered the model.
- **Seed words and stemming.** If stemming is enabled, seed words are applied to the stemmed DFM. This means seed words should ideally be entered in their stemmed form (e.g. `govern` not `government`, `technolog` not `technology`) for best results. The tool does not auto-stem seed words — this is intentional so users retain full control.
- **Residual topic.** `residual = TRUE` adds one extra topic beyond the user-defined ones. Documents whose content does not match any seed topic will tend to accumulate here.

---

## Citation

If you use TopicDetector in your research, please cite it as:

> Schweinberger, Martin. (2024). *TopicDetector: A browser-based topic modelling tool*. Brisbane: The University of Queensland. Language Technology and Data Analysis Laboratory (LADAL). Retrieved from https://ladal.edu.au/tools.html

```bibtex
@manual{schweinberger2024topicdetector,
  author       = {Schweinberger, Martin},
  title        = {TopicDetector: A browser-based topic modelling tool},
  year         = {2024},
  organization = {The University of Queensland, School of Languages and Cultures},
  address      = {Brisbane},
  url          = {https://ladal.edu.au/tools.html}
}
```

---

## References

- Blei, D.M., Ng, A.Y., & Jordan, M.I. (2003). Latent Dirichlet Allocation. *Journal of Machine Learning Research*, 3, 993–1022.
- Lu, B., Ott, M., Cardie, C., & Tsou, B.K. (2011). Multi-aspect sentiment analysis with topic models. In *Proceedings of ICDM Workshops*.
- Silge, J. & Robinson, D. (2017). Topic modeling. In *Text Mining with R*. O'Reilly. [https://www.tidytextmining.com](https://www.tidytextmining.com)

---

## Links

- [LADAL Topic Modelling Tutorial](https://ladal.edu.au/tutorials/topicmodels/topicmodels.html)
- [LADAL tools page](https://ladal.edu.au/tools.html)
- [Report an issue](https://github.com/SLCLADAL/tools/issues)

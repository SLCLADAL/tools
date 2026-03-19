# 🔗 CollocationCalculator

**Collocation association measures — LADAL**  
[https://ladal.edu.au](https://ladal.edu.au) · University of Queensland

---

## Overview

CollocationCalculator is a browser-based Shiny tool for calculating collocational association measures from your own plain-text files. Upload one or more `.txt` files, specify one or more node words, set an asymmetric collocation window, and the tool computes nine association measures for every (node, collocate) pair found in the corpus. Results can be explored interactively, sorted by any measure, and downloaded as Excel or CSV. The bar chart can be downloaded as PNG or PDF.

All nine measures are computed from scratch using a vectorised `data.table` engine with `stringi` tokenisation — no external association measure package is required.

---

## Features

- Accepts any number of `.txt` files; all files are concatenated into one corpus
- Multiple node words analysed together in a single combined table
- Asymmetric collocation window (left and right spans set independently)
- Nine association measures computed in one pass
- Minimum co-occurrence frequency filter
- Top N collocates per node for the bar chart
- User-selectable sort measure for the plot
- Case-insensitive mode (default) or case-sensitive toggle
- Bar chart faceted by node word — PNG and PDF download
- Full results table — Excel and CSV download
- Built-in measure guide with definitions and key variable descriptions

---

## Workflow

1. **Upload** one or more `.txt` files (plain text, UTF-8 recommended)
2. **Enter node word(s)** — comma-separated (e.g. `climate, change, policy`)
3. **Set window** — left and right spans independently (default: L5 / R5)
4. **Set filters** — minimum co-occurrence count and top N for the plot
5. **Choose plot measure** — the measure used to rank bars in the chart
6. **Click Calculate** — results appear across three tabs
7. **Download** — bar chart (PNG/PDF) and/or results table (Excel/CSV)

---

## Association measures

All measures are based on a 2×2 contingency table of observed and expected co-occurrences within the collocation window. The table is built per (node, collocate) pair across the full concatenated corpus.

### Key variables

| Symbol | Meaning |
|--------|---------|
| **O** | Observed co-occurrence count (within window) |
| **E** | Expected count: (f_node × f_collocate) / N |
| **f_node** | Corpus frequency of the node word |
| **f_collocate** | Corpus frequency of the collocate |
| **N** | Total corpus size in tokens |

### Measures

| Measure | Formula | Notes |
|---------|---------|-------|
| **O/E** | O / E | Raw ratio of observed to expected; values > 1 indicate attraction |
| **MI** | log₂(O / E) | Mutual Information; sensitive to low-frequency pairs |
| **MI2** | log₂(O² / E) | Squares the observed count; reduces MI's low-frequency bias |
| **MI3** | log₂(O³ / E) | Cubes the observed count; further reduces low-frequency bias |
| **G2** | Full 4-cell log-likelihood ratio | Robust frequency-sensitive measure; recommended for large corpora |
| **t-score** | (O − E) / √O | Frequency-sensitive; favours high-frequency collocates |
| **ΔP12** | P(collocate\|node) − P(collocate\|¬node) | Directional: how strongly the node predicts the collocate |
| **ΔP21** | P(node\|collocate) − P(node\|¬collocate) | Directional: how strongly the collocate predicts the node |
| **Fisher p** | Two-tailed Fisher exact p-value | Significance test; lower = stronger association |

### Choosing a measure

- **MI / MI2 / MI3** — good for identifying phraseologically tight combinations; MI over-weights rare pairs, MI2/MI3 correct for this
- **G2** — the most statistically robust choice for large corpora; penalises very low observed counts
- **t-score** — tends to favour high-frequency, grammatically predictable collocates (function words)
- **ΔP12** — best when you want to know what words the node word attracts (node-centric)
- **ΔP21** — best when you want to know what words are strongly tied to the node (collocate-centric)
- **Fisher p** — use when you want a significance threshold rather than a strength ranking

> **Reference:** Gries, S.Th. (2013). *Statistics for Linguistics with R* (2nd ed.). De Gruyter Mouton.

---

## Outputs

### 📊 Bar chart tab

A faceted horizontal bar chart — one panel per node word, bars sorted by the chosen measure, showing the top N collocates. Each bar is labelled with its rounded measure value. The chart is downloadable as PNG or PDF; dimensions scale automatically with the number of node words and top N.

### 📋 Results table tab

A fully interactive table showing every (node, collocate) pair that meets the minimum co-occurrence threshold, with all nine measures as columns. Columns can be sorted by clicking the header; the table also supports per-column filtering. Colour bars on the MI / ΔP columns give a quick visual overview of association strength.

### ℹ️ Measure guide tab

A quick-reference table of all nine measure definitions and a glossary of the key variables used in the formulas.

---

## Window and filtering settings

### Collocation window

The window defines which tokens around each node occurrence are counted as potential collocates. CollocationCalculator uses an **asymmetric window**: the left span (tokens to the left of the node) and right span (tokens to the right) are set independently.

| Setting | Default | Range | Effect |
|---------|---------|-------|--------|
| Left span | 5 | 1–20 | Tokens to the left of each node occurrence |
| Right span | 5 | 1–20 | Tokens to the right of each node occurrence |

Narrower windows (e.g. L2/R2) capture tight syntactic collocates; wider windows (e.g. L10/R10) capture more topical or thematic associations.

### Filters

| Setting | Default | Effect |
|---------|---------|--------|
| Min. co-occurrence count | 2 | Collocate pairs with fewer than this many joint occurrences are excluded from all outputs |
| Top N (plot) | 20 | Only the top N collocates per node (by the chosen sort measure) appear in the bar chart; the full table always shows all pairs meeting the min. frequency threshold |

---

## Case sensitivity

By default the tool operates in **case-insensitive** mode: all tokens and node words are lowercased before processing. This is the recommended setting for most analyses. Toggle **Ignore case** off in the sidebar to preserve original capitalisation (useful for proper nouns or language-specific case distinctions).

When case-insensitive mode is active, node words entered in any combination of upper and lower case are automatically lowercased before matching.

---

## Technical notes

- **Tokenisation** uses `stringi::stri_extract_all_words` — Unicode-safe, handles accented characters and non-ASCII scripts correctly
- **Frequency counts** use `data.table` keyed lookups for O(log n) performance
- **Window extraction** is fully vectorised per node occurrence — no R-level row-by-row loops
- **Fisher's exact test** uses `simulate.p.value = TRUE` for contingency tables with cell totals above 2,000 to avoid excessive computation time
- All files are concatenated into a **single token vector** before analysis; document boundaries are not preserved
- The token vector is built once per upload session and cached — changing node words or window settings does not re-read the files

---

## Deployment

### Repository placement

```
SLCLADAL/tools/
└── collocationcalculator/
    └── app.R
```

### Port

CollocationCalculator runs on port **3845**.

### Launcher notebook

Use `collocationcalculator_launcher.ipynb` in the `SLCLADAL/tools` root. The Binder launch URLs are:

**ARDC BinderHub (recommended for AU/NZ institutions):**
```
https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fcollocationcalculator_launcher.ipynb%26branch%3Dmain
```

**MyBinder.org (open access):**
```
https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fcollocationcalculator_launcher.ipynb%26branch%3Dmain
```

### R dependencies

All dependencies are already present in `tools-env/install.R`:

| Package | Role |
|---------|------|
| `shiny` | Web application framework |
| `data.table` | Fast vectorised frequency counting and window operations |
| `stringi` | Unicode-safe tokenisation |
| `ggplot2` | Bar chart (part of `tidyverse`) |
| `writexl` | Excel download |
| `DT` | Interactive results table |

No changes to `install.R` or `postBuild` are required.

---

## Citation

If you use CollocationCalculator in your research, please cite it as:

> Schweinberger, Martin. (2024). *CollocationCalculator: A browser-based collocation analysis tool*. Brisbane: The University of Queensland. Language Technology and Data Analysis Laboratory (LADAL). Retrieved from https://ladal.edu.au/tools.html

```bibtex
@manual{schweinberger2024collocationcalculator,
  author       = {Schweinberger, Martin},
  title        = {CollocationCalculator: A browser-based collocation analysis tool},
  year         = {2024},
  organization = {The University of Queensland, School of Languages and Cultures},
  address      = {Brisbane},
  url          = {https://ladal.edu.au/tools.html}
}
```

---

## Links

- [LADAL Collocation & N-gram Tutorial](https://ladal.edu.au/tutorials/coll/coll.html)
- [LADAL tools page](https://ladal.edu.au/tools.html)
- [Report an issue](https://github.com/SLCLADAL/tools/issues)

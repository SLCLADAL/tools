# LADAL Tools

**Browser-based text analysis tools — no installation required**  
[Language Technology and Data Analysis Laboratory (LADAL)](https://ladal.edu.au) · University of Queensland

---

## Overview

This repository contains the source code for the LADAL interactive tools suite — a collection of browser-based [Shiny](https://shiny.posit.co/) applications for linguistic and text analysis. Each tool runs entirely in the browser via [Binder](https://mybinder.org/); no local installation of R or any other software is needed.

Tools are launched through a per-tool Jupyter notebook (the *launcher*) which starts the Shiny app on a fixed port and opens it automatically in a new browser tab.

> **File contents are never stored.** Uploaded files exist only for the duration of a browser session and are discarded when the session ends.

---

## Quick-launch table

Click a launch button to open a tool directly. **ARDC BinderHub** is recommended for users at Australian and New Zealand research institutions (requires [AAF](https://aaf.edu.au/) or [Tuakiri](https://www.reannz.co.nz/products-and-services/tuakiri) login). **MyBinder.org** is open to everyone with no login required.

| Tool | Description | ARDC BinderHub ⭐ | MyBinder.org |
|------|-------------|:-----------------:|:------------:|
| 🔍 **WordFinder** | KWIC concordancing | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordfinder_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordfinder_launcher.ipynb%26branch%3Dmain) |
| 🔑 **KeywordExtractor** | Keyness analysis | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fkeywordextractor_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fkeywordextractor_launcher.ipynb%26branch%3Dmain) |
| 🕸️ **WordWebber** | Word co-occurrence networks | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordwebber_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordwebber_launcher.ipynb%26branch%3Dmain) |
| 🏷️ **POSTagger** | POS tagging & dependency parsing (65+ languages) | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fpostagger_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fpostagger_launcher.ipynb%26branch%3Dmain) |
| 🧹 **TextCleaner** | Text cleaning with regex | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ftextcleaner_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ftextcleaner_launcher.ipynb%26branch%3Dmain) |
| ✏️ **FileRenamer** | Batch file renaming | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ffilerenamer_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ffilerenamer_launcher.ipynb%26branch%3Dmain) |
| 💬 **SentimentExplorer** | NRC word-emotion sentiment analysis | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fsentimentexplorer_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fsentimentexplorer_launcher.ipynb%26branch%3Dmain) |
| 🔗 **CollocationCalculator** | Collocation association measures | [Launch](https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fcollocationcalculator_launcher.ipynb%26branch%3Dmain) | [Launch](https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fcollocationcalculator_launcher.ipynb%26branch%3Dmain) |

---

## Tools

### 🔍 WordFinder — KWIC Concordancing

Search for words, phrases, or regex patterns across a collection of plain-text files and explore results as keyword-in-context (KWIC) concordance lines. Results include a frequency-by-document table, a lexical dispersion plot, and Excel/CSV download.

- **Input:** one or more `.txt` files
- **Port:** 3838
- **Tutorial:** [Concordancing Tutorial](https://ladal.edu.au/tutorials/kwics/kwics.html)

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordfinder_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordfinder_launcher.ipynb%26branch%3Dmain` |

---

### 🔑 KeywordExtractor — Keyness Analysis

Compare vocabulary between a target corpus and a reference corpus to identify words that are statistically over- or under-represented. Supports multiple keyness measures and produces downloadable results tables.

- **Input:** target `.txt` files + reference `.txt` files
- **Port:** 3839
- **Tutorial:** [Keyness and Keyword Analysis Tutorial](https://ladal.edu.au/tutorials/key/key.html)

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fkeywordextractor_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fkeywordextractor_launcher.ipynb%26branch%3Dmain` |

---

### 🕸️ WordWebber — Word Co-occurrence Networks

Build and visualise interactive word co-occurrence networks from plain-text files. Texts are automatically lowercased and cleaned; optional stopword removal and English lemmatisation (via `textstem`) are available. Node size reflects each word's overall corpus frequency; edge thickness reflects MI score (collocational attraction strength). Both an interactive network (via `visNetwork`) and a static publication-ready plot are produced. Networks and co-occurrence tables can be downloaded as PNG, Excel, and CSV.

- **Input:** one or more `.txt` files
- **Port:** 3840
- **Tutorial:** [Network Analysis Tutorial](https://ladal.edu.au/tutorials/net/net.html)

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordwebber_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fwordwebber_launcher.ipynb%26branch%3Dmain` |

---

### 🏷️ POSTagger — POS Tagging & Dependency Parsing

Annotate plain-text files with Universal Dependencies part-of-speech tags, lemmas, and dependency relations using [UDPipe](https://ufal.mff.cuni.cz/udpipe). Supports 65+ languages. Pre-bundled models for 29 common treebanks start instantly; all others download automatically on first use.

- **Input:** one or more `.txt` files
- **Port:** 3841
- **Tutorial:** [POS Tagging Tutorial](https://ladal.edu.au/tutorials/postag/postag.html)

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fpostagger_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fpostagger_launcher.ipynb%26branch%3Dmain` |

---

### 🧹 TextCleaner — Regex-based Text Cleaning

Remove or replace words, tags, and patterns across a collection of plain-text files using regular expressions. Supports multiple find-and-replace rules applied in sequence, with a live preview before download.

- **Input:** one or more `.txt` files
- **Port:** 3842
- **Tutorial:** [String Processing Tutorial](https://ladal.edu.au/tutorials/string/string.html)

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ftextcleaner_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ftextcleaner_launcher.ipynb%26branch%3Dmain` |

---

### ✏️ FileRenamer — Batch File Renaming

Rename large batches of files in the browser. Six composable operations — find & replace, remove substrings, change case, reformat date patterns, add prefix/suffix, and sequential numbering — are applied in a fixed order with a live colour-coded preview. Renamed files are delivered as a ZIP archive. File contents are never read or modified.

- **Input:** any file type
- **Port:** 3843
- **README:** [filerenamer/README.md](filerenamer/README.md)

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ffilerenamer_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Ffilerenamer_launcher.ipynb%26branch%3Dmain` |

---

### 💬 SentimentExplorer — NRC Word-Emotion Sentiment Analysis

Annotate plain-text files with emotion and sentiment scores using the [NRC Word-Emotion Association Lexicon](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) (Mohammad & Turney, 2013). Upload files, group them into named sections (or keep each file separate), and explore results across three outputs: a per-token annotation table, a section-level summary, and a faceted bar chart showing the percentage and count of words associated with each of the 10 NRC categories (anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive).

- **Input:** one or more `.txt` files
- **Port:** 3844
- **Tutorial:** [Sentiment Analysis Tutorial](https://ladal.edu.au/tutorials/sentiment/sentiment.html)
- **Lexicon:** NRC EmoLex v0.92 — free for research use; commercial use requires permission from Saif M. Mohammad (saif.mohammad@nrc-cnrc.gc.ca). Please cite: Mohammad & Turney (2013), *Computational Intelligence*, 29(3): 436–465. [https://doi.org/10.1111/j.1467-8640.2012.00460.x](https://doi.org/10.1111/j.1467-8640.2012.00460.x)
- **Setup note:** the file `nrc_lexicon.csv` must be present in the `sentimentexplorer/` folder. Run `prepare_nrc.R` once to generate it from the raw NRC `.txt` file (see `sentimentexplorer/prepare_nrc.R` for instructions).

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fsentimentexplorer_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fsentimentexplorer_launcher.ipynb%26branch%3Dmain` |

---

### 🔗 CollocationCalculator — Collocation Association Measures

Calculate collocational association measures for one or more node words across uploaded plain-text files. All files are concatenated into a single corpus; an asymmetric window (left and right spans set independently) defines the co-occurrence context. Nine measures are computed in one vectorised pass using `data.table` and `stringi` for speed: O/E, MI, MI2, MI3, log-likelihood (G2), t-score, ΔP12, ΔP21, and Fisher's exact test p-value. Results are displayed in a faceted bar chart (sorted by a user-chosen measure) and a full interactive table, both downloadable.

- **Input:** one or more `.txt` files
- **Port:** 3845
- **Tutorial:** [Collocation & N-gram Tutorial](https://ladal.edu.au/tutorials/coll/coll.html)
- **README:** [collocationcalculator/README.md](collocationcalculator/README.md)

**Launch URLs**

| Host | URL |
|------|-----|
| ARDC BinderHub ⭐ | `https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fcollocationcalculator_launcher.ipynb%26branch%3Dmain` |
| MyBinder.org | `https://mybinder.org/v2/gh/SLCLADAL/tools-env/main?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools%26urlpath%3Dlab%252Ftree%252Ftools%252Fcollocationcalculator_launcher.ipynb%26branch%3Dmain` |

---

## Repository structure

```
tools/
├── wordfinder/
│   └── app.R                         # WordFinder Shiny app
├── keywordextractor/
│   └── app.R                         # KeywordExtractor Shiny app
├── wordwebber/
│   └── app.R                         # WordWebber Shiny app
├── postagger/
│   ├── app.R                         # POSTagger Shiny app
│   └── download_udpipe_models.R      # Pre-download script (called by postBuild)
├── textcleaner/
│   └── app.R                         # TextCleaner Shiny app
├── filerenamer/
│   ├── app.R                         # FileRenamer Shiny app
│   └── README.md                     # FileRenamer documentation
├── sentimentexplorer/
│   ├── app.R                         # SentimentExplorer Shiny app
│   ├── nrc_lexicon.csv               # NRC EmoLex (generate via prepare_nrc.R)
│   └── prepare_nrc.R                 # One-time script to build nrc_lexicon.csv
├── collocationcalculator/
│   ├── app.R                         # CollocationCalculator Shiny app
│   └── README.md                     # CollocationCalculator documentation
├── wordfinder_launcher.ipynb
├── keywordextractor_launcher.ipynb
├── wordwebber_launcher.ipynb
├── postagger_launcher.ipynb
├── textcleaner_launcher.ipynb
├── filerenamer_launcher.ipynb
├── sentimentexplorer_launcher.ipynb
└── collocationcalculator_launcher.ipynb
```

Each tool is a self-contained `app.R` file with no external `source()` dependencies. Launcher notebooks start the Shiny app on a fixed port and expose it through [`jupyter-server-proxy`](https://github.com/jupyterhub/jupyter-server-proxy).

---

## How the launcher pattern works

1. A Binder URL encodes the tool's launcher notebook path via `nbgitpuller`.
2. JupyterLab opens and the launcher notebook cell executes automatically (via the `start` script in [`SLCLADAL/tools-env`](https://github.com/SLCLADAL/tools-env)).
3. The cell starts the Shiny app with `subprocess.Popen` on the tool's fixed port.
4. The cell polls `localhost:<port>` until the app is ready (up to 90 seconds).
5. A green button appears linking to `{JUPYTERHUB_SERVICE_PREFIX}proxy/<port>/`.
6. The user clicks the button and the Shiny app opens in a new browser tab.

If the button does not appear within 90 seconds, re-run the cell manually with **Shift + Enter**.

---

## Port assignments

| Tool | Port |
|------|------|
| WordFinder | 3838 |
| KeywordExtractor | 3839 |
| WordWebber | 3840 |
| POSTagger | 3841 |
| TextCleaner | 3842 |
| FileRenamer | 3843 |
| SentimentExplorer | 3844 |
| CollocationCalculator | 3845 |

---

## Environment

The Binder environment is defined in the companion repository [`SLCLADAL/tools-env`](https://github.com/SLCLADAL/tools-env):

| File | Purpose |
|------|---------|
| `runtime.txt` | R version (`r-4.4.2-2024-10-31`) |
| `install.R` | R package installation |
| `requirements.txt` | Python packages (`nbgitpuller`, `jupyter-server-proxy==3.2.3`, `jupyter-shiny-proxy`) |
| `apt.txt` | System dependencies |
| `postBuild` | Pre-downloads UDPipe models to `~/udpipe-models` |
| `start` | Pre-executes the launcher notebook before JupyterLab opens |

### R packages

| Package | Used by |
|---------|---------|
| `shiny` | All tools |
| `DT` | All tools |
| `zip` | FileRenamer |
| `writexl` | WordFinder, KeywordExtractor, POSTagger, SentimentExplorer, CollocationCalculator, WordWebber |
| `tidyverse` | WordFinder, KeywordExtractor, POSTagger, TextCleaner, SentimentExplorer |
| `dplyr` | WordWebber (standalone; others get it via `tidyverse`) |
| `ggplot2` | WordWebber (standalone; others get it via `tidyverse`) |
| `tibble` | WordWebber (standalone; others get it via `tidyverse`) |
| `readr` | WordWebber, CollocationCalculator (CSV download) |
| `quanteda` | WordFinder, KeywordExtractor, WordWebber |
| `quanteda.textstats` | KeywordExtractor |
| `quanteda.textplots` | WordFinder (dispersion plot), WordWebber (static network) |
| `visNetwork` | WordWebber (interactive network) |
| `textstem` | WordWebber (optional English lemmatisation; loaded lazily) |
| `koRpus` | WordWebber (`textstem` dependency) |
| `sylly` | WordWebber (`koRpus` / `textstem` dependency) |
| `udpipe` | POSTagger |
| `stringi` | TextCleaner, FileRenamer, CollocationCalculator |
| `data.table` | TextCleaner, CollocationCalculator |
| `tidyr` | SentimentExplorer (pivot NRC lexicon; included in `tidyverse`) |

---

## Related repositories

| Repository | Purpose |
|------------|---------|
| [`SLCLADAL/tools-env`](https://github.com/SLCLADAL/tools-env) | Binder environment for this tools repo |
| [`SLCLADAL/interactive-notebooks`](https://github.com/SLCLADAL/interactive-notebooks) | Older notebook-based LADAL tools (separate — do not modify) |
| [`SLCLADAL/interactive-notebooks-environment`](https://github.com/SLCLADAL/interactive-notebooks-environment) | Binder environment for the notebook-based tools (separate — do not modify) |

---

## Reporting issues

If a tool fails to launch or behaves unexpectedly, please email Martin at [m.schweinberger@uq.edu.au](mailto:m.schweinberger@uq.edu.au). Please include the tool name, which launch option you used (ARDC BinderHub or MyBinder.org), and a brief description of what happened.

Alternatively, [open an issue](https://github.com/SLCLADAL/tools/issues) on this repository.

---

## Citation

If you use any of these tools in your research, please cite them as:

> Schweinberger, Martin. (2024). *LADAL Interactive Tools*. Brisbane: The University of Queensland. Language Technology and Data Analysis Laboratory (LADAL). Retrieved from https://ladal.edu.au/tools.html

---

## Licence

© 2024 Language Technology and Data Analysis Laboratory (LADAL) · University of Queensland  
[Citing & Licensing](https://ladal.edu.au/about.html#citing) · [ladal@uq.edu.au](mailto:ladal@uq.edu.au)

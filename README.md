# ✏️ FileRenamer

**Batch file renaming tool — LADAL**  
[https://ladal.edu.au](https://ladal.edu.au) · University of Queensland

---

## Overview

FileRenamer is a browser-based Shiny tool that lets you rename large batches of files quickly and safely. Upload any files, configure one or more renaming operations, preview the results before committing, and download all renamed files in a single ZIP archive.

**File contents are never read or modified.** Only file names change.

---

## Features

- Accepts **any file type** (`.txt`, `.csv`, `.pdf`, `.docx`, images, etc.)
- Six composable rename operations applied in a fixed, predictable order
- Live **preview table** with colour-coded diff before any download
- **Duplicate conflict detection** — the download button is locked until all conflicts are resolved
- Renamed files delivered as a single **ZIP archive**
- Automatic sanitisation of illegal file-system characters

---

## Rename operations

Operations are applied **top-to-bottom** in the order listed below. Multiple operations can be active at the same time.

### 1 · Find & replace 🔁

Replaces every occurrence of a search string with a replacement string.

| Option | Description |
|--------|-------------|
| **Find** | Text to search for |
| **Replace with** | Text to substitute in its place (leave empty to delete) |
| **Regex** | Treat the search string as a regular expression |
| **Case-sensitive** | When unchecked, matching ignores case |

**Examples**

| Original | Find | Replace | Result |
|----------|------|---------|--------|
| `Interview_Draft_2024.txt` | `Draft` | `Final` | `Interview_Final_2024.txt` |
| `data file v2.csv` | `\s+` *(regex)* | `_` | `data_file_v2.csv` |
| `Notes (copy).docx` | ` (copy)` | *(empty)* | `Notes.docx` |

---

### 2 · Remove substrings 🗑

Removes every occurrence of a pattern without a replacement. Functionally equivalent to Find & replace with an empty replacement, but kept separate for clarity.

| Option | Description |
|--------|-------------|
| **Pattern to remove** | Text or regex pattern to delete everywhere it appears |
| **Regex** | Treat the pattern as a regular expression |
| **Case-sensitive** | When unchecked, matching ignores case |

**Examples**

| Original | Pattern | Result |
|----------|---------|--------|
| `transcript_FINAL_v3.txt` | `_FINAL` | `transcript_v3.txt` |
| `2024-01-15_notes.txt` | `\d{4}-\d{2}-\d{2}_` *(regex)* | `notes.txt` |
| `Report (1).pdf` | ` (1)` | `Report.pdf` |

---

### 3 · Change case 🔤

Transforms the case of the entire file name stem (the extension is always preserved as-is).

| Option | Effect | Example |
|--------|--------|---------|
| **lowercase** | All characters lower | `My_File` → `my_file` |
| **UPPERCASE** | All characters upper | `my_file` → `MY_FILE` |
| **Title Case** | First letter of every word capitalised | `my interview notes` → `My Interview Notes` |
| **Sentence case** | First letter capitalised, rest lower | `MY NOTES` → `My notes` |

---

### 4 · Date patterns 📅

Detects and acts on date strings embedded in file names. Three date formats are recognised, with `-`, `_`, or `.` as separators:

| Format | Example match |
|--------|--------------|
| `YYYY-MM-DD` | `2024-03-15`, `2024_03_15`, `2024.03.15` |
| `DD-MM-YYYY` | `15-03-2024`, `15_03_2024` |
| `YYYYMMDD` | `20240315` |

| Action | Description |
|--------|-------------|
| **Remove date entirely** | Strips the date string and any leftover leading/trailing separators |
| **Replace date with…** | Substitutes the date with a custom string you provide |

**Examples**

| Original | Action | Result |
|----------|--------|--------|
| `interview_2024-03-15.txt` | Remove | `interview.txt` |
| `20240315_fieldnotes.txt` | Remove | `fieldnotes.txt` |
| `corpus_2023_06_01.csv` | Replace with `2024` | `corpus_2024.csv` |

---

### 5 · Add prefix / suffix ➕

Prepends and/or appends a fixed string to every file name stem. This operation runs **after** all transforms (Find & replace, Remove, Change case, Dates) so the affixes are never accidentally modified by those operations.

| Field | Description |
|-------|-------------|
| **Prefix** | Text added to the beginning of the stem |
| **Suffix** | Text added to the end of the stem (before the extension) |

**Examples**

| Original | Prefix | Suffix | Result |
|----------|--------|--------|--------|
| `interview01.txt` | `LADAL_` | *(empty)* | `LADAL_interview01.txt` |
| `notes.docx` | *(empty)* | `_reviewed` | `notes_reviewed.docx` |
| `data.csv` | `2024_` | `_v2` | `2024_data_v2.csv` |

---

### 6 · Sequential numbering 🔢

Appends or prepends a zero-padded counter to every file name. Applied **last**, after all other operations.

| Option | Description |
|--------|-------------|
| **Position** | `Suffix` (name then number) or `Prefix` (number then name) |
| **Separator** | Character(s) between the name and the number (default `_`) |
| **Start at** | The first number in the sequence (default `1`) |
| **Min digits** | Minimum width of the counter, zero-padded (default `3` → `001`) |

**Examples**

| Original | Position | Sep | Start | Digits | Result |
|----------|----------|-----|-------|--------|--------|
| `interview.txt` | Suffix | `_` | 1 | 3 | `interview_001.txt` |
| `notes.txt` | Prefix | `-` | 10 | 2 | `10-notes.txt` |
| `data.csv` | Suffix | `_` | 1 | 4 | `data_0001.csv` |

---

## Preview table

After clicking **Preview new names**, the main panel shows a side-by-side comparison of original and new file names:

| Colour | Meaning |
|--------|---------|
| 🟢 **Green** | Name has changed |
| ⚫ **Grey** | Name is unchanged |
| 🔴 **Red** | Duplicate conflict — two or more files would receive the same name |

Summary chips at the top of the panel show total files, number renamed, number unchanged, and number of conflicts.

The **Download renamed files (.zip)** button only appears when there are **no conflicts**. Resolve any red rows by adjusting your settings and clicking Preview again.

---

## Output

Clicking the download button produces a ZIP archive named:

```
renamed_files_YYYY-MM-DD.zip
```

The archive contains all uploaded files under their new names. File contents are bit-for-bit identical to the originals.

---

## Name sanitisation

After all operations are applied, the tool automatically:

- Replaces characters illegal on Windows, macOS, and Linux file systems (`\ / : * ? " < > |`) with `_`
- Collapses runs of consecutive separators (e.g. `___` → `_`) that may result from removals
- Strips leading and trailing separators and spaces
- Assigns a safe fallback name (`file_N`) to any stem that becomes empty after processing

---

## Deployment

### Repository placement

```
SLCLADAL/tools/
└── filerenamer/
    └── app.R
```

### Port

FileRenamer runs on port **3843**.

### Launcher notebook

Use `filerenamer_launcher.ipynb` in the `SLCLADAL/tools` root alongside the other tool launchers. The Binder launch URL follows the same pattern as the other LADAL tools:

```
https://binderhub.atap-binder.cloud.edu.au/v2/gh/SLCLADAL/tools-env/main
  ?urlpath=git-pull
  %3Frepo%3Dhttps%253A%252F%252Fgithub.com%252FSLCLADAL%252Ftools
  %26urlpath%3Dlab%252Ftree%252Ftools%252Ffilerenamer_launcher.ipynb
  %26branch%3Dmain
```

### R dependencies

All dependencies are already present in `tools-env/install.R`:

| Package | Role |
|---------|------|
| `shiny` | Web application framework |
| `stringi` | Fast, Unicode-safe string operations |
| `DT` | Interactive preview table |
| `zip` | ZIP archive creation |

No changes to `install.R` or `postBuild` are required.

---

## Technical notes

- **Operation order is fixed.** Find & replace → Remove → Change case → Date patterns → Prefix/suffix → Sequential numbering. This ensures predictable results when multiple operations are active simultaneously.
- **Extensions are preserved.** Only the file name stem (the part before the last `.`) is transformed. The original extension, including its original case, is always kept.
- **Sequential numbering respects batch size.** The zero-padding width is automatically widened if the batch is larger than the configured minimum (e.g. 1,000 files will always produce at least 4-digit counters regardless of the Min digits setting).
- **No server-side storage.** Uploaded files are held only in Shiny's temporary session directory and are discarded when the session ends.

---

## Citation

If you use FileRenamer in your research, please cite it as:

> Schweinberger, Martin. (2024). *FileRenamer: A browser-based batch file renaming tool*. Brisbane: The University of Queensland. Language Technology and Data Analysis Laboratory (LADAL). Retrieved from https://ladal.edu.au/tools.html

---

## Links

- [LADAL tools page](https://ladal.edu.au/tools.html)
- [LADAL website](https://ladal.edu.au)
- [Report an issue](https://github.com/SLCLADAL/tools/issues)

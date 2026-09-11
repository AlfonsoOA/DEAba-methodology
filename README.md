# DEAba: Analysis Scripts and Data 

This repository contains the R scripts, anonymized datasets, and supplementary
analysis materials used in:

> Olaya-Abril, A. *DEAba: A paper-based, data-driven methodology for deep
> learning of metabolism.* Journal of Chemical Education (submitted).

Two independent analyses are included, corresponding to the two main strands
of the manuscript: (1) the evaluation of the DEAba teaching methodology across
seven academic years, and (2) the 24-year longitudinal analysis of the
Andalusian university entrance examination (EBAU).

## Repository contents
├── DEAba_analysis.R # Main analysis: DEAba methodology evaluation
├── EBAU_analysis.R # Secondary analysis: entrance exam content analysis
├── 0_BQM_marks_ordered.xlsx # Final theoretical exam marks, by student/year
├── 1_BQM_PreviousKnowledgePlusOthers_ordered.xlsx # Diagnostic test + 18-item perception survey
├── 2_BQM_DEASurvey_ordered.xlsx # 5-item DEAba perception survey (2022/2023–2024/2025)
├── 3_BiologyIndices_ordered.xlsx # Institutional degree-level indices (annual aggregates)
├── Resumen_Selectividad.xlsx # EBAU exam content coding (2001–2024)
└── README.md

All datasets are fully anonymized. No student identifiers are included; the
`Student` column in the marks/survey files is an arbitrary numeric code with
no link to any external identifier.

## Requirements

Both scripts are written in R (tested on R ≥ 4.2) and require the following
packages, which will be installed automatically if missing:
readxl, dplyr, ggplot2, tidyr, rstatix, purrr, stringr, car, MASS, Hmisc, broom


## 1. `DEAba_analysis.R` — Academic Performance and Perception Analysis

### Purpose

Loads and cleans data from four sources (final marks, diagnostic/perception
surveys, DEAba perception survey, institutional degree indices) to:

- Compare academic performance (grades, exam completion, pass rates) between
  the pre-DEAba and DEAba periods.
- Compare diagnostic ("prior knowledge") scores between periods, separately
  for first- and second-semester content, to rule out differences in
  incoming student preparation as a confound.
- Analyze the DEAba perception survey (interest, usefulness, difficulty,
  satisfaction) and its internal consistency.
- Correlate perceptions, prior knowledge, and degree-level indices with
  final grades.
- Fit an exploratory regression model of final grade on period and
  perception scores.

### How to run

1. Edit the `base_path` variable (top of the script) to point to the folder
   containing the four input `.xlsx` files.
2. Run the script from top to bottom in R or RStudio. Output files (CSV,
   TXT, PNG) are written to a timestamped subfolder defined by
   `output_folder_name`.

### Key configurable parameters

| Parameter | Purpose |
|---|---|
| `before_new_methodology_years_marks` / `after_new_methodology_years_marks` | Academic years assigned to the pre-DEAba and DEAba periods for the marks-based analyses (Sections 1, 4, 8, 9, 10). **2019/2020 is deliberately excluded** (emergency remote assessment, documented irregularities in academic integrity; see manuscript Methods). 2020/2021 and 2021/2022 are included in the pre-DEAba period, as final examinations were conducted in person in both years. |
| `before_new_methodology_years_surveys` / `after_new_methodology_years_surveys` | Same logic, applied to the diagnostic/perception survey dataset (`df_surveys`), which uses a slightly different year set due to survey availability. |
| `covid_pre_pandemic_years` / `covid_pandemic_years` / `covid_post_pandemic_years` | A **separate, purely chronological** classification of academic years relative to the COVID-19 pandemic, independent of examination modality. Used only in the perception-survey COVID-period analysis (Section 2.5) and to exclude pandemic-year data from the prior-knowledge Before/After comparisons (Section 2.4). Note that 2020/2021 is classified as "Pandemic" here even though it is included in the Before period for the marks-based analyses — the two classifications answer different questions and are not meant to be interchangeable. |
| `consistent_perception_cols` | The 5 DEAba perception items (Q1–Q5), the only items present across all three years of DEAba implementation and therefore usable in the pooled regression model. |

### Output sections

The script prints and saves results under numbered sections that correspond
directly to the manuscript's Results and Supplementary Statistical Analysis:

1. Marks file analysis — descriptive statistics, performance groups, and the
   Before/After inferential comparison (grades, completion rate, pass rate).
2. Survey analysis — 1Q/2Q mark comparison, prior-knowledge Before/After
   comparison (with COVID-pandemic years excluded), perception-response
   distributions, and correlations with final grade.
3. DEAba methodology survey analysis — descriptive statistics, internal
   correlations (Q1–Q5), and year-over-year trend analysis.
4. Degree indices analysis — correlations between institutional degree-level
   indices and DEAba perceptions/grades.
5. Lagged analysis — entrance exam score vs. course marks.
6. Visualization of mark and entrance-mark trends.
8. Exploratory regression model (final grade ~ period + perceptions).

Note: regression models involving `Avg_Entrance_Mark` (Section 8, and parts
of Section 9) will report "not enough data" when institutional entrance-exam
data are missing for a given year — this reflects a genuine data limitation
(the entrance-exam index is only available as an annual institutional
aggregate, not at the student level) and is discussed as such in the
manuscript's Limitations.

## 2. `EBAU_analysis.R` — University Entrance Examination Content Analysis

### Purpose

Analyzes the presence, frequency, and cognitive level of metabolism-related
questions across 96 university entrance examinations (Andalusia, Spain)
spanning the 2001–2024 period (24 years × 2 examination calls × 2 question
options), to characterize the pre-university assessment context motivating
the development of DEAba.

### Input data

`Resumen_Selectividad.xlsx` contains one row per (curriculum item × year ×
call × option) combination, coding whether that item was asked (`x` in the
`N_questions` column) in that specific exam sitting. The curriculum was
divided into 28 discrete metabolism-related content items (`Item` column;
full list in the manuscript's Supplementary Information).

### How to run

1. Edit `file_path` and `results_path` (top of the script) to point to the
   input file and desired output folder.
2. Run the script from top to bottom. Each numbered analysis writes its own
   `.txt` report (via `sink()`) and, where applicable, an accompanying
   `.png` plot.

### Analyses performed (numbered to match the manuscript and reviewer
correspondence)

1. Overall frequency of the metabolism block (binomial test against a
   uniform, mandatory distribution).
2. Temporal trend in the number of exams containing at least one
   metabolism question (linear regression).
3. Preference for specific items (chi-squared goodness-of-fit test).
4. Differences between examination calls (Ordinary vs. Extraordinary).
5. Differences between question options (A vs. B).
6. Items never asked about in any of the 96 exams (n = 9; listed in the
   manuscript's Supplementary Information).
7. Most frequently and consistently asked items ("profitable" topics).
8. and 11. Exam sittings/options in which metabolism could be entirely
   avoided.
9. and 10. Evolution of the question profile over time (10 most frequent
   items), with a trend test for the most basic item (item-level
   classification given in the script).
12. Average percentage of the metabolism syllabus covered per year.
13. Temporal trend in the proportion of "basic" vs. "advanced" questions
    (linear regression; see manuscript Methods for the classification
    criterion distinguishing "basic" from "advanced" items).

## Reproducibility notes

- Both scripts are self-contained: given the accompanying data files, they
  reproduce every statistical result, table, and figure reported in the
  manuscript and in the response to reviewers.
- File paths (`base_path`, `file_path`, `results_path`) are local to the
  original analysis machine and **must be edited** before running.
- All p-values are reported unrounded in the script output; the manuscript
  follows the convention of reporting p < 0.001 only where the exact value
  is below that threshold, and the exact value otherwise (e.g., p = 0.001).
- Effect sizes are reported as Cohen's *d* for comparisons of continuous
  outcomes (grades, diagnostic scores) and as the phi coefficient (φ) for
  chi-squared-based comparisons of categorical outcomes (completion and pass
  rates).

## Contact

For questions about the data or analysis code, contact the corresponding
author (b22olaba@uco.es).

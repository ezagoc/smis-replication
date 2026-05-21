# FinalTests Replication Manifest

## Purpose

This folder contains a mix of:

- modern scripts that read upstream data and write all outputs into `FinalTests/results/`
- helper/wrapper scripts that call shared runners
- legacy scripts that still write outside this folder into `../../../../data/...` or `../../../../results/...`

For a slim git replication package, the safest approach is to keep the modern local-output pipeline and exclude the legacy scripts unless they are still needed for the paper.

## Core code in this folder

### Modern local-output analysis scripts

- `extensive_fes_batches_500.R`
- `extensive_fes_batches_500_strong.R`
- `extensive_linear_90p_p5.R`
- `extensive_linear_90p_p5_strong.R`
- `intensive_baseline_weighted_batches_500.R`
- `intensive_baseline_weighted_batches_500_strong.R`
- `intensive_linear_95p_weighted_batches_500.R`
- `intensive_linear_95p_weighted_batches_500_strong.R`
- `first_stage_all_transforms_runner.R`
- `first_stage_all_transforms_tables_runner.R`
- `plot_saved_results_only.R`

### Thin wrappers over the first-stage runners

- `first_stage_extensive_all_transforms.R`
- `first_stage_extensive_all_transforms_strong.R`
- `first_stage_intensive_all_transforms.R`
- `first_stage_intensive_all_transforms_strong.R`
- `first_stage_extensive_all_transforms_tables.R`
- `first_stage_extensive_all_transforms_strong_tables.R`
- `first_stage_intensive_all_transforms_tables.R`
- `first_stage_intensive_all_transforms_strong_tables.R`

### Legacy scripts with external writes

- `extensive_noblockfes_b1b2.R`
- `intensive_baseline.R`
- `intensive_linear_95p.R`

These are not ideal for a clean replication bundle because they write outside `FinalTests/results/`.

### Files that can likely be excluded

- `extensive_noblockfes_b1b2 (Copia en conflicto de Eduardo Zago 2026-05-04).R`
- `.Rhistory`

## Shared upstream code required

All modern scripts source utilities from:

- `../../../../src/utils/funcs_analysis.R`
- `../../../../src/utils/constants_final.R`
- `../../../../src/utils/import_data.R`

Important functions used by this folder:

- `ipak()`
- `poolTreatmentBalance2()`
- `get_analysis_ver_final_winsor()`
- `get_analysis_english_winsor()`

## Upstream data dependencies

### Common to most modern scripts

- `../../../../data/04-analysis/joint/below_p90_p95_divider.parquet`
- `../../../../data/04-analysis/joint/small_ties_b1b2p/small_tie1.parquet` ... `small_tie500.parquet`

### Built indirectly through `get_analysis_ver_final_winsor()`

For each stage used:

- `../../../../data/04-analysis/KE/stage1_2/verifiability_b1b2.parquet`
- `../../../../data/04-analysis/SA/stage1_2/verifiability_b1b2.parquet`
- `../../../../data/04-analysis/KE/stage3_4/verifiability_b1b2.parquet`
- `../../../../data/04-analysis/SA/stage3_4/verifiability_b1b2.parquet`
- `../../../../data/04-analysis/KE/stage5_6/verifiability_b1b2.parquet`
- `../../../../data/04-analysis/SA/stage5_6/verifiability_b1b2.parquet`

### Built indirectly through `get_analysis_english_winsor()`

Only needed by scripts that explicitly call the English loader:

- `../../../../data/04-analysis/KE/stage1_2/english_b1b2.parquet`
- `../../../../data/04-analysis/SA/stage1_2/english_b1b2.parquet`
- `../../../../data/04-analysis/KE/stage3_4/english_b1b2.parquet`
- `../../../../data/04-analysis/SA/stage3_4/english_b1b2.parquet`
- `../../../../data/04-analysis/KE/stage5_6/english_b1b2.parquet`
- `../../../../data/04-analysis/SA/stage5_6/english_b1b2.parquet`

### Extra inputs used by specific modern scripts

`extensive_fes_batches_500*.R`

- `../Attrition/attrition.parquet`
- `../../../../data/04-analysis/KE/baseline/baseline_divided_b1b2.parquet`
- `../../../../data/04-analysis/SA/baseline/baseline_divided_b1b2.parquet`

`first_stage_all_transforms_runner.R`

- `../../../../data/06-other/1-retweeters/aggregated/RTs_counts_smi.parquet`
- `../../../../data/03-experiment/ads/2-retweets/aggregated/RTs_counts_ads.parquet`

`first_stage_all_transforms_tables_runner.R`

- reads `results/<results_subdir>/estimates/*.xlsx`
- also re-reads:
  - `../../../../data/04-analysis/joint/below_p90_p95_divider.parquet`
  - `../../../../data/06-other/1-retweeters/aggregated/RTs_counts_smi.parquet`
  - `../../../../data/03-experiment/ads/2-retweets/aggregated/RTs_counts_ads.parquet`
  - plus staged `verifiability_b1b2.parquet` through `get_analysis_ver_final_winsor()`

`plot_saved_results_only.R`

- reads only files already produced in `FinalTests/results/`

### Extra inputs used only by legacy scripts

`extensive_noblockfes_b1b2.R`

- `../../../../data/04-analysis/KE/extensive_fixed_effects.parquet`
- `../../../../data/04-analysis/SA/extensive_fixed_effects.parquet`
- `../../../../data/04-analysis/KE/baseline/baseline_english_divided_b1b2.parquet`
- `../../../../data/04-analysis/SA/baseline/baseline_english_divided_b1b2.parquet`

`intensive_baseline.R` and `intensive_linear_95p.R`

- `../../../../data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet`

## Local outputs created inside this folder

All modern scripts write into `FinalTests/results/`.

Top-level output families currently present:

- `results/original`
- `results/permutations`
- `results/plots`
- `results/strong`
- `results/extensive_linear_90p_p5`
- `results/extensive_linear_90p_p5_strong`
- `results/intensive_baseline_weighted`
- `results/intensive_baseline_weighted_strong`
- `results/intensive_linear_95p_weighted`
- `results/intensive_linear_95p_weighted_strong`
- `results/first_stage_extensive_all_transforms`
- `results/first_stage_extensive_all_transforms_strong`
- `results/first_stage_intensive_all_transforms`
- `results/first_stage_intensive_all_transforms_strong`
- `results/replots`

Current output types in `results/`:

- `.xlsx`
- `.pdf`
- `.tex`

## Which scripts are fully local-output

These are the best candidates for a clean replication bundle:

- `extensive_fes_batches_500.R`
- `extensive_fes_batches_500_strong.R`
- `extensive_linear_90p_p5.R`
- `extensive_linear_90p_p5_strong.R`
- `intensive_baseline_weighted_batches_500.R`
- `intensive_baseline_weighted_batches_500_strong.R`
- `intensive_linear_95p_weighted_batches_500.R`
- `intensive_linear_95p_weighted_batches_500_strong.R`
- `first_stage_extensive_all_transforms.R`
- `first_stage_extensive_all_transforms_strong.R`
- `first_stage_intensive_all_transforms.R`
- `first_stage_intensive_all_transforms_strong.R`
- `first_stage_extensive_all_transforms_tables.R`
- `first_stage_extensive_all_transforms_strong_tables.R`
- `first_stage_intensive_all_transforms_tables.R`
- `first_stage_intensive_all_transforms_strong_tables.R`
- `plot_saved_results_only.R`

## Scripts that are not self-contained locally

These still write outside this folder and are poor fits for a slim replication repo:

- `extensive_noblockfes_b1b2.R`
- `intensive_baseline.R`
- `intensive_linear_95p.R`

## Recommended slim replication package

### Include code

- all modern local-output scripts listed above
- the four first-stage wrappers and four table wrappers
- `../../../../src/utils/funcs_analysis.R`
- `../../../../src/utils/constants_final.R`
- `../../../../src/utils/import_data.R`

### Include data

- `data/04-analysis/joint/below_p90_p95_divider.parquet`
- `data/04-analysis/joint/small_ties_b1b2p/`
- staged `verifiability_b1b2.parquet` files for KE and SA
- staged `english_b1b2.parquet` files only if you keep scripts that use English outcomes
- `Attrition/attrition.parquet` if you keep `extensive_fes_batches_500*.R`
- first-stage retweet aggregates if you keep the first-stage pipeline

### Include generated outputs if you want Overleaf-only reproduction

- `results/**.pdf`
- `results/**.tex`
- optionally `results/**/estimates/*.xlsx` if you want table rebuilding without rerunning regressions

### Exclude if aiming for a clean package

- legacy scripts with external writes
- conflict-copy script
- `.Rhistory`
- bulky intermediate `.xlsx` permutation outputs if Overleaf only needs final figures/tables

## Practical replication modes

### Full rerun mode

Include:

- modern scripts
- utility scripts
- all upstream parquet inputs
- `results/` can be empty

### Paper-build / Overleaf mode

Include:

- `plot_saved_results_only.R`
- first-stage table wrapper scripts if rebuilding tables from saved estimates
- `results/` PDFs, `.tex`, and any `.xlsx` estimate files needed by the table runner

In this mode you can often omit the 500 permutation parquet files and most raw upstream data, unless you want to recompute coefficients from scratch.

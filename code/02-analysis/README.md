# FinalTests Results Replication

This folder contains the copied `FinalTests` results scripts for the replication repo.

## Path convention

These scripts now assume the replication-repo layout:

- code: `code/02-analysis` with `analysis`, `graphs`, and `pre-process` subfolders
- shared R utilities: `src/utils`
- analysis data: `data/04-analysis`
- additional first-stage inputs: `data/others`
- outputs: `results`

All paths were updated by changing literals only. The code structure and run logic were left unchanged.

## Recommended run order

Main extensive and intensive results:

1. `analysis/extensive_fes_batches_500.R`
2. `analysis/extensive_fes_batches_500_strong.R`
3. `analysis/extensive_linear_90p_p5.R`
4. `analysis/extensive_linear_90p_p5_strong.R`
5. `analysis/intensive_baseline_weighted_batches_500.R`
6. `analysis/intensive_baseline_weighted_batches_500_strong.R`
7. `analysis/intensive_linear_95p_weighted_batches_500.R`
8. `analysis/intensive_linear_95p_weighted_batches_500_strong.R`

First-stage wrappers:

1. `analysis/first_stage_extensive_all_transforms.R`
2. `analysis/first_stage_extensive_all_transforms_strong.R`
3. `analysis/first_stage_intensive_all_transforms.R`
4. `analysis/first_stage_intensive_all_transforms_strong.R`
5. `analysis/first_stage_extensive_all_transforms_tables.R`
6. `analysis/first_stage_extensive_all_transforms_strong_tables.R`
7. `analysis/first_stage_intensive_all_transforms_tables.R`
8. `analysis/first_stage_intensive_all_transforms_strong_tables.R`

Saved-results rebuild:

1. `graphs/plot_saved_results_only.R`

Pre-processing helpers:

- `pre-process/aggregate_batches.R`
- `pre-process/aggregate_batches_intensive.R`
- `pre-process/inspect_stage1_2_varnames.R`

Legacy scripts retained in this folder:

- `extensive_noblockfes_b1b2.R`
- `extensive_noblockfes_b1b2 (Copia en conflicto de Eduardo Zago 2026-05-04).R`
- `intensive_baseline.R`
- `intensive_linear_95p.R`

## Current readiness check

The checked path dependencies for the main results families are now present in the replication repo, including:

- `data/04-analysis/attrition.parquet`
- `data/04-analysis/KE/baseline/baseline_divided_b1b2.parquet`
- `data/04-analysis/KE/baseline/baseline_english_divided_b1b2.parquet`
- `data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet`
- `data/others/RTs_counts_smi.parquet`
- `data/others/RTs_counts_ads.parquet`

So the folder now looks aligned from a path and file-presence perspective for both the modern script families and the retained legacy scripts.

## Notes

- The permutation scripts point to `data/04-analysis/joint/small_ties_b1b2/`.
- The first-stage scripts read `RTs_counts_smi.parquet` and `RTs_counts_ads.parquet` from `data/others/`.
- Existing outputs are expected under `results/` with the same subfolder structure already copied into the repo.
- The remaining step is execution verification in R.

# Agent Handoff: Results Section

## Scope

This handoff covers only the `FinalTests` results scripts copied into `code/02-analysis` of the replication repo.

## What was changed

- Repointed shared utility imports from `../../../../src/utils/` to `../../src/utils/`.
- Repointed analysis data reads into the repo-local `../../data/04-analysis/` tree.
- Repointed first-stage extra inputs from `../../../../data/06-other/` to `../../data/others/`.
- Repointed modern output roots from `results` to `../../results`.
- Repointed permutation draws from `small_ties_b1b2p` to `small_ties_b1b2`.
- Repointed the attrition lookup to `../../data/04-analysis/attrition.parquet`.
- Repointed first-stage retweet counts to `../../data/others/RTs_counts_smi.parquet` and `../../data/others/RTs_counts_ads.parquet`.
- Added folder documentation in `code/02-analysis/README.md`.
- Added repo-root `.gitignore` to ignore all data.

## Files updated

Main path updates were applied across the `.R` files in `code/02-analysis`, especially:

- `extensive_fes_batches_500.R`
- `extensive_fes_batches_500_strong.R`
- `extensive_linear_90p_p5.R`
- `extensive_linear_90p_p5_strong.R`
- `first_stage_all_transforms_runner.R`
- `first_stage_all_transforms_tables_runner.R`
- `intensive_baseline_weighted_batches_500.R`
- `intensive_baseline_weighted_batches_500_strong.R`
- `intensive_linear_95p_weighted_batches_500.R`
- `intensive_linear_95p_weighted_batches_500_strong.R`
- `plot_saved_results_only.R`

Legacy scripts were also repointed at the root level:

- `extensive_noblockfes_b1b2.R`
- `extensive_noblockfes_b1b2 (Copia en conflicto de Eduardo Zago 2026-05-04).R`
- `intensive_baseline.R`
- `intensive_linear_95p.R`

## Current status

The checked path dependencies that had been missing earlier are now present in the repo, including:

- `data/04-analysis/attrition.parquet`
- `data/04-analysis/KE/baseline/baseline_divided_b1b2.parquet`
- `data/04-analysis/KE/baseline/baseline_english_divided_b1b2.parquet`
- `data/04-analysis/joint/BlocksIntensive/original/intensive_fe.parquet`
- `data/others/RTs_counts_smi.parquet`
- `data/others/RTs_counts_ads.parquet`

So the results folder is now aligned from a path and file-presence perspective. The main remaining task is execution verification in an R session.

## Suggested next step

Run the modern script families first, then use the legacy scripts only if they are still needed for the replication package.

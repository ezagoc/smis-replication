# `00-randomization`

This folder contains the randomization workflow as copied into the replication repo.

The code structure has been kept intact. The only code edits made here were path fixes so the packaged randomization scripts point to the replication repo layout:

- randomization data root: `../../data/randomization/`
- local R helper: `../../code/00-randomization/funcs.R`
- Python helpers: `../../src/utils/funcs.py` and `../../src/utils/general.py`

## What is packaged and path-checked

These scripts now point at the current replication repo structure and the packaged files already present in `data/randomization/`:

- `01-creating_indexes.R`
- `02-overlap.R`
- `03-stratification_rand.R`
- `04-strat_rand_batch2.R`
- `05-join_strats_rand.ipynb`
- `06-balance_strat.R`
- `filtering_strong_wak.R`
- `randomization.R`
- `get_profiles.R`

These are the files I would treat as the working randomization pipeline in this repo.

## Files that are still upstream/raw notebooks

These notebooks still contain reads from raw pre-randomization locations such as `data/03-experiment/...` or `data/01-characterize/...`:

- `00-join_ties_randomization.ipynb`
- `summary_stats_followers.ipynb`
- `summary_stats_influencers.ipynb`

I updated their `data/randomization/...` read/write paths where that was clearly downstream-facing, but I did not rewrite their raw-data logic because the required upstream files are not currently packaged in this repo.

So:

- they are useful as archival/preparation notebooks
- they are not the cleanest entrypoint for the slim replication package

## Recommended run order

### A. Upstream / preparation

1. `get_profiles.R`
   Uses participant handles/IDs and writes profile data to `data/randomization/<country>/01-profiles/profiles.xlsx`.

2. `00-join_ties_randomization.ipynb`
   Upstream notebook that builds follower-level join-ties objects and writes variable files such as `variables_followers_batch2.parquet`.
   Note: this notebook still expects raw inputs outside the packaged randomization tree.

3. `01-creating_indexes.R`
   Builds `.Rda` assignment inputs from the packaged variable files in `02-variables/`.

### B. Followers randomization, batch 1

4. `03-stratification_rand.R`
   Runs first-batch follower stratified randomization using `twitter_followers_filtered.parquet`.

5. `filtering_strong_wak.R`
   Creates:
   - `followers_randomized_strong_weak.parquet`
   - `followers_randomized_strong_weak_abs.parquet`
   - `followers_randomized_abs.parquet`

### C. Followers randomization, batch 2

6. `02-overlap.R`
   Uses the first-batch outputs to remove overlapping already-treated followers and creates:
   - `twitter_followers_filtered.parquet`
   - `twitter_followers_filtered_batch2.parquet`

7. `04-strat_rand_batch2.R`
   Runs batch-2 stratified randomization over the filtered batch-2 follower inputs and writes shard files into:
   - `04-stratification/collect_batch2/`

8. `05-join_strats_rand.ipynb`
   Concatenates the shard files from `collect_batch2/` into:
   - `04-stratification/integrate/followers_randomized_batch2.parquet`

9. `06-balance_strat.R`
   Checks balance for the integrated batch-2 follower randomization.

### D. Influencer-level legacy randomization

10. `randomization.R`
   Influencer-level block randomization code.
   This is kept because it is part of the original workflow, but it is somewhat separate from the follower batch-2 path above.

## Main packaged data locations

The downstream scripts in this folder now use:

- `../../data/randomization/KE/...`
- `../../data/randomization/SA/...`

Important subfolders:

- `00-participants/`
- `01-profiles/`
- `02-variables/`
- `03-assignment/input/`
- `03-assignment/output/`
- `04-stratification/collect/`
- `04-stratification/collect_batch2/`
- `04-stratification/integrate/`

## Helper files

### R

- local helper used by the R scripts in this folder:
  - `funcs.R`

### Python

- notebook helpers in `../../src/utils/`:
  - `funcs.py`
  - `general.py`

## Notes on verification

I verified that the downstream R scripts no longer point to:

- `../../data/02-randomize/`
- `../../code/02-randomize/funcs.R`
- `../../../social-media-influencers-africa/...`

The only remaining old-path references are in the upstream/raw notebooks that still expect non-packaged source data under:

- `../../data/03-experiment/...`
- `../../data/01-characterize/...`

That is intentional for now so the historical notebook logic stays readable.

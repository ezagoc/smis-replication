# Instructions for Next Coding Pass: Outcome Coverage and Figure/Table Cleanup

## Purpose

The previous figure reorganization has already been completed. This next task should focus only on two objectives:

1. Fill outcome-level holes in the existing specifications.
2. Clean up the presentation of plots, graphs, and tables using John's low-risk recommendations.

This iteration should remain narrow. Do not change the empirical specifications, or make major decisions about the structure of the paper.

---

## Main task: fill outcome-level holes

The issue is not that the main specifications are missing. The issue is that some existing figures and tables appear to be generated using incomplete outcome lists.

For example, some stage-by-stage figures already exist, but they do not include all available outcomes in the relevant outcome family. Other figures, such as some ads outcome figures, appear to use a more complete outcome list. The task is therefore to audit and complete the outcome coverage within the existing figure- and table-generation routines.

The agent should:

1. Inspect the code that defines the outcome lists used by each figure and table.
2. Identify the full set of available outcomes for each outcome family.
3. Compare the full available outcome set to the outcomes currently included in each figure and table.
4. Modify the code so that each existing specification is run on all relevant available outcomes for its corresponding outcome family.
5. Regenerate the affected figures and tables.
6. Save the regenerated outputs using clear and systematic filenames.

The goal is to fill outcome-level holes within existing specifications, not to create new specifications.

---

## Specifications and outputs to check

At minimum, check the following groups.

### 1. Stage-by-stage effects

These figures already exist, but some appear to omit available outcomes. Update the code so that the existing stage-by-stage specification runs on the full outcome set for each relevant outcome family.

Pay special attention to cases like:

- `Extensive Outcomes: Stage-by-Stage Effects, Batch 1`

where some core outcomes may be missing.

Use more complete figures, such as some ads stage-by-stage figures, as references for what complete outcome coverage should look like.

### 2. Aggregated treatment-period effects

Check whether the aggregated treatment-period figures also omit available outcomes. If they do, update the outcome lists and regenerate the affected figures.

### 3. Make a code to run all of the missing specifications together

## Plot and graph cleanup using John's recommendations

After completing the outcome coverage, apply the following presentation changes to the affected plots and graphs. These are intended to improve clarity without changing the underlying empirical specifications.

### Control outcome mean, sd and outcome range (non - log)

Make a code that outputs the control outcome mean, control outcome standard deviation, and range for all the outcomes on the aggregated sample. These should be saved to later be used on the aggregated Figures. Take into account that this change depending on the sample, so there should be different ones for extensive, extensive strong, intensive, and the other variants.

### Aggregate plots using mock file

Using the information on coefficients, standard deviations, control outcome mean, sd and range, make a code that plots the aggregate results using as an example the file: mock_coefficient_plot_template.R

Then, from all the other graphs:

### Titles

Replace vague titles such as:

- `Extensive Outcomes`
- `Intensive Outcomes`
- `Baseline Effects`
- `Aggregated Treatment-Period Effects`

with more descriptive titles.

Suggested title logic:

- For initially-followed SMI treatment:
  - `Average effect of one initially-followed SMI being assigned to treatment on online follower behaviors`

- For strong-follower sample:
  - `Average effect of one strongly-followed SMI being assigned to treatment on online follower behaviors`

- For marginal effects / additional SMI:
  - `Sample-weighted average marginal effect of an additional initially-followed SMI being assigned to treatment on online follower behaviors`

- For ads:
  - `Average effect of assignment to receive treatment via paid-for ads on X`

Add batch information at the end of the title when relevant:

- `(Batch 1 only)`
- `(Batch 2 only)`
- `(Both batches)`

Move technical terms such as `aggregated treatment-period effects` or `stage-by-stage effects` into the figure note rather than relying on them as the main title.

### Axis labels

Use a clearer x-axis label:

```text
Average treatment effect, with 95% confidence interval
```

Avoid vague labels such as:

- `Total`
- `Effect`
- `Estimate`

unless the figure note clearly explains what is being estimated.

Remove the y-axis label:

```text
Variable
```

because the outcome labels already make this clear.

### Outcome labels

Make outcome labels easier to read and consistent across figures and tables.

In particular:

- Use `log(X)` instead of `log X`.
- Make labels with `+` signs clear.
- Use consistent names for the same outcome across all figures and tables.
- Reverse the outcome order so broad or general outcomes appear at the top and more specific outcomes appear below.

### Figure notes

Add or revise figure notes so they explain:

- whether the estimates are aggregated treatment-period effects or stage-by-stage effects;
- whether the sample is initially-followed or strongly-followed;
- whether the figure uses Batch 1, Batch 2, or Both Batches;
- what the confidence intervals represent;
- any basic estimation details already used in the current figures.

Do not add new methodological material beyond what is already part of the existing specification.

---

## Table cleanup using John's recommendations

Apply equivalent cleanup to tables.

The agent should:

1. Use clearer table titles.
2. Make batch and sample labels explicit.
3. Make outcome labels consistent with the figures.
4. Use `log(X)` notation for logged outcomes.
5. Make column names easier to interpret.
6. Avoid adding new substantive columns unless they are already part of the current table structure.

For Table 1 specifically, John noted that the title is hard to understand. Rename it using this logic:

```text
Average effect of one initially-followed SMI being assigned to treatment on number of [Y] (Batch 1 only)
```

Replace `[Y]` with the actual outcome represented in the table.

Do not redesign Table 1 as a figure in this iteration.

---

## What not to do in this iteration

Do not work on:

- attrition tests;
- balance tests;
- control-group mean, SD, or range add-ons;
- outcome standardization;
- PCA or factor-analysis indexes;
- deciding which outcomes move to the appendix;
- moving Figure 3 to the appendix;
- creating a figure version of Table 1;
- restructuring compliance figures;
- changing the empirical specifications.

Those can be considered later. The current goal is only to complete outcome coverage and clean the presentation.

---

## Deliverables

The agent should provide:

1. Updated code that completes outcome coverage within the existing specifications.
2. Regenerated figures and tables where outcomes were missing.
3. Cleaned versions of plots, graphs, and tables following John's presentation recommendations.
4. A short summary listing:
   - which outcome families had missing outcomes;
   - which scripts were modified;
   - which figures and tables were regenerated;
   - which presentation changes were applied;
   - any remaining ambiguous outputs, especially the follower aggregate effects figure.

---

## Bottom line

This is a focused coding and cleanup pass. The agent should complete missing outcome coverage inside the existing specifications and apply John's low-risk presentation improvements. The agent should not expand the scope into new diagnostics, new empirical specifications, or broader paper-structure decisions.

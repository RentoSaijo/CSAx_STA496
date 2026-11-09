# Physical Play Relative to Size

This STA 496 project studies calibrated size above expected (CSAx), a measure of how NHL players' physical play compares with what their listed size suggests. It brings together direct contact and position-specific signs of involvement in battles for the puck and space. The current analysis pools player-season profiles from 2021–22 through 2024–25 within each position, then compares the scores with earlier scouting descriptions, special-teams roles, postseason hitting, and next-season NHL participation.

## Reports

- [Research proposal](reports/proposal.pdf) and [Quarto source](reports/proposal.qmd)
- [Data sources](reports/dataset.pdf) and [Quarto source](reports/dataset.qmd)
- [Checkpoint 3](reports/progress1.pdf) and [Quarto source](reports/progress1.qmd)

The proposal, data report, and Checkpoint 3 document earlier stages of the study. The current pooled analysis is bundled in `data/analysis_data.rds`; the related [JHK research repository](https://github.com/RentoSaijo/CSAx_JHK) contains the journal manuscript.

## Reproduce

R package versions are pinned in `renv.lock`. With R, Quarto, and LaTeX available, render the reports from the project root:

```sh
Rscript -e "renv::restore(prompt = FALSE)"
quarto render reports/proposal.qmd --to pdf
quarto render reports/dataset.qmd --to pdf
quarto render reports/progress1.qmd --to pdf
```

The data report reads descriptive inputs from `data/analysis_data.rds`, which also contains the current pooled scores and application results. The numbered scripts prepare source records, fit the models, and analyze outcomes in order: `scripts/01_prepare_data.R`, `scripts/02_build_csax.R`, and `scripts/03_analyze.R`. A full refit requires the source cache in `data/cache/`, which is not tracked in Git.

## Sources and license

The NHL supplies game events, rosters, and shifts, retrieved through the pinned [`nhlscraper`](https://github.com/RentoSaijo/nhlscraper) revision. Corey Sznajder's [All Three Zones transition workbook](https://public.tableau.com/app/profile/corey.sznajder/viz/transitionstats/Sheet1) supplies tracked microstats; the [source catalog](validation/external_validation_source_catalog.csv) identifies official NHL draft-year scouting reports. Original project code uses the [MIT license](LICENSE). Third-party materials retain their own terms.

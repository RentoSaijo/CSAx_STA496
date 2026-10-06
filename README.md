# Physical Play Relative to Size

This STA 496 project studies calibrated size above expected (CSAx), a measure of how NHL players' physical play compares with what their listed size suggests. It brings together direct contact and position-specific signs of involvement in battles for the puck and space. We plan to examine whether the resulting forward and defenseman scores agree with earlier scouting descriptions and relate to special-teams roles, postseason hitting, and next-season NHL participation.

## Reports

- [Research proposal](reports/proposal_STA496.pdf) and [Quarto source](reports/proposal_STA496.qmd)
- [Dataset report](reports/dataset_STA496.pdf) and [Quarto source](reports/dataset_STA496.qmd)

The reports describe the research questions and data without using fitted CSAx or application findings. The repository also contains the broader [CSAx research workflow](https://github.com/RentoSaijo/CSAx_MITSSACRPC) and its bundled analysis object.

## Reproduce

R package versions are pinned in `renv.lock`. With R, Quarto, and LaTeX available, render the reports from the project root:

```sh
Rscript -e "renv::restore(prompt = FALSE)"
quarto render reports/proposal_STA496.qmd --to pdf
quarto render reports/dataset_STA496.qmd --to pdf
```

The reports read descriptive inputs from `data/analysis_data.rds`. To rebuild the research analysis from those bundled inputs, run the numbered scripts in order: `scripts/01_prepare_data.R`, `scripts/02_build_csax.R`, and `scripts/03_analyze.R`.

## Sources and license

The NHL supplies game events, rosters, and shifts, retrieved through the pinned [`nhlscraper`](https://github.com/RentoSaijo/nhlscraper) revision. Corey Sznajder's [All Three Zones transition workbook](https://public.tableau.com/app/profile/corey.sznajder/viz/transitionstats/Sheet1) supplies tracked microstats; the [source catalog](validation/external_validation_source_catalog.csv) identifies official NHL draft-year scouting reports. Original project code uses the [MIT license](LICENSE). Third-party materials retain their own terms.

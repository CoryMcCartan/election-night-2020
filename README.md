## [Run your own conditional forecasts on election night &raquo;](https://corymccartan.github.io/2020/11/election-night)

# Live Election-night Projections

This repository contains a series of tools and visualizations for election night.

## Files

* `R/election_night.R`: main election night script. Run to produce regular projections.
* `R/new_model.R`: fits a conjugate Bayesian linear model to predict the remaining vote
  in states with partial results, using informative priors, previous election results,
  and demographics. 
* `R/pull_returns.R`: scrapers to get the election returns from state websites &
  news outlets.
* `R/forecast_update.R`: loads and filters model posterior draws.
* `data/`: county presidential returns 2000-16, senate returns 2012-16, and
  2016 county demographics.  State EV data.
* `app.Rmd`: a Shiny app to do conditional forecasts, using posterior draws from
  [my](https://corymccartan.github.io/projects/president-20/) and the
  [Economist's](https://projects.economist.com/us-2020-forecast/president)
  election models.  Also uses `styles_app.css`, `map.js`, and `usa.js`.
* `viz/`: a dynamic R Markdown file to output the visualize from the scripts.
* `viz_old/`: the old version.
* `scripts/`: helper scripts; currently unused.
* `R/model1.R`: old live projection model.

## Getting Started

1. Run `Rscript R/pull_returns.R`, which will download returns for key states and
save them to `data/pulled/returns.rdata`
2. Run `R/election_night.R`, which will use the downloaded returns to make calls 
and create the visualization.

You can edit the last line of `R/election_night.R` to output visualizations using
either model (state calls are made using both).

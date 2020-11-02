#######################################
# ELECTION NIGHT FORECASTS
# November 3, 2020

# CORY McCARTAN
#######################################

# Initial called states
dem_states = "DC MA HI VT MD RI CA NY DE CT NJ WA OR IL CO"
gop_states = "WY WV ND ID OK AR UT AL TN KY SD MS"

#######################################
# SETUP
#######################################
library(cli)
source("R/pull_returns.R")
source("R/new_model.R")
source("R/forecast_update.R")

rule(center="ELECTION NIGHT FORECASTS", line=2, width=60)

cli_h1("Setting up")
counties = suppressMessages(get_county_data())
cli_alert_success("Loaded county data.")

draws = suppressMessages(get_draws())
draws_m = select(draws$mine, -draw, -ev, -natl) %>% as.matrix
cli_alert_success("Loaded pre-election forecast draws.")

prev_prior = calc_prev_prior(counties, 2012, infl=10)
cli_alert_success("Calculated prior from last election's results.")

states = suppressMessages(read_csv("data/state_ev.csv")) %>%
    select(state, ev=ev.2020) %>%
    left_join(suppressMessages(read_csv("data/state-data.csv")) %>%
                  select(abbr, state),
              by="state") %>%
    arrange(abbr)
state_ev = set_names(states$ev, states$abbr)
cli_alert_success("Loaded state EV data.")

calls = rep(NA, 51) %>% set_names(states$abbr)
calls[str_split(dem_states, " ")[[1]]] = 1
calls[str_split(gop_states, " ")[[1]]] = 0
cli_alert_info("Safe races called:")
cli_alert("DEM: {str_split(dem_states, ' ')[[1]]}")
cli_alert("GOP: {str_split(gop_states, ' ')[[1]]}")


#######################################
# FORECAST LOOP
#######################################
run_forecast = function(state_calls, fr=0.1, samples=1000) {
    cli_h1("Running forecast: {as.character(Sys.time(), format='%I:%M %p %Z')}")

    focus_states = names(state_calls)[is.na(state_calls)]
    cli_h2("States forecasting")
    cli_alert("{focus_states}")

    returns = pull_returns(fr)
    cli_alert_success("Pulled election returns.")
    cli_alert_info("{scales::percent(with(returns, sum(rep) / sum(precincts)))} of precincts reporting.")

    fit_d = get_fit_data(counties, returns, focus_states, 2016)
    pred_d = get_pred_data(counties, returns, focus_states, 2016)
    cli_alert_success("Merged returns with county data.")

    cli_alert_info("Fitting model to returns and county data.")
    model = fit_model(fit_d, prev_prior, samples=samples)
    cli_alert_success("Model fit successfully.")

    state_predict(model, draws_m, fit_d, pred_d)
}

call_states = function(state_calls, preds, quiet=F) {
    probs = apply(preds$states, 2, function(x) weighted.mean(x > 0.5, preds$w))
    dem_calls = names(probs)[probs > 0.9995]
    gop_calls = names(probs)[probs < 0.0005]

    if (length(dem_calls) + length(gop_calls) > 0) {
        cli_h1("NEW RACE CALLS")
        if (!quiet) beepr::beep(3)

        if (length(dem_calls) > 0)
            cli_alert_info("{str_to_upper(states$state[states$abbr %in% dem_calls])} for Biden")
        if (length(gop_calls) > 0)
            cli_alert_info("{str_to_upper(states$state[states$abbr %in% gop_calls])} for Trump")

        state_calls[dem_calls] = 1
        state_calls[gop_calls] = 0
    }

    state_calls
}

preds = run_forecast(calls, fr=0.25)
calls = call_states(calls, preds)
rmarkdown::render("summary.Rmd", envir=globalenv())

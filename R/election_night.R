#######################################
# ELECTION NIGHT FORECASTS
# November 3, 2020

# CORY McCARTAN
#######################################

calls = list(
    dem = c("DC", "MA", "HI", "VT", "MD", "RI", "CA", "NY", "DE", "CT", "NJ", "WA", "OR", "IL", "CO"),
    gop = c("WY", "WV", "ND", "ID", "OK", "AR", "UT", "AL", "TN", "KY", "SD", "MS")
)

#######################################
# SETUP
#######################################
library(cli)
source("R/pull_returns.R")
source("R/new_model.R")
source("R/forecast_update.R")

rule(center="ELECTION NIGHT FORECASTS", line=2, width=60)

cli_h1("Setting up")
county_d = suppressMessages(get_county_data())
cli_alert_success("Loaded county data.")

draws = suppressMessages(get_draws())
cli_alert_success("Loaded pre-election forecast draws.")

usa = suppressMessages(read_csv("data/state_ev.csv")) %>%
    select(state, ev=ev.2020) %>%
    left_join(suppressMessages(read_csv("data/state-data.csv")) %>%
                  select(abbr, state),
              by="state") %>%
    arrange(abbr)
usa$state[8] = "D.C."
state_ev = set_names(usa$ev, usa$abbr)
state_names = set_names(usa$state, usa$abbr)
cli_alert_success("Loaded state EV data.")

returns = tibble()


#######################################
# FORECAST LOOP
#######################################
run_forecast = function(pull=F, samples=2000) {
    cli_h1("Running forecast: {as.character(Sys.time(), format='%I:%M %p %Z')}")

    if (pull) {
        system2("Rscript R/pull_returns.R")
        cli_alert_success("Pulled election returns.")
    }

    returns = read_rds("data/pulled/returns.rdata")
    rpt = with(returns, sum(rep, na.rm=T) / sum(precincts, na.rm=T))
    cli_alert_info("{scales::percent(rpt)} of precincts reporting.")
    focus_states = sort(unique(returns$abbr))
    cli_h2("States forecasting")
    cli_alert("{focus_states}")

    proj_draws = map(focus_states, function(s) {
        cli_alert_info("Fitting model to {s} returns.")
        predict_state(returns, county_d, s, 2020, samples)
    }) %>%
        set_names(focus_states)
    cli_alert_success("Models fit successfully.")

    preds = list(
        draws = proj_draws,
        mine = filter_is(draws$mine, proj_draws),
        economist = filter_is(draws$economist, proj_draws)
    )
}

call_states = function(calls, preds, quiet=F) {
    prob_mine = colMeans(draws$mine[preds$mine, usa$abbr] > 0.5)
    prob_econ = colMeans(draws$economist[preds$economist, usa$abbr] > 0.5)


    cur_calls = sum(map_dbl(calls, length))
    dem_calls = union(usa$abbr[prob_mine > 0.995 & prob_econ > 0.995], calls$dem)
    gop_calls = union(usa$abbr[prob_mine < 0.005 & prob_econ < 0.005], calls$gop)

    if (length(dem_calls) + length(gop_calls) > cur_calls) {
        cli_h1("NEW RACE CALLS")
        if (!quiet) beepr::beep(3)

        new_dem = setdiff(dem_calls, calls$dem)
        new_gop = setdiff(gop_calls, calls$gop)
        if (length(new_dem) > 0)
            cli_alert_info("{str_to_upper(usa$state[usa$abbr %in% new-dem])} for Biden")
        if (length(new_gop) > 0)
            cli_alert_info("{str_to_upper(usa$state[usa$abbr %in% new_gop])} for Trump")
    }

    list(
        dem = dem_calls,
        gop = gop_calls
    )
}

preds = run_forecast()
calls = call_states(calls, preds)
rmarkdown::render("viz/viz.Rmd", envir=globalenv(), params=list(model="mine"))

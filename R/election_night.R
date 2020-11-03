#######################################
# ELECTION NIGHT FORECASTS
# November 3, 2020

# CORY McCARTAN
#######################################

calls = list(
    dem = c("DC", "MA", "HI", "VT", "MD", "RI", "CA", "NY", "DE", "CT", "NJ", "WA", "OR", "IL", "CO"),
    gop = c("WY", "WV", "ND", "ID", "OK", "AR", "UT", "AL", "TN", "KY", "SD", "MS")
)
calls_senate = list(dem=character(0), gop=character(0))

#######################################
# SETUP
#######################################
library(cli)
source("R/pull_returns.R")
source("R/new_model.R")
source("R/forecast_update.R")
source("R/forecast_update_senate.R")

rule(center="ELECTION NIGHT FORECASTS", line=2, width=60)

cli_h1("Setting up")
county_d = suppressMessages(get_county_data())
cli_alert_success("Loaded county data.")

draws = suppressMessages(get_draws())
draws_s = suppressMessages(get_draws.senate())
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
returns_sen = tibble()


#######################################
# FORECAST LOOP
#######################################
run_forecast = function(pull=F, samples=2000) {
    cli_h1("Running Presidential forecast: {as.character(Sys.time(), format='%I:%M %p %Z')}")

    if (pull) {
        system2("Rscript R/pull_returns.R")
        cli_alert_success("Pulled election returns.")
    }

    returns <<- read_rds("data/pulled/returns.rdata")
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

    list(
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
            cli_alert_info("{str_to_upper(usa$state[usa$abbr %in% new_dem])} for Biden")
        if (length(new_gop) > 0)
            cli_alert_info("{str_to_upper(usa$state[usa$abbr %in% new_gop])} for Trump")
    }

    list(
        dem = dem_calls,
        gop = gop_calls
    )
}

run_forecast_senate = function(proj_gen, pull=F, samples=2000) {
    cli_h1("Running Senate forecast: {as.character(Sys.time(), format='%I:%M %p %Z')}")

    if (pull) {
        system2("Rscript R/pull_returns.R")
        cli_alert_success("Pulled election returns.")
    }

    returns_sen <<- read_rds("data/pulled/returns_senate.rdata")
    rpt = with(returns_sen, sum(rep, na.rm=T) / sum(precincts, na.rm=T))
    cli_alert_info("{scales::percent(rpt)} of precincts reporting.")
    focus_states = sort(unique(returns_sen$abbr))
    cli_h2("States forecasting")
    cli_alert("{focus_states}")

    proj_draws = map(focus_states, possibly(function(s) {
        cli_alert_info("Fitting model to {s} returns.")
        predict_state(returns_sen, county_d, s, 2020, samples)
    }, NA)) %>%
        set_names(focus_states)
    cli_alert_success("Models fit successfully.")

    proj_gen_idx = filter_is_natl(draws_s, proj_gen)
    list(
        draws = proj_draws,
        idx = proj_gen_idx,
        mine = filter_is(draws_s[proj_gen_idx,], proj_draws)
    )
}

call_races = function(calls, preds, quiet=F) {
    race_abbr = colnames(draws_s)[-1:-3]
    prob = colMeans(draws_s[preds$idx,][preds$mine, race_abbr] > 0.5)

    cur_calls = sum(map_dbl(calls, length))
    dem_calls = union(race_abbr[prob > 0.995], calls$dem)
    gop_calls = union(race_abbr[prob < 0.005], calls$gop)

    if (length(dem_calls) + length(gop_calls) > cur_calls) {
        cli_h1("NEW RACE CALLS")
        if (!quiet) beepr::beep(3)

        new_dem = setdiff(dem_calls, calls$dem)
        new_gop = setdiff(gop_calls, calls$gop)
        if (length(new_dem) > 0)
            cli_alert_info("{race_abbr[race_abbr %in% new_dem]} Senate race for the Democrats")
        if (length(new_gop) > 0)
            cli_alert_info("{race_abbr[race_abbr %in% new_gop]} Senate race for the GOP")
    }

    list(
        dem = dem_calls,
        gop = gop_calls
    )
}


###########################################################
preds = run_forecast()
calls = call_states(calls, preds)

proj_gen = c(predict_sen(draws$mine$natl[preds$mine]),
             predict_sen(draws$economist$natl[preds$economist]))
preds_senate = run_forecast_senate(proj_gen)
calls_senate = call_races(calls_senate, preds_senate)

rmarkdown::render("viz/viz.Rmd", envir=globalenv(), params=list(model="mine"))

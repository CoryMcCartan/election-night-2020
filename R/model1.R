suppressMessages({
    library(dplyr)
    library(tidyr)
    library(readr)
    library(stringr)
})

#' Load the data
get_county_data = function() {
    # Remove DC. Patch 2016 Bedford cty, VA. Remove KC MO from 2000
    # as FIPS code doesn't match.
    votes = read_csv("data/countypres_2000-2016.csv") %>%
        select(year, state, abbr=state_po, county, fips=FIPS, party,
               votes=candidatevotes, total=totalvotes) %>%
        # remove DC
        filter(party %in% c("democrat", "republican"), abbr != "DC") %>%
        mutate(party = if_else(party=="republican", "gop", "dem")) %>%
        pivot_wider(names_from=party, values_from=votes) %>%
        # patch bedford VA
        rows_update(tibble(fips=51515, year=2016, dem=9768, gop=30659, total=42525),
                    by=c("year", "fips")) %>%
        # remove KC MO
        filter(fips != 36000)

    pop_chg = read_csv(str_c("http://www2.census.gov/programs-surveys/popest/datasets/",
                             "2010-2019/national/totals/nst-est2019-alldata.csv")) %>%
        transmute(state=NAME, chg = POPESTIMATE2019/POPESTIMATE2015)

    votes_2020 = filter(votes, year==2016) %>%
        left_join(pop_chg, by="state") %>%
        mutate(year=2020,
               dem=NA_real_, gop=NA_real_,
               total = total*chg) %>%
        select(-chg)
    votes = bind_rows(votes, votes_2020)

    demg = read_csv("data/counties_12_16_demg.csv") %>%
        select(fips, white_pct:rural_pct) %>%
        rename(young_pct=age29andunder_pct, old_pct=age65andolder_pct) %>%
        mutate(across(ends_with("_pct"), ~ ./100),
               lmed_inc = log(median_hh_inc))

    inner_join(votes, demg, by="fips") %>%
        mutate(twop = coalesce(dem + gop, total),
               dem_pct = dem/twop) %>%
        group_by(state, year) %>%
        mutate(share_state = twop / sum(twop, na.rm=T),
               state_dem = sum(dem, na.rm=T) / sum(twop, na.rm=T),
               lg_dem = qlogis(dem_pct),
               #lg_dem = dem_pct - state_dem,
               across(c(ends_with("_pct"), -starts_with("dem_")),
                      ~ . - weighted.mean(., twop, na.rm=T)),
               noncol_white_pct = lesscollege_pct * white_pct,
               lmed_inc = lmed_inc - weighted.mean(lmed_inc, twop, na.rm=T)) %>%
        group_by(fips) %>%
        mutate(last_dem_pct = lag(dem_pct, order_by=year),
               last_lgdem = lag(lg_dem, order_by=year),
               last_state_dem = lag(state_dem, order_by=year))
}

#' The baseline model formula
model_formula = lg_dem ~ 0 + white_pct + lesscollege_pct + old_pct +
    lmed_inc + rural_pct*last_lgdem + abbr + (0+last_lgdem|abbr)

#' The variable priors from the previous election. Inflate the variance of the fit from the previous year
calc_prev_prior = function(county_d, yr, form=model_formula, infl=100) {
    library(lme4)

    d = filter(county_d, year==yr) %>%
        drop_na

    m = lmer(form, data=d)

    prior = tibble(
        coef = names(fixef(m)),
        loc = fixef(m),
        scale = sqrt(diag(vcov(m))) * infl
    )

    prior
}

#' Get the state priors from the website
get_state_prior = function(path="http://corymccartan.github.io/president/state_history.csv") {
    suppressWarnings(read_csv(path)) %>%
        filter(date==max(date)) %>%
        transmute(abbr=state,
                  loc=qlogis(dem_exp),
                  scale=4*(dem_q75 - dem_q25)/1.349)
}

#' Fit the model with priors
fit_model = function(d, prev_prior, samples=1000) {
    library(rstanarm)

    d = drop_na(d)
    # set up prior w/ correct ordering
    states = sort(unique(d$abbr))
    coef_names = update(model_formula, ~ . - (0+last_lgdem|abbr)) %>%
        model.matrix(data=d) %>%
        colnames()
    coef_prior = prev_prior[match(coef_names, prev_prior$coef),]

    stan_lmer(model_formula, data=d, prior=normal(coef_prior$loc, coef_prior$scale, F),
             QR=T, chains=1, iter=samples+400, warmup=400, control=list(stepsize=0.3))
}

get_fit_data = function(county_d, returns, states, yr=2020) {
    full_d = filter(returns, rep/precincts >= 0.99)
    filter(county_d, year==yr, abbr %in% states) %>%
        select(-total, -dem, -gop, -twop) %>%
        inner_join(full_d, by="fips") %>%
        drop_na(any_of(as.character(attr(terms(model_formula), "variables"))))
}

get_pred_data = function(county_d, returns, states, yr=2020) {
    full_d = filter(returns, rep/precincts >= 0.99)
    filter(county_d, year==yr, abbr %in% states) %>%
        anti_join(full_d, by="fips") %>%
        drop_na(any_of(as.character(attr(terms(model_formula), "variables"))))
}

state_predict = function(model, draws_m, fit_d, pred_d) {
    library(mvtnorm)

    fit_states = sort(unique(fit_d$abbr))
    pred_d = filter(pred_d, abbr %in% fit_states)
    pred_states =sort(unique(pred_d$abbr))

    state_twop = bind_rows(fit_d, pred_d) %>%
        filter(abbr %in% pred_states) %>%
        group_by(abbr) %>%
        summarize(sum(twop)) %>%
        pull
    tallied = fit_d %>%
        filter(abbr %in% pred_states) %>%
        group_by(abbr) %>%
        summarize(sum(dem)) %>% pull

    preds = posterior_predict(model, newdata=pred_d, re.form=~0)
    state_pred = t(apply(preds, 1, function(x) {
        # add some more noise for conservativeness
        twop = pred_d$twop * rnorm(length(x), 1, 0.05)
        grouped = tapply(plogis(x)*twop, pred_d$abbr, sum)
        (grouped + tallied) / state_twop
    }))

    prior = draws_m[,pred_states]
    wgts = apply(state_pred, 1, function(x) {
        dmvnorm(x, colMeans(prior), cov(prior), log=T)
    })
    wgts = exp(wgts - max(wgts))
    # cap at 4 per
    N = nrow(state_pred)
    idx = c(
        sample.int(N, round(N/4), replace=F, prob=wgts),
        sample.int(N, round(N/4), replace=F, prob=wgts),
        sample.int(N, round(N/4), replace=F, prob=wgts),
        sample.int(N, round(N/4), replace=F, prob=wgts)
    )

    list(est_dem = colMeans(preds),
         states = state_pred,
         w = wgts,
         rs = idx,
         coefs = tibble(
             coef = names(fixef(model)),
             loc = fixef(model),
         ))
}

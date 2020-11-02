suppressMessages({
    library(dplyr)
    library(tidyr)
    library(readr)
    library(stringr)
    library(rstanarm)
    library(mvtnorm)
})

full_formula = lg_dem ~ white_pct + lesscollege_pct +
    old_pct + lmed_inc + last_lgdem*last_ltwop
basic_formula = lg_dem ~ rural_pct + last_lgdem*last_ltwop
turnout_formula = ltwop ~ old_pct + lmed_inc + last_lgdem*last_ltwop


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

    votes_2020 = filter(votes, year==2016) %>%
        mutate(year=2020, dem=NA_real_, gop=NA_real_, total = NA_real_)
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
               ltwop = log(twop)) %>%
        group_by(fips) %>%
        mutate(last_ltwop = lag(ltwop, order_by=year),
               last_lgdem = lag(lg_dem, order_by=year))
}

# Get data to fit model to
get_fit_data = function(county_d, returns, st, yr=2020) {
    full_d = filter(returns, rep/precincts >= 0.99)
    filter(county_d, year==yr, abbr==st) %>%
        select(-total, -dem, -gop, -twop) %>%
        inner_join(full_d, by="fips") %>%
        mutate(lg_dem = qlogis(dem / twop)) %>%
        drop_na(any_of(as.character(attr(terms(model_formula), "variables"))))
}

# Get data to predict using model
get_pred_data = function(county_d, returns, st, yr=2020) {
    full_d = filter(returns, rep/precincts >= 0.99)
    filter(county_d, year==yr, abbr==st) %>%
        anti_join(full_d, by="fips") %>%
        mutate(lg_dem = qlogis(dem / twop)) %>%
        drop_na(any_of(as.character(attr(terms(model_formula), "variables"))))
}

# get posterior draws from conjugate linear model, using importance sampling to
# apply priors
bayes_draw = function(m, newdata=m$model, draws=5e3, priors=NULL, infl=1.5) {
    idx = !is.na(coef(m))
    sigmas = sigma(m) / sqrt(rchisq(draws, df.residual(m)) / df.residual(m))
    beta_zs = sigmas * rmvnorm(draws, sigma=vcov(m)[idx, idx]/sigma(m)^2)
    betas = t(apply(beta_zs, 1, function(x) x + coef(m)[idx]))
    X = model.matrix(formula(m), newdata)
    error = infl * matrix(rnorm(draws*nrow(X), 0, sigmas), nrow=draws)
    ypred = betas %*% t(X[,idx,drop=F]) + error

    if (!is.null(priors)) {
        wgts = dmvnorm(betas, priors$mean[idx], priors$cov[idx,idx], log=T)
        wgts = wgts - max(wgts)
    } else {
        wgts = rep(0, draws)
    }

    ypred[sample(draws, replace=T, prob=exp(wgts)),,drop=F]
}

# calculate linear model priors from previous year, with cov. inflated by `infl`
calc_priors = function(county_d, st, yr=2020, infl=5) {
    d = filter(county_d, year<yr, abbr==st)

    list(
        basic = lm(basic_formula, data=d),
        full = lm(full_formula, data=d),
        turnout = lm(turnout_formula, data=d)
    ) %>%
        map(function(m) {
            loc = coef(m)
            loc[1] = 0
            scale = infl^2 * vcov(m)
            scale[1,] = scale[1,] * 3
            scale[,1] = scale[,1] * 3

            list(mean=loc, cov=scale)
        })
}




returns = pull_returns.wapo()
county_d = get_county_data()

predict_state = function(returns, county_d, st, yr=2020) {
    priors = calc_priors(county_d, st, yr, infl=10)
    d_fit = get_fit_data(county_d, returns, st, yr)
    d_pred = get_pred_data(county_d, returns, st, yr)
    if (nrow(d_fit) == 0)
        return(NA)
    if (nrow(d_pred) == 0)
        return(sum(d_fit$dem) / sum(d_fit$twop))

    m_full = lm(full_formula, data=d_fit)
    m_basic = lm(basic_formula, data=d_fit)
    m_turn = lm(turnout_formula, data=d_fit)

    # AIC weights for full vs basic model
    model_wgt = -0.5*c(AIC(m_basic), AIC(m_full))
    model_wgt = model_wgt - max(model_wgt)
    model_wgt = exp(model_wgt) / sum(exp(model_wgt))
    dr = 1 + round((draws-2)*model_wgt[1])
    dr = c(dr, draws - dr)

    pred_vote = plogis(rbind(
        bayes_draw(m_basic, d_pred, priors=priors$basic, draws=dr[1]),
        bayes_draw(m_full, d_pred, priors=priors$full, draws=dr[2])
    ))
    pred_turn = exp(bayes_draw(m_turn, d_pred, priors=priors$turnout, draws=draws))
    dem_pct = (rowSums(pred_vote * pred_turn) + sum(d_fit$dem)) /
        (rowSums(pred_turn) + sum(d_fit$twop))

    dem_pct
}

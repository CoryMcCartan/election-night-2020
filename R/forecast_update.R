suppressMessages({
    library(dplyr)
    library(purrr)
    library(readr)
    library(mvtnorm)
})

get_draws = function() {
    tmp = tempfile()
    download.file("https://cdn.economistdatateam.com/us-2020-forecast/data/president/economist_model_output.zip", tmp)
    econ_d = read_csv(unz(tmp, "output/site_data//electoral_college_simulations.csv"))
    unlink(tmp)
    list(
        economist = rename(econ_d, ev=dem_ev, natl=natl_pop_vote),
        mine = read_csv("https://corymccartan.github.io/president/draws_mat.csv")
    )
}

# dem_win and gop_win each a list of state abbreviations
filter_wins = function(draws, dem_win, gop_win) {
    map(draws, function(dr) {
        filter(dr, across(all_of(dem_win), ~ . > 0.5),
               across(all_of(gop_win), ~ . < 0.5))
    })
}

# input a list of 2-element vectors containing the ranges
filter_ranges = function(draws, close_states, input) {
    if (is.null(draws) || !all(close_states %in% names(input)))
        return(draws)
    keep = rep(T, nrow(draws))

    for (s in close_states) {
        keep = keep & draws[,s] >= input[[s]][1]/100 &
            draws[,s] <= input[[s]][2]/100
    }
    draws[keep,]
}

# model_draws a matrix from get_draws
# proj_draws a list with an entry for each state of a vector of two-way dem_pct predictions
filter_is = function(model_draws, proj_draws) {
    model_m = as.matrix(select(model_draws, -draw:-natl))
    model_loc = colMeans(model_m)
    model_cov = cov(model_m)
    N = nrow(model_m)

    st = names(proj_draws)
    log_source = dmvnorm(model_m[,st], model_loc[st], model_cov[st, st], log=T)
    log_tgt = imap(proj_draws, function(x, s) {
        if (is.na(x)) return(rep(0, N))
        dx = density(x, adjust=1.5)
        coalesce(approxfun(dx$x, log(dx$y))(model_m[,s]), 0)
    }) %>%
        do.call(rbind, .) %>%
        colSums
    if (all(log_tgt == 0)) return(1:N)

    lr = log_tgt - log_source
    lr = pmin(lr, quantile(lr, 0.95))
    wgt = exp(lr - max(lr))
    rs = sample(N, replace=T, prob=wgt)
    rs
}

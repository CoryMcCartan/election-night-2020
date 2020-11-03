suppressMessages({
    library(dplyr)
    library(purrr)
    library(readr)
    library(mvtnorm)
})

get_draws.senate = function() {
    read_csv("https://corymccartan.github.io/senate/draws_mat.csv")
}

# model_draws is get_draws.senate
# proj_natl a vector of natl generic ballot projections
filter_is_natl = function(model_draws, proj_natl) {
    natl = model_draws$natl
    N = nrow(model_m)

    log_source = dnorm(natl, mean(natl), sd(natl), log=T)

    dx = density(proj_natl, adjust=1.5)
    log_tgt = coalesce(approxfun(dx$x, log(dx$y))(natl), 0)

    lr = log_tgt - log_source
    lr = pmin(lr, quantile(lr, 0.95))
    wgt = exp(lr - max(lr))
    rs = sample(N, replace=T, prob=wgt)
    rs
}

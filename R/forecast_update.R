suppressMessages({
    library(dplyr)
    library(purrr)
    library(readr)
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

filter_wins = function(draws, dem_win, gop_win) {
    map(draws, function(dr) {
        filter(dr, across(all_of(dem_win), ~ . > 0.5),
               across(all_of(gop_win), ~ . < 0.5))
    })
}

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

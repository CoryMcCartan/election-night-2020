#############################
# ELECTION RESULTS SCRAPER
#
# E-DAY TODO
# - find natl site with returns for other states
# - check NC colums: 'votes' or 'total'
# - check IN precincts reporting
# - build scraper for other key states: try MN, MT, MO, NH
#############################

suppressMessages({
    library(dplyr)
    library(purrr)
    library(tidyr)
    library(readr)
    library(stringr)
    library(jsonlite)
    library(furrr)
    library(cli)
})

plan(multicore, workers=3)

usa = tibble(abbr = c("AL","AK","AZ","AR","CA", "CO","CT","DE","DC","FL","GA","HI","ID","IL","IN", "IA","KS","KY","LA","ME","MD","MA","MI","MN",
                      "MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC", "ND","OH","OK","OR","PA","RI","SC","SD","TN", "TX","UT","VT","VA","WA","WV","WI","WY"),
             fips = c("01","02","04","05","06", "08","09","10","11","12","13","15","16","17","18", "19","20","21","22","23","24","25","26","27",
                      "28","29","30","31","32","33","34","35","36","37", "38","39","40","41","42","44","45","46","47", "48","49","50","51","53","54","55","56"))

pull_returns = function(states=c("VA", "FL", "IN", "NC", "GA", "ME", "IA", "MT",
                                 "NH", "MI", "WI", "MN", "SC", "AZ", "TX")) {
    fns = list(
        VA = pull_returns.VA,
        FL = pull_returns.FL,
        IN = pull_returns.IN,
        NC = pull_returns.NC
    )
    custom_scrapers = names(fns)
    fns = append(fns, map(setdiff(states, custom_scrapers),
                          ~ function() { pull_returns.politico(.) }))

    future_map_dfr(fns, ~ .(), .options=furrr_options(seed=T))
}

pull_returns.wapo = function() {
    url = "https://dohdeick6sqa6.cloudfront.net/api/v2/metadata/aggregates/president/all-reporting-units.json"
    raw = read_json(url, simplifyVector=F)

    get_counties = function(st) keep(st$reportingUnits, ~ .$level == "FIPSCode")

    to_tbl = function(cty) {
        tibble(fips = as.integer(cty$fipsCode),
               rep = cty$precinctsReporting,
               precincts = cty$precinctsTotal,
               dem = keep(cty$candidates, ~ .$last == "Clinton")[[1]]$voteCount,
               gop = keep(cty$candidates, ~ .$last == "Trump")[[1]]$voteCount,
               twop = dem+gop,
               total = sum(map_dbl(cty$candidates, ~ .$voteCount))
        )
    }

    raw %>%
        map(get_counties) %>%
        purrr::flatten() %>%
        map_dfr(to_tbl)
}

pull_returns.politico = function(abbr) {
    cli_alert_info("Getting Politico results for {abbr}")
    fips = usa$fips[usa$abbr == abbr]
    url_meta = str_glue("https://www.politico.com/2020-election/data/general-election-results/metadata/{fips}/potus.meta.json")
    url_votes = str_glue("https://www.politico.com/2020-election/data/general-election-results/live-results/{fips}/potus-counties.json")

    raw_meta = read_json(url_meta, simplifyVector=T)
    raw_votes = read_json(url_votes, simplifyVector=F)
    cands = raw_meta$candidates
    if (is.list(cands) && !is.data.frame(cands)) cands = cands[[1]]
    dem_id = cands$candidateID[cands$party == "dem"]
    gop_id = cands$candidateID[cands$party == "gop"]

    to_tbl = function(cty) {
        tibble(abbr = abbr,
               fips = as.integer(cty$countyFips),
               rep = cty$progressReporting,
               precincts = cty$progressTotal,
               dem = keep(cty$candidates, ~ .$candidateID == dem_id)[[1]]$vote,
               gop = keep(cty$candidates, ~ .$candidateID == gop_id)[[1]]$vote,
               twop = dem+gop,
               total = sum(map_dbl(cty$candidates, ~ .$vote))
        )
    }

    map_dfr(raw_votes$races, to_tbl)
}

pull_returns.politico_sen = function(abbr) {
    cli_alert_info("Getting Politico results for the {abbr} Senate race")
    fips = usa$fips[usa$abbr == str_sub(abbr, 1, 2)]
    url_meta = str_glue("https://www.politico.com/2020-election/data/general-election-results/metadata/{fips}/sen.meta.json")
    url_votes = str_glue("https://www.politico.com/2020-election/data/general-election-results/live-results/{fips}/sen-counties.json")
    if (abbr == "AZ" || abbr == "GA-S") {
        url_meta = str_glue("https://www.politico.com/2020-election/data/general-election-results/metadata/{fips}/senSpecial.meta.json")
        url_votes = str_glue("https://www.politico.com/2020-election/data/general-election-results/live-results/{fips}/senSpecial-counties.json")
    }

    raw_meta = read_json(url_meta, simplifyVector=T)
    raw_votes = read_json(url_votes, simplifyVector=F)
    cands = raw_meta$candidates
    if (is.list(cands)) cands = cands[[1]]
    dem_id = cands$candidateID[cands$party == "dem"]
    gop_id = cands$candidateID[cands$party == "gop"]

    to_tbl = function(cty) {
        tibble(abbr = abbr,
               fips = as.integer(cty$countyFips),
               rep = cty$progressReporting,
               precincts = cty$progressTotal,
               dem = keep(cty$candidates, ~ .$candidateID == dem_id)[[1]]$vote,
               gop = keep(cty$candidates, ~ .$candidateID == gop_id)[[1]]$vote,
               twop = dem+gop,
               total = sum(map_dbl(cty$candidates, ~ .$vote))
        )
    }

    map_dfr(raw_votes$races, to_tbl)
}

pull_returns.VA = function() {
    cli_alert_info("Getting Virginia results.")
    url = "https://results.elections.virginia.gov/vaelections/2020%20November%20General/Json/President_and_Vice_President.json"
    raw = read_json(url, simplifyVector=F)

    to_tbl = function(cty) {
        tibble(abbr = "VA",
               fips = 51e3 + as.integer(cty$Locality$LocalityCode),
               rep = cty$PrecinctsReporting,
               precincts = cty$PrecinctsParticipating,
               dem = keep(cty$Candidates, ~ .$PoliticalParty == "Democratic")[[1]]$Votes,
               gop = keep(cty$Candidates, ~ .$PoliticalParty == "Republican")[[1]]$Votes,
               twop = dem+gop,
               total = sum(map_dbl(cty$Candidates, ~ .$Votes))
        )
    }

    map_dfr(raw$Localities, to_tbl)
}

pull_returns.FL = function() {
    cli_alert_info("Getting Florida results.")
    url = "https://flelectionfiles.floridados.gov/enightfilespublic/20201103_ElecResultsFL.txt"
    raw = read_tsv(url, col_types="cccccccddddcccd")

    counties = tibble(
        fips = c(12001L,12003L,12005L,12007L,12009L,12011L,12013L,12015L,12017L,
                 12019L,12021L,12023L,12027L,12029L,12031L,12033L,12035L,12037L,
                 12039L,12041L,12043L,12045L,12047L,12049L,12051L,12053L,12055L,
                 12057L,12059L,12061L,12063L,12065L,12067L,12069L,12071L,12073L,
                 12075L,12077L,12079L,12081L,12083L,12085L,12086L,12087L,12089L,
                 12091L,12093L,12095L,12097L,12099L,12101L,12103L,12105L,12107L,
                 12109L,12111L,12113L,12115L,12117L,12119L,12121L,12123L,12125L,
                 12127L,12129L,12131L,12133L),
        county = c("Alachua","Baker","Bay","Bradford","Brevard","Broward","Calhoun",
                   "Charlotte", "Citrus","Clay","Collier","Columbia", "De Soto",
                   "Dixie","Duval","Escambia", "Flagler","Franklin","Gadsden",
                   "Gilchrist","Glades","Gulf","Hamilton","Hardee", "Hendry",
                   "Hernando","Highlands", "Hillsborough","Holmes","Indian River",
                   "Jackson","Jefferson","Lafayette","Lake", "Lee","Leon","Levy",
                   "Liberty","Madison", "Manatee","Marion","Martin", "Miami-Dade",
                   "Monroe","Nassau","Okaloosa", "Okeechobee","Orange","Osceola",
                   "Palm Beach","Pasco","Pinellas","Polk","Putnam","St Johns",
                   "St Lucie","Santa Rosa","Sarasota","Seminole","Sumter",
                   "Suwannee","Taylor","Union","Volusia","Wakulla","Walton","Washington")
        )

    raw %>%
        filter(RaceCode == "PRE") %>%
        group_by(CountyName) %>%
        mutate(total = sum(CanVotes)) %>%
        ungroup %>%
        select(party=PartyCode, county=CountyName, rep=PrecinctsReporting,
               precincts=Precincts, votes=CanVotes, total) %>%
        filter(party %in% c("DEM", "REP")) %>%
        mutate(party = if_else(party=="DEM", "dem", "gop")) %>%
        pivot_wider(names_from=party, values_from=votes) %>%
        mutate(twop = dem+gop, abbr="FL") %>%
        left_join(counties, by="county") %>%
        select(abbr, fips, rep, precincts, dem, gop, twop, total)
}

pull_returns.IN = function() {
    cli_alert_info("Getting Indiana results.")
    url = "https://enr.indianavoters.in.gov/site/data/OffCatC_1019_A.json"
    raw = read_json(url)

    precincts = c(25L,338L,68L,15L,12L,53L,12L,19L,
                  40L,72L,24L,39L,18L,28L,45L,22L,39L,78L,40L,118L,28L,
                  60L,18L,23L,17L,34L,63L,32L,222L,50L,39L,104L,41L,
                  73L,37L,30L,29L,18L,26L,25L,135L,33L,69L,16L,525L,
                  91L,40L,112L,600L,30L,18L,31L,82L,27L,54L,18L,29L,
                  11L,22L,18L,17L,21L,18L,123L,34L,15L,31L,18L,25L,17L,
                  223L,16L,40L,24L,21L,23L,21L,12L,119L,15L,10L,136L,
                  17L,89L,26L,13L,59L,21L,60L,22L,20L,34L)
    names(precincts) = c(18001L,18003L,18005L,18007L,18009L,
             18011L,18013L,18015L,18017L,18019L,18021L,18023L,18025L,
             18027L,18029L,18031L,18033L,18035L,18037L,18039L,18041L,
             18043L,18045L,18047L,18049L,18051L,18053L,18055L,18057L,
             18059L,18061L,18063L,18065L,18067L,18069L,18071L,18073L,
             18075L,18077L,18079L,18081L,18083L,18085L,18087L,18089L,
             18091L,18093L,18095L,18097L,18099L,18101L,18103L,
             18105L,18107L,18109L,18111L,18113L,18115L,18117L,18119L,
             18121L,18123L,18125L,18127L,18129L,18131L,18133L,18135L,
             18137L,18139L,18141L,18143L,18145L,18147L,18149L,18151L,
             18153L,18155L,18157L,18159L,18161L,18163L,18165L,18167L,
             18169L,18171L,18173L,18175L,18177L,18179L,18181L,18183L)

    to_tbl = function(cty) {
        cands = cty$Races$Race$Candidates$Candidate
        tibble(abbr = "IN",
               fips = as.integer(cty$MAP_FIPS),
               rep = NA,
               precincts = precincts[as.character(fips)],
               dem = keep(cands, ~ .$POLITICALPARTYID == "1010")[[1]]$TOTAL,
               gop = keep(cands, ~ .$POLITICALPARTYID == "1011")[[1]]$TOTAL,
               twop = dem+gop,
               total = sum(map_dbl(cands, ~ .$TOTAL))
        )
    }

    map_dfr(raw$Root$OfficeCategory$Regions$Region, to_tbl)
}

# CHECK ON E-DAY
pull_returns.NC = function() {
    cli_alert_info("Getting North Carolina results.")
    url = "http://dl.ncsbe.gov/ENRS/2020_11_03/results_pct_20201103.zip"
    temp = tempfile()
    download.file(url, temp, quiet=T)
    raw = read_tsv(unz(temp, "results_pct_20201103.txt"),
                   col_types="ccciccccddddddc")
    unlink(temp)

    counties = tibble(
        county = c("ALAMANCE", "ALEXANDER", "ALLEGHANY", "ANSON", "ASHE", "AVERY",
                   "BEAUFORT", "BERTIE", "BLADEN", "BRUNSWICK", "BUNCOMBE", "BURKE",
                   "CABARRUS", "CALDWELL", "CAMDEN", "CARTERET", "CASWELL", "CATAWBA",
                   "CHATHAM", "CHEROKEE", "CHOWAN", "CLAY", "CLEVELAND", "COLUMBUS",
                   "CRAVEN", "CUMBERLAND", "CURRITUCK", "DARE", "DAVIDSON", "DAVIE",
                   "DUPLIN", "DURHAM", "EDGECOMBE", "FORSYTH", "FRANKLIN", "GASTON",
                   "GATES", "GRAHAM", "GRANVILLE", "GREENE", "GUILFORD", "HALIFAX",
                   "HARNETT", "HAYWOOD", "HENDERSON", "HERTFORD", "HOKE", "HYDE",
                   "IREDELL", "JACKSON", "JOHNSTON", "JONES", "LEE", "LENOIR",
                   "LINCOLN", "MCDOWELL", "MACON", "MADISON", "MARTIN", "MECKLENBURG",
                   "MITCHELL", "MONTGOMERY", "MOORE", "NASH", "NEW HANOVER",
                   "NORTHAMPTON", "ONSLOW", "ORANGE", "PAMLICO", "PASQUOTANK",
                   "PENDER", "PERQUIMANS", "PERSON", "PITT", "POLK", "RANDOLPH",
                   "RICHMOND", "ROBESON", "ROCKINGHAM", "ROWAN", "RUTHERFORD",
                   "SAMPSON", "SCOTLAND", "STANLY", "STOKES", "SURRY", "SWAIN",
                   "TRANSYLVANIA", "TYRRELL", "UNION", "VANCE", "WAKE", "WARREN",
                   "WASHINGTON", "WATAUGA", "WAYNE", "WILKES", "WILSON", "YADKIN", "YANCEY"),
        fips  = c(37001L,37003L,37005L, 37007L,37009L,37011L,37013L,37015L,37017L,
                  37019L,37021L,37023L,37025L,37027L,37029L,37031L, 37033L,37035L,
                  37037L,37039L,37041L,37043L,37045L, 37047L,37049L,37051L,37053L,
                  37055L,37057L, 37059L,37061L,37063L,37065L,37067L,37069L,37071L,
                  37073L,37075L,37077L,37079L,37081L,37083L, 37085L,37087L,37089L,
                  37091L,37093L,37095L,37097L, 37099L,37101L,37103L,37105L,37107L,
                  37109L,37111L, 37113L,37115L,37117L,37119L,37121L,37123L, 37125L,
                  37127L,37129L,37131L,37133L,37135L,37137L, 37139L,37141L,37143L,
                  37145L,37147L,37149L,37151L, 37153L,37155L,37157L,37159L,37161L,
                  37163L, 37165L,37167L,37169L,37171L,37173L,37175L,37177L, 37179L,
                  37181L,37183L,37185L,37187L,37189L, 37191L,37193L,37195L,37197L,37199L)
        )

    raw %>%
        filter(`Contest Name` == "US PRESIDENT") %>%
        select(county=County, party=`Choice Party`, votes=`Vote For`, total=`Total Votes`) %>%
        group_by(county, party) %>%
        summarize(rep = sum(total>0),
                  precincts = n(),
                  votes = sum(total)) %>%
        group_by(county) %>%
        mutate(total = sum(votes)) %>%
        ungroup %>%
        filter(party %in% c("DEM", "REP")) %>%
        mutate(party = if_else(party=="DEM", "dem", "gop")) %>%
        pivot_wider(names_from=party, values_from=votes) %>%
        mutate(twop = dem + gop, abbr="NC") %>%
        left_join(counties, by="county") %>%
        select(abbr, fips, rep, precincts, dem, gop, twop, total)
}

# not working
pull_returns.NV = function() {
    prec_url = "https://silverstateelection.nv.gov/_xml/CountyIndex.xml"
    vote_url = "https://silverstateelection.nv.gov/_xml/Homepage.xml"
    # download both
    #system2("scripts/NV.sh")
    browseURL(prec_url)
    browseURL(vote_url)

    counties = tibble(fips=c(32001L,32003L,32005L,32007L,32009L,32011L,32013L,32015L,32017L,
                             32019L,32021L,32023L,32027L,32029L,32031L,32033L,32510L),
                      county=c("Churchill","Clark","Douglas","Elko","Esmeralda","Eureka",
                               "Humboldt","Lander","Lincoln","Lyon","Mineral","Nye","Pershing",
                               "Storey","Washoe","White Pine","Carson City"))

    if (!tryCatch({
        raw_prec = read_xml("data/pulled/NV_prec.xml")
        raw_votes = read_xml("data/pulled/NV_votes.xml")
        T
    }, error = function(e) F)) return(NULL)

    precincts = xml_find_all(raw_prec, "County") %>%
        map_dfr(function(cty) {
            tibble(county = xml_attr(cty, "CountyName"),
                   precincts = as.numeric(xml_attr(cty, "TotalPrecincts")),
                   rep = as.numeric(xml_attr(cty, "PrecinctsReported"))
            )
        })

    xml_find_all(raw_votes, ".//Race[@RaceID='1']/Candidate") %>%
        map_dfr(function(cand) {
            props = xml_attrs(cand)
            cty_names = na.omit(str_match(names(props), "(.+)Votes")[,2])
            # match to names with spaces
            cty_names = precincts$county[match(cty_names, str_replace(precincts$county, " ", ""))]
            cty_votes = as.numeric(props[str_ends(names(props), ".Votes")])
            tibble(party = props["Party"],
                   county = cty_names,
                   votes = cty_votes)
        }) %>%
        group_by(county) %>%
        mutate(total = sum(votes)) %>%
        ungroup %>%
        filter(party %in% c("DEM", "REP")) %>%
        mutate(party = if_else(party == "DEM", "dem", "gop")) %>%
        pivot_wider(names_from=party, values_from=votes) %>%
        mutate(twop = dem+gop) %>%
        left_join(precincts, by="county") %>%
        left_join(counties, by="county") %>%
        select(fips, rep, precincts, dem, gop, twop, total)
}

# not working
pull_returns.TX = function() {
    url = "https://results.texas-election.com/static/data/election/44144/108/County.json"
    raw = read_json(url)
    conn = base::url(url, headers=list(
                          "authority"="results.texas-election.com",
                          "referer"="https://results.texas-election.com/county",
                          "sec-fetch-site"="same-origin",
                          "sec-fetch-mode"="cors",
                          "sec-fetch-dest"="empty",
                          "cookie"="__cfduid=da633f56df5db9dda6c939aad4b51efcf1604113553; __cflb=0H28vXUGMBPc1d121iUdKmJCgpSmrjSRSFhRUvGrabv"
                      ))
}

if (!interactive()) {
    ret = pull_returns()
    dir.create("data/pulled", showWarnings = FALSE)
    write_rds(ret, "data/pulled/returns.rdata")
}

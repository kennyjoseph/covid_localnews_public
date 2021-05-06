library(tidytext)
library(usmap)
library(urbnmapr) 
library(fixest)
library(car)
library(data.table)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(maps)
library(ggrepel)
library(visreg)
library(mgcv)
library(Hmisc)
library(usmap)
library(tidyverse)
library(GGally)
library(cowplot)
library(ggpubr)
library(plyr)
library(scales)
fill_datatable_na_with_zero <- function(DT){
  for(j in seq_along(DT)){
    set(DT, i = which(is.na(DT[[j]]) & is.numeric(DT[[j]])), j = j, value = 0)
  }
}

two_sd_scale <- function(d){
  return( (d-mean(d[!is.na(d)]))/(2*sd(d[!is.na(d)])))
}

theme_set(theme_minimal(20))

get_weekly_covid <- function(weekly_covid, groupby_var){
  weekly_covid <- weekly_covid[order(week_num)]
  
  weekly_covid[, lag1_cum_cases := shift(cum_cases,1), by=groupby_var]
  weekly_covid[, lag2_cum_cases := shift(cum_cases,2), by=groupby_var]
  weekly_covid[, n_cases := cum_cases - lag1_cum_cases]
  weekly_covid[, lag1_n_cases := lag1_cum_cases - lag2_cum_cases]
  
  weekly_covid[, lag1_cum_deaths := shift(cum_deaths,1), by=groupby_var]
  weekly_covid[, lag2_cum_deaths := shift(cum_deaths,2), by=groupby_var]
  weekly_covid[, n_deaths := cum_deaths - lag1_cum_deaths]
  weekly_covid[, lag1_n_deaths := lag1_cum_deaths - lag2_cum_deaths]
  
  # remove the few points early on where we don't have case data
  weekly_covid<- weekly_covid[!is.na(lag1_cum_cases) & 
                                             !is.na(lag2_cum_cases) & 
                                             !is.na(lag1_cum_deaths) & 
                                             !is.na(lag2_cum_deaths)]
  
  weekly_covid[n_cases < 0 ]$n_cases <- 0
  weekly_covid[n_deaths < 0 ]$n_deaths <- 0
  weekly_covid[lag1_n_cases < 0 ]$lag1_n_cases <- 0
  weekly_covid[lag1_n_deaths < 0 ]$lag1_n_deaths <- 0
  
  colnames <-  c("n_cases","lag1_n_cases","cum_cases","lag1_cum_cases","lag2_cum_cases",
                 "n_deaths","lag1_n_deaths","cum_deaths","lag1_cum_deaths","lag2_cum_deaths")
  setnames(weekly_covid,colnames,paste(groupby_var, colnames,sep="_"))
  return(weekly_covid)
  
}


get_coef <- function(model, mod_name){
  df <- data.table(tidy(model))
  df[, name := mapvalues(term,
                         c("week_num",
                           "country_n_cases_per1k_log_norm",
                           "fips_n_cases_per1k_log_norm",
                           "state_n_cases_per1k_log_norm",
                           "country_n_deaths_per1k_log_norm",
                           "state_n_deaths_per1k_log_norm",
                           "fips_n_deaths_per1k_log_norm",
                           "country_lag1_n_cases_per1k_log_norm",
                           "fips_lag1_n_cases_per1k_log_norm",
                           "state_lag1_n_cases_per1k_log_norm",
                           "country_lag1_n_deaths_per1k_log_norm",
                           "state_lag1_n_deaths_per1k_log_norm",
                           "fips_lag1_n_deaths_per1k_log_norm"),
                         c("Week Indicator",
                           "Cases - U.S.",
                           "Cases - County",
                           "Cases - State",
                           "Deaths - U.S.",
                           "Deaths - County",
                           "Deaths - State",
                           "Cases - U.S.",
                           "Cases - County",
                           "Cases - State",
                           "Deaths - U.S.",
                           "Deaths - County",
                           "Deaths - State"))]
  df$mod_name <- mod_name
  return(df)  
}


load_data <- function(){
  ########################################################################################
  ################ Read in article data ######################
  ########################################################################################

  d <- fread("data/url_to_flag_updatedtoFeb24_obitfilter.csv")
  article_counts = d[, list(ntotal=.N, 
                                        n_covid_limited_filter=sum(limited_covid_filter_flag), 
                                        n_covid_full_filter=sum(full_covid_filter_flag)),
                     by=.(sourcedomain_id,date)]
  # Now merge in the location data so we can merge with case counts
  source_to_location <- fread("data/source_to_location_data.csv")
  source_to_location$fips_covid <- source_to_location$fips
  source_to_location[source %in% c("forward","newyorkobserver","newyorkpost",
                                   "thevillager","thewaveoflongisland",
                                   "thequeenscourier","riverdalereview",
                                   "thequeensgazette") ]$fips_covid <- -1
  site_info = fread("data/site_info.csv")
  source_to_location <- merge(source_to_location, site_info, by="url")
  # NOTE: Same URL pointing to different locations. 
  # IF they are in the same county, they are, to our model, identical
  # So we should get rid of them,
  # If it's a different county, it just tells us that
  # the audience is broader
  source_to_location[url %in% source_to_location[duplicated(url)]$url]
  source_to_location <- source_to_location[!duplicated(source_to_location[,.(url,county)])]
  
  article_counts <- merge(article_counts, 
                          source_to_location[,.(sourcedomain_id,city,url,lon,lat,fips,fips_covid,title,rank,state)], 
                          by="sourcedomain_id")
  
  # aggregate to weekly, because of periodicals
  article_counts[, week_num:= (year(date)-2020)*52 + (week(date)-1)]
  weekly_articles <- article_counts[, list(n_total_articles = sum(ntotal),
                                           n_covid_limited_filter= sum(n_covid_limited_filter),
                                           n_covid_full_filter= sum(n_covid_full_filter)
  ), 
  by=.(sourcedomain_id,state,week_num,lon,lat,fips,fips_covid,title,rank)]
  
  ########################################################################################
  # Link weekly article counts to various case counts
  ########################################################################################
  
  covid_cases <- fread(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  covid_cases <- covid_cases[county != "Unknown"]
  # deal with NYC explicitly for the COVID data,
  # see: https://github.com/nytimes/covid-19-data#geographic-exceptions
  covid_cases[is.na(fips) & state=="New York"]$fips = -1
  
  covid_cases[, week_num:= (year(date)-2020)*52 + (week(date)-1)]
  # To avoid NA in death counts, Puerto Rico doesn't report
  covid_cases <- covid_cases[!is.na(deaths)]
  covid_cases$country <- "USA"
  
  weekly_covid <- covid_cases[, 
                              list(cum_cases=max(cases), cum_deaths=max(deaths)), 
                              by = c("week_num","fips","state","country")]
  weekly_county_covid_cases <- get_weekly_covid(weekly_covid,"fips")
  weekly_county_covid_cases$state <- NULL
  weekly_county_covid_cases$country <- NULL
  
  weekly_state_covid_cases <- get_weekly_covid(weekly_covid[, list(cum_cases=sum(cum_cases), 
                                                                   cum_deaths=sum(cum_deaths)), 
                                                            by=.(week_num,state)],"state")
  
  weekly_national_covid_cases <- get_weekly_covid(weekly_covid[, list(cum_cases=sum(cum_cases), 
                                                                      cum_deaths=sum(cum_deaths)), 
                                                               by=.(week_num,country)],"country")
  
  ########################################################################################
  # Combine article counts and COVID counts
  
  ########################################################################################
  
  weekly_aggd <- merge(weekly_articles, 
                       weekly_county_covid_cases,
                       by.x=c("week_num","fips_covid"),
                       by.y=c("week_num","fips"))
  weekly_aggd <- merge(weekly_aggd, 
                       weekly_state_covid_cases,
                       by.x=c("week_num","state"),
                       by.y=c("week_num","state"))
  weekly_aggd <- merge(weekly_aggd, 
                       weekly_national_covid_cases,
                       by=c("week_num"))
  weekly_aggd[, date := ymd("2020-01-01") + weeks(week_num)]
  
  ########################################################################################
  # Merge in election and census data
  ########################################################################################
  # MIT data, note that Alaska is not included in their data
  election_data <- fread(
    "https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
  
  weekly_aggd <- merge(weekly_aggd, election_data[, .(county,fips,trump16,clinton16,
                                                      total_population,white_pct,black_pct,
                                                      hispanic_pct,age65andolder_pct,lesshs_pct,
                                                      rural_pct,ruralurban_cc)], by= "fips")
  
  #### add current population estimates
  d <- fread("data/co-est2019-alldata.csv")
  
  popest <- d[, .(STATE,COUNTY,POPESTIMATE2019)]
  popest[,state_fips := str_pad(STATE,2,side="left",pad="0")]
  popest <- merge(popest,fips_info(),  by.x="state_fips",by.y='fips')
  popest[, county_fips := as.integer(paste0(STATE,str_pad(COUNTY,3,side="left",pad='0')))]
  weekly_aggd <- merge(weekly_aggd, 
                       popest[, list(state_popn=sum(POPESTIMATE2019)), by=full],
                       by.x="state",by.y="full")
  weekly_aggd <- merge(weekly_aggd,
                       popest[,.(county_fips,POPESTIMATE2019)],
                       by.x="fips",by.y="county_fips")
  weekly_aggd[, country_population := sum(popest$POPESTIMATE2019)]
  
  
  weekly_aggd[, lo_trump_vote_16 := log(trump16/clinton16)]
  ########################################################################################
  # Create Predictor vars
  ########################################################################################
  
  popn_rescale_vars <- c("fips_n_cases", "fips_lag1_n_cases", 
                         "fips_n_deaths", "fips_lag1_n_deaths",
                         "state_n_cases", "state_lag1_n_cases", 
                         "state_n_deaths", "state_lag1_n_deaths",
                         "country_n_cases","country_lag1_n_cases",
                         "country_n_deaths","country_lag1_n_deaths")
  rescale_vals <- c(rep("POPESTIMATE2019",4),rep("state_popn",4),rep("country_population",4))
  # rate variables
  for(ind in 1:12){
    var = popn_rescale_vars[ind]
    rescale_val = rescale_vals[ind]
    weekly_aggd$x <- weekly_aggd[,get(var)]*100000/(weekly_aggd[,get(rescale_val)])
    weekly_aggd$lx <- log(weekly_aggd$x+1)
    setnames(weekly_aggd, "x",paste0(var,"_per1k"))
    setnames(weekly_aggd, "lx",paste0(var,"_per1k_log"))

    weekly_aggd$x <- log((weekly_aggd[,get(var)]+1)/(weekly_aggd[,get(rescale_val)]+1))
    setnames(weekly_aggd, "x",paste0(var,"_lo"))
    
  } 
  
  # see supplement, misreporting details
  weekly_aggd <- weekly_aggd[state_n_cases_lo > -12.5]
  weekly_aggd[, fips_full := str_pad(fips,5,side="left",pad="0")]
  weekly_aggd[, perc_full := n_covid_full_filter / n_total_articles]
  weekly_aggd[, perc_lim := n_covid_full_filter / n_total_articles]
  
  
  d <- fread("data/cre-2018-a11.csv")
  d <- d[!is.na(county) & is.na(tract)]
  d[, fips_full := str_pad(geoid,5,side="left",pad="0")]
  
  weekly_aggd <- merge(weekly_aggd,d[,.(fips_full,
                                        predrt_0,
                                        predrt_3, 
                                        predrt_12,
                                        popuni)], by="fips_full")
  
  weekly_aggd[, log_pop :=   log(popuni)]
  weekly_aggd[, log_rank :=    log(1/rank)]
  weekly_aggd[is.na(log_rank)]$log_rank <- max(weekly_aggd[!is.na(log_rank)]$log_rank)
  
  d <- fread("https://raw.githubusercontent.com/kjhealy/us_elections_2020_csv/master/results_current.csv")
  d <- spread(d[race == "President" & lname %in% c("Trump","Biden") & !is.na(fips5)][,list(votes=sum(votes)), by=.(fips5,lname)],
              lname, votes)[, fips_full := str_pad(fips5,5,side="left",pad="0")]
  
  weekly_aggd <- merge(weekly_aggd, d, by="fips_full")
  weekly_aggd[, lo_trump_vote := log(Trump/Biden)]
  
  return(weekly_aggd)
}
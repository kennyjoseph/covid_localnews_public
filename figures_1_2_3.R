source("util.r")
weekly_aggd <- load_data()
# Drop b/c they lost the domain name
weekly_aggd <- weekly_aggd[sourcedomain_id != "riverdalereview-riverdalereview.com"]


########################################################################################
# Figure 1
########################################################################################
p1 <- ggplot(weekly_aggd, aes(date,perc_full))  +
          stat_smooth(color='black') +
          scale_y_continuous("Percent of Articles\nMentioning at least\n One COVID Keyword",
          labels=percent)+
          xlab("Date") +
          scale_x_date(breaks="1 month",labels=label_date("%B")) +
          theme(axis.text.x = element_text(angle=45,hjust=1))
ggsave("img/fig1a.pdf",p1,h=5,w=6)

county_map_data <- urbnmapr::counties %>%
  mutate(has_local_news = as.numeric(county_fips %in% weekly_aggd$fips_full))
county_map_local_news <- ggplot() +
  
  # here's where the plot actually happens               
  geom_polygon(data = county_map_data %>%
                 mutate(has_local_news = ifelse(is.na(has_local_news), 
                                                0, has_local_news)),
               aes(long, lat, group = group,
                   fill = factor(has_local_news, levels = c(1,0))),
               color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(name = "County Covered by\nLocal News Site?",
                    breaks = c(1,0),
                    values = c("black","grey"),
                    labels = c("Yes","No"))+
  labs(x= "", y = "",
       title = "") + 
  theme_map()
ggsave("img/county_map.pdf",county_map_local_news, h=8,w=11)


mlt <- melt(weekly_aggd,id="date", 
            measure=c("perc_lim","country_n_cases_per1k_log",
                      "country_n_deaths_per1k_log"))
mlt <- mlt[, list(value=mean(value)), by=.(date,variable)]
mlt[, v := (value-mean(value))/sd(value), by=variable]
p12 <- ggplot(mlt,aes(date,v,color=variable))  +
  geom_line(size=1.2) +
  xlab("Date") +
  ylab("Scaled and Centered\nMeasure")+
  scale_color_discrete("",labels=c("Percent of Articles\nAbout COVID",
                                   "Log(N) Cases\n(Country-wide)",
                                   "Log(N) Deaths\n(Country-wide)")) +
  scale_x_date(breaks="1 month",labels=label_date("%B")) +
  theme(axis.text.x = element_text(angle=45,hjust=1), legend.position="bottom") 
ggsave("img/rq1b.pdf",p12,h=7,w=10)



########################################################################################
# Modeling
########################################################################################

# logged measures

####################################################
#### RQ1
####################################################

for(sv in c("country_n_cases_per1k_log",
            "fips_n_cases_per1k_log",
            "state_n_cases_per1k_log",
            "country_n_deaths_per1k_log",
            "state_n_deaths_per1k_log",
            "fips_n_deaths_per1k_log",
            "country_lag1_n_cases_per1k_log",
            "fips_lag1_n_cases_per1k_log",
            "state_lag1_n_cases_per1k_log",
            "country_lag1_n_deaths_per1k_log",
            "state_lag1_n_deaths_per1k_log",
            "fips_lag1_n_deaths_per1k_log")){
  
  if(length(grep(paste0(sv,"_norm"),names(weekly_aggd))) > 0){
    next
  }
  print(sv)
  weekly_aggd[, c(paste0(sv,"_norm")) :=  (get(sv)-mean(get(sv)))/sd(get(sv))]
}
m_nolag<- feglm(n_covid_full_filter/n_total_articles~week_num+
            country_n_cases_per1k_log_norm+
            fips_n_cases_per1k_log_norm+
            state_n_cases_per1k_log_norm+
            country_n_deaths_per1k_log_norm+
            state_n_deaths_per1k_log_norm+
            fips_n_deaths_per1k_log_norm | sourcedomain_id, 
        data=weekly_aggd,
        se="cluster",
        family="binomial",
        weights=weekly_aggd$n_total_articles)

m_noweek<- feglm(n_covid_full_filter/n_total_articles~
                  country_n_cases_per1k_log_norm+
                  fips_n_cases_per1k_log_norm+
                  state_n_cases_per1k_log_norm+
                  country_n_deaths_per1k_log_norm+
                  state_n_deaths_per1k_log_norm+
                  fips_n_deaths_per1k_log_norm | sourcedomain_id, 
                data=weekly_aggd,
                se="cluster",
                family="binomial",
                weights=weekly_aggd$n_total_articles)

m_all_lag <- feglm(n_covid_full_filter/n_total_articles~week_num+
                    country_lag1_n_cases_per1k_log_norm+
                    fips_lag1_n_cases_per1k_log_norm+
                    state_lag1_n_cases_per1k_log_norm+
                    country_lag1_n_deaths_per1k_log_norm+
                    state_lag1_n_deaths_per1k_log_norm+
                    fips_lag1_n_deaths_per1k_log_norm | sourcedomain_id, 
          data=weekly_aggd,
          se="cluster",
          family="binomial",
          weights=weekly_aggd$n_total_articles)

m_lag_deaths <- feglm(n_covid_full_filter/n_total_articles~week_num+
                     country_n_cases_per1k_log_norm+
                     fips_n_cases_per1k_log_norm+
                     state_n_cases_per1k_log_norm+
                     country_lag1_n_deaths_per1k_log_norm+
                     state_lag1_n_deaths_per1k_log_norm+
                     fips_lag1_n_deaths_per1k_log_norm | sourcedomain_id, 
                   data=weekly_aggd,
                   se="cluster",
                   family="binomial",
                   weights=weekly_aggd$n_total_articles)

m_lag_cases <- feglm(n_covid_full_filter/n_total_articles~week_num+
                        country_lag1_n_cases_per1k_log_norm+
                        fips_lag1_n_cases_per1k_log_norm+
                        state_lag1_n_cases_per1k_log_norm+
                        country_n_deaths_per1k_log_norm+
                        state_n_deaths_per1k_log_norm+
                        fips_n_deaths_per1k_log_norm | sourcedomain_id, 
                      data=weekly_aggd,
                      se="cluster",
                      family="binomial",
                      weights=weekly_aggd$n_total_articles)


m_lim_filter<- feglm(n_covid_limited_filter/n_total_articles~week_num+
            country_n_cases_per1k_log_norm+
            fips_n_cases_per1k_log_norm+
            state_n_cases_per1k_log_norm+
            country_n_deaths_per1k_log_norm+
            state_n_deaths_per1k_log_norm+
            fips_n_deaths_per1k_log_norm | sourcedomain_id, 
          data=weekly_aggd,
          se="cluster",
          family="binomial",
          weights=weekly_aggd$n_total_articles)


m_no_mass <- feglm(n_covid_full_filter/n_total_articles~week_num+
             country_n_cases_per1k_log_norm+
             fips_n_cases_per1k_log_norm+
             state_n_cases_per1k_log_norm+
             country_n_deaths_per1k_log_norm+
             state_n_deaths_per1k_log_norm+
             fips_n_deaths_per1k_log_norm | sourcedomain_id, 
           data=weekly_aggd[state!="Massachusetts"],
           se="cluster",
           family="binomial",
           weights=weekly_aggd[state!="Massachusetts"]$n_total_articles)

m_no_state<- feglm(n_covid_full_filter/n_total_articles~week_num+
             country_n_cases_per1k_log_norm+
             fips_n_cases_per1k_log_norm+
             country_n_deaths_per1k_log_norm+
             fips_n_deaths_per1k_log_norm | sourcedomain_id, 
           data=weekly_aggd,
           se="cluster",
           family="binomial",
           weights=weekly_aggd$n_total_articles)

m_no_county<- feglm(n_covid_full_filter/n_total_articles~week_num+
             country_n_cases_per1k_log_norm+
             state_n_cases_per1k_log_norm+
             country_n_deaths_per1k_log_norm+
             state_n_deaths_per1k_log_norm | sourcedomain_id, 
           data=weekly_aggd,
           se="cluster",
           family="binomial",
           weights=weekly_aggd$n_total_articles)

etable(m_nolag, m_all_lag,m_lag_cases,m_lag_deaths, tex = T)
etable(m_nolag, m_noweek, tex = T)
etable(m_nolag, m_lim_filter,m_no_mass,m_no_state,m_no_county,tex=T)




fin_mod <- rbind(
    get_coef(m_nolag, "Full filter,\nAll Variables"),
    get_coef(m_no_county, "Full filter,\nNo County Variables"),
    get_coef(m_no_state, "Full filter,\nNo State Variables"),
    get_coef(m_lim_filter, "Limited filter,\nAll Variables"))

rq1_res_plot <- ggplot(fin_mod, aes(name, exp(estimate),
                               ymin=exp(estimate-2*std.error),
                               ymax=exp(estimate+2*std.error),
                               color=mod_name)) + 
  geom_hline(yintercept = 1,color='red',linetype='dashed' ) + 
  geom_pointrange(position=position_dodge(.7),size=.6) + 
  coord_flip() + xlab("Predictor") +
  scale_color_discrete("Model")+ 
  theme(legend.position="bottom")+
  scale_y_log10("Odds Ratio") + 
  guides(color=guide_legend(nrow=2))
rq1_res_plot
ggsave("img/rq1a.pdf",rq1_res_plot,h=7,w=10)

vif(glm(n_covid_full_filter/n_total_articles~week_num+
          country_n_cases_per1k_log_norm+
          fips_n_cases_per1k_log_norm+
          state_n_cases_per1k_log_norm+
          country_n_deaths_per1k_log_norm+
          state_n_deaths_per1k_log_norm+
          fips_n_deaths_per1k_log_norm + sourcedomain_id, 
        data=weekly_aggd,
        family="binomial",
        weights=n_total_articles))


fixed_effects <- fixef(m_nolag)
fixed_effects2 <- fixef(m_lim_filter)

source_vars <- c("lo_trump_vote",
                 "log_pop",
                 "log_rank",
                 #"predrt_0",
                 "predrt_12",
                 "predrt_3")

limited_fe <- data.table(sourcedomain_id=names(fixed_effects$sourcedomain_id), 
                         lim_fe = as.vector(fixed_effects$sourcedomain_id))
full_fe <- data.table(sourcedomain_id=names(fixed_effects2$sourcedomain_id), 
                      full_fe = as.vector(fixed_effects2$sourcedomain_id))
full_fe[, full_fe := full_fe-mean(full_fe)]
limited_fe[, lim_fe := lim_fe-mean(lim_fe)]
fe <- merge(limited_fe,full_fe, by="sourcedomain_id")
fe <- merge(fe,
            weekly_aggd[, .SD[1,], by=sourcedomain_id]
                  [, c("sourcedomain_id","state",source_vars),with=F], 
        by = "sourcedomain_id")
fe <- merge(fe, weekly_aggd[, list(total_articles=sum(n_total_articles)),
                            by=sourcedomain_id],
            bu="sourcedomain_id")
ggsave("img/lim_fe_vs_full_fe.pdf",
       ggplot(fe, aes(full_fe,lim_fe))+geom_point() +
         stat_cor() + labs(x="Fixed Effect Size\n(Full Keyword Filter)",
                           y= "Fixed Effect Size\n(Limited Keyword Filter)"),
       h=6,w=6)

for(sv in source_vars){
  print(sv)
  fe[, c(sv) :=  (get(sv)-mean(get(sv)))/sd(get(sv))]
}

mlt <- melt(fe[, -"state",with=F], id=c("sourcedomain_id",
                                        "total_articles",
                                        "lim_fe","full_fe"))
ggpairs(fe[,source_vars, with=F])

mlt[, name := mapvalues(variable,
                        c("log_pop","lo_trump_vote","log_rank",
                          "predrt_0","predrt_12","predrt_3"),
                        c("Log(Population)", 
                          "Log-odds of Voting\nTrump in 2020",
                          "Log(Alexa rank\n of site)",
                          "Pred. Rate\n0 Risk Factors",
                          "Pred. Rate\n1-2 Risk Factors",
                          "Pred. Rate\n3+ Risk Factors"))]
mlt$name <- factor(mlt$name, levels=c(
                                      "Log(Alexa rank\n of site)",
                                      "Log-odds of Voting\nTrump in 2020",
                                      "Pred. Rate\n0 Risk Factors",
                                      "Log(Population)", 
                                      "Pred. Rate\n1-2 Risk Factors",
                                      "Pred. Rate\n3+ Risk Factors"))
fe_plot <- ggplot(mlt[variable != "predrt_0"], aes(value,exp(full_fe)))+ 
  geom_point(alpha=.5) + 
  geom_smooth(method="lm") + 
  geom_hline(yintercept=1,color='red',linetype='dashed')+
  stat_cor() + facet_wrap(~name,scales="free_x",nrow=1) +
  scale_y_log10(
    "Change in Odds of a COVID-related\nArticle from Outlet on Average")+
  xlab("Centered and Scaled Variable") +
  theme_bw(20) 
fe_plot
ggsave("img/rq1c.pdf",fe_plot,h=5,w=14)


summary(lm(full_fe~log_rank+lo_trump_vote+log_pop+predrt_12+predrt_3,fe))
summary(lm(lim_fe~log_rank+lo_trump_vote+log_pop+predrt_12+predrt_3,fe))


d <- fread("data/synd_counts_shallow.csv")
setnames(d, "Source-domain", "sourcedomain_id")
fe <- merge(fe, d[, .(sourcedomain_id,percent_synd)], by="sourcedomain_id")
fe$percent_synd <- log(fe$percent_synd+.01)
synd <- ggplot(fe, aes(percent_synd,exp(full_fe)))+ 
  geom_point(alpha=.5) + 
  geom_smooth(method="lm") + 
  geom_hline(yintercept=1,color='red',linetype='dashed')+
  stat_cor() +
  scale_y_log10(
    "Change in Odds of a COVID-related\nArticle from Outlet on Average")+
  xlab("Log(Percent Syndicated)") +
  theme_bw(20) 
ggsave("img/percent_synd.pdf",synd, h=7,w=7)

source("util.r")
weekly_aggd <- load_data()
# Drop b/c they lost the domain name
weekly_aggd <- weekly_aggd[sourcedomain_id != "riverdalereview-riverdalereview.com"]


##########################################
########## Data description plot ########
##########################################
p_dat <- melt(weekly_aggd[, 
                          list(  
                            n_covid_limited_filter= sum(n_covid_limited_filter)/sum(n_total_articles),
                            n_covid_full_filter= sum(n_covid_full_filter)/sum(n_total_articles),
                            covid_cases_national = mean(country_n_cases),
                            covid_deaths_national = mean(country_n_deaths)),
                          by=.(week_num)], 
              id="week_num")
p_dat[, scale_val := two_sd_scale(value), by=variable]
ggplot(p_dat, aes(week_num,scale_val,color=variable)) +
  geom_point()+
  stat_smooth(se=F) 


nweeks <- length(unique(weekly_aggd$week_num))
news_summ_counts <- weekly_aggd[, list( n_covid_limited_filter= sum(n_covid_limited_filter),
                                        n_covid_full_filter= sum(n_covid_full_filter),
                                        total=sum(n_total_articles)/nweeks), 
                                by=sourcedomain_id]
p1 <- ggplot(news_summ_counts, aes(total))+geom_histogram(binwidth=10) +
  ylab("N. News Sources") + xlab("Avg. N. of Articles Per Week")
p1

p2 <- ggplot(melt(weekly_aggd[, 
                              list( 
                                n_covid_limited_filter= sum(n_covid_limited_filter),
                                n_covid_full_filter= sum(n_covid_full_filter),
                                ntot=sum(n_total_articles)), 
                              by=date], 
                  id="date"), 
             aes(date, value,color=variable,fill=variable)) + 
  stat_smooth(alpha=.2) +
  scale_y_continuous("Total Number of Articles") + 
  scale_color_discrete("",labels=c('COVID-related Articles (full keywords)',
                                   "COVID-related Articles (limited keywords)",
                                   "All Articles"))+
  scale_fill_discrete("",labels=c('COVID-related Articles (full keywords)',
                                  "COVID-related Articles (limited keywords)",
                                  "All Articles"))+
  theme(legend.position = "bottom") + xlab("") + 
  guides(color=guide_legend(nrow=4,byrow=TRUE),fill=guide_legend(nrow=4,byrow=TRUE)) +
  geom_point(alpha=.5) +
  scale_x_date(breaks="2 month",labels=label_date("%B"))
p2 

p3 <- ggplot(melt(weekly_aggd[, 
                              list(  
                                n_covid_limited_filter= sum(n_covid_limited_filter)/sum(n_total_articles),
                                n_covid_full_filter= sum(n_covid_full_filter)/sum(n_total_articles)), 
                              by=sourcedomain_id], 
                  id="sourcedomain_id"), 
             aes(value,fill=variable)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous(labels=percent)+
  xlab("Percent articles\nabout COVID per source") +
  ylab("Density")  +
  scale_fill_discrete("",labels=c("COVID-related Articles (limited keywords)",
                                  "COVID-related Articles (full keywords)")
  )+
  theme(legend.position = "bottom") + 
  guides(fill=guide_legend(nrow=3,byrow=TRUE))
p3

source_to_location <- fread("data/source_to_location_data.csv")
p4 <- ggplot(source_to_location[,.N, by= state], aes(N,reorder(state,N))) + 
  geom_col() + xlab("Number of Newspaper Agencies") + ylab("State") + theme_minimal(12)

table(source_to_location[,.N, by=fips]$N)

ggsave("img/perc_covid.pdf",plot_grid(p2,p3,p1,p4,
                                      nrow=2,
                                      labels=c("A","B","C","D")),h=12,w=12)


#########################################3
### Anomolies in case/death data
##########################################
##### These plots reveal some odd results...mis/non-reporting

pl <- ggplot(weekly_aggd, aes(week_num,state_n_cases_lo))+
  geom_point()  + 
  facet_wrap(~state) +
  theme_bw(8) +stat_smooth() +
  geom_hline(yintercept = -12,color='red')
ggsave("img/state_cases.pdf",pl,h=10,w=10)
# address mass and RI issues
weekly_aggd <- weekly_aggd[state_n_cases_lo > -12.5]

pl <- ggplot(weekly_aggd, aes(week_num,state_n_deaths_lo))+
  geom_point()  + 
  facet_wrap(~state) +
  theme_bw(8) +stat_smooth() +
  geom_hline(yintercept = -15,color='red')
ggsave("img/state_deaths.pdf",pl,h=10,w=10)
# address NY and NJ issues
weekly_aggd <- weekly_aggd[state_n_deaths_lo > -15.5]

pl <- ggplot(weekly_aggd, aes(week_num,fips_n_cases_lo))+
  geom_point()  + 
  facet_wrap(~fips) +
  theme_bw(8) +stat_smooth() 
ggsave("img/counties_cases.pdf",pl,h=20,w=20)

pl <- ggplot(weekly_aggd, aes(week_num,fips_n_deaths_lo))+
  geom_point()  + 
  facet_wrap(~fips) +
  theme_bw(8) +stat_smooth() 
ggsave("img/counties_deaths.pdf",pl,h=20,w=20)



source("util.r")

library(MASS)
A1_COVID_TOPICS <-c(1,14,17,30,39,44,59,74,58,37)
A2_COVID_TOPICS <- c(1,14,17,30,39,44,59,74,56)
a1 <- rep(0, 79)
a1[A1_COVID_TOPICS] <- 1
a2 <- rep(0, 79)
a2[A2_COVID_TOPICS] <- 1
library(irr)
kripp.alpha(rbind(a1,a2))
COVID_TOPICS <- c(intersect(A1_COVID_TOPICS,A2_COVID_TOPICS),56,58)

thetas_from_full <- fread("data/theta_Feb27update_withdomtopic.csv")
thetas_from_full[, sourcedomain_id := sub("\"","", sourcedomain_id)]
thetas_from_full[, sourcedomain_id := sub("_[0-9]+","",sourcedomain_id)]
thetas_from_full[, sourcedomain_id := sub("-202[01]-[0-9]+-[0-9]+$",
                                        "", 
                                        sourcedomain_id)]

thetas_from_full[, id := sub("_[0-9]+","",id)]
thetas_from_full[, day := ymd(str_sub(id,-10,-1))]
thetas_from_full[, week_num := (year(day)-2020)*52 + (week(day)-1)]
thetas_from_full[, date := ymd("2020-01-01") + weeks(week_num)]
thetas_from_full <- thetas_from_full[ date >= ymd("2020-04-01")]

########## Plot Figure 3 ###################3
topic_per_day = melt(thetas_from_full, 
                     id="date", 
                     measure = paste0("V",1:79))[, sum(value), by =.(date,variable)]
topic_per_day[, variable := as.integer(sub("V","", variable))]

topic_to_term <- fread("data/stm_res.tsv")
topic_to_term$terms <- apply(str_split_fixed(topic_to_term$terms,", ",6), 1, 
                             function(l){paste0(paste(l[1:3], collapse=", "),"\n",
                                                paste(l[4:6], 
                                                      collapse=", "))})

topic_per_day <- merge(topic_per_day, topic_to_term, by.x="variable", by.y="topic_num")
total_topic <- topic_per_day[,sum(V1), by=.(variable,terms)][order(-V1)]

top_time <- ggplot(topic_per_day, aes(date,V1))+stat_smooth(se=F) +
  facet_wrap(~terms) + theme_bw(10)
ggsave("img/all_docs_topics_over_time.pdf",top_time,h=20,w=20)

topics_for_fig1_covid <- c(14,17,44,39,59)
topics_for_fig1_noncovid <- c(71,49,50,72,38)
topics_for_fig1 <- c(topics_for_fig1_covid,topics_for_fig1_noncovid)
fig1_topics <- topic_per_day[variable %in% topics_for_fig1]
fig1_topics[, is_covid := ifelse(variable %in% topics_for_fig1_covid, 
                                 "COVID-related", "Not COVID-Related")]
fig1_topics[, terms := factor(terms,
                              levels=c(total_topic[variable %in% topics_for_fig1_covid]$terms,
                                total_topic[variable %in% topics_for_fig1_noncovid]$terms))]
fig1_topics <- merge(fig1_topics,topic_per_day[, list(tot=sum(V1)),by=date], by="date")
fig1_topics[, prop := V1/tot]
p3 <- ggplot(fig1_topics, aes(date,prop,color=is_covid,fill=is_covid))  +
  geom_point(size=3) +
  scale_y_continuous("Expected %age Articles On Topic",
                     labels=scales::percent_format(accuracy = .5))+
  xlab("Date") +
  facet_wrap(~terms, nrow=2,scales="free_y") + 
  scale_x_date(breaks="2 months",labels=label_date("%B")) +
  theme_minimal(16) +
  theme(axis.text.x = element_text(angle=45,hjust=1), 
        legend.position = "none") +
  geom_line()
p3
ggsave("img/topic_prop.pdf",p3,h=6,w=16)


######################### Results Figure 4

thetas_covid <- melt(thetas_from_full, id="sourcedomain_id",
                     measure=paste0("V",COVID_TOPICS))
thetas_covid <- thetas_covid[, sum(value), by=.(sourcedomain_id,variable)]
thetas_covid_spr <- spread(thetas_covid, variable,V1,fill=0)
thetas_covid <-  merge(thetas_covid_spr, thetas_covid[, list(total=sum(V1)), by=sourcedomain_id])

for(x in paste0("V",COVID_TOPICS)){
  print(x)
  thetas_covid[, c(x) :=  (get(x)/total)]
}

thetas_covid_mlt <-   melt(thetas_covid, id="sourcedomain_id",
                           measure=paste0("V",COVID_TOPICS))
thetas_covid_mlt[, variable := as.integer(sub("V","", variable))]
thetas_covid_mlt <- merge(thetas_covid_mlt, 
                          topic_to_term, 
                          by.x="variable", by.y="topic_num")
thetas_covid_mlt <- merge(thetas_covid_mlt, fe,by="sourcedomain_id")


thetas_covid_mlt$terms <- factor(thetas_covid_mlt$terms, levels=thetas_covid_mlt[,sum(value),by=terms][order(-V1)]$terms)

ggplot(thetas_covid_mlt[sourcedomain_id != "thestar-pipestonestar.com"], 
       aes(terms, value,color=lo_trump_vote,group=sourcedomain_id))+
  geom_point() + 
  geom_line(alpha=.8) + 
  theme(legend.position="none", 
        axis.text.x=element_text(angle=45,hjust=1)) + 
  scale_color_gradientn(colors=c("blue","white","red"))

thetas_covid_mlt <- merge(thetas_covid_mlt, 
            weekly_aggd[, list(deaths=sum(fips_cum_deaths)/POPESTIMATE2019[1], 
                               cases=sum(fips_cum_cases)/POPESTIMATE2019[1]), 
                        by=.(sourcedomain_id)], by="sourcedomain_id")
m <- data.table()
for(v in c("cases","deaths",source_vars)){
  z <- thetas_covid_mlt[sourcedomain_id != "thestar-pipestonestar.com", list(corv = cor(value,get(v)),
                               p = cor.test(value,get(v))$p.value),
                        by=terms]
  z$v <- v
  m <- rbind(m,
             z
  )

}
m[, name := mapvalues(v,
                        c("log_pop","lo_trump_vote","log_rank",
                          "predrt_0","predrt_12","predrt_3"),
                        c("Log(Population)", 
                          "Log-odds of Voting\nTrump in 2020",
                          "Log(Alexa rank\n of site)",
                          "Pred. Rate\n0 Risk Factors",
                          "Pred. Rate\n1-2 Risk Factors",
                          "Pred. Rate\n3+ Risk Factors"))]
m$name <- factor(m$name, levels=c(
  "Log-odds of Voting\nTrump in 2020",
  "Log(Population)",
  "Pred. Rate\n1-2 Risk Factors",
  "Log(Alexa rank\n of site)",
  "Pred. Rate\n0 Risk Factors",
  "Pred. Rate\n3+ Risk Factors"))
m$terms <- factor(m$terms,
  levels= c(
    "suppli, equip, shortag\nmanufactur, associ, ventil, product",
    "patient, hospit, health\nmedic, clinic, nurs, diseas",
    "vaccin, dose, pfizer\nmoderna, fda, variant, biontech",
            "virus, infect, reopen\nhealth, coronavirus, governor, state",
            "unemploy, worker, employe\njobless, job, employ, evict",
            "restaur, store, busi\ncustom, retail, dine, reopen",
    
    "test, symptom, covid\nhealth, posit, virus, facil",
            "school, student, teacher\ndistrict, classroom, educ, superintend",
            "case, icu, pipeston\ndeath, hospit, mdh, total",
            "mask, wear, distanc\nsanit, guidelin, social, disinfect"))

plt <- ggplot(m[v != "predrt_3" & 
                  terms !="suppli, equip, shortag\nmanufactur, associ, ventil, product" &
                  !is.na(name)], 
              aes(terms,name, fill=ifelse(p < .01, corv,0)))+ 
  geom_tile() + 
  scale_fill_gradientn("Correlation", colors=c("orange","white","blue")) + 
  theme_classic(20)+ xlab("Topics")+ylab("County-level Variable")+ 
  theme(axis.text.x=element_text(angle=45,hjust=1)) 
plt
ggsave("img/corr_vars_topics.pdf",plt,w=14,h=8)


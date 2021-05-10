library(stm)

k = 79

data <- read.csv("stm_ready_withdaynum.csv")
processed <- textProcessor(data$article, metadata = data)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

#get names of the documents
labels = meta$article_id
labels_df <- data.frame(labels)

prevFit <- stm(documents = out$documents, vocab = out$vocab, K = k, prevalence =~ s(daynum), max.em.its = 100, data = out$meta, init.type = "Spectral")

#get the doc to topic dist back 
d2t <- as.data.frame(prevFit$theta)
outdf <- cbind(labels_df, d2t)
write.csv(outdf, "./theta.csv")

labelTopics(prevFit)
plot(prevFit, type = "summary", xlim = c(0, 0.5))

prep <- estimateEffect(1:k ~ s(daynum), prevFit, meta = out$meta, uncertainty = "Global")

l <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79)

summary(prep, topics = l)

monthseq <- seq(from = as.Date("2020-04-01"), to = as.Date("2021-02-24"), by = "month")
monthnames <- months(monthseq, abbr=TRUE)

for (val in l){
plot(prep, "daynum", model = prevFit, topics = val, method = "continuous", xlab = "Time (2020 to 2021)", linecol = "purple3", ylim = c(0, 0.10), printlegend = FALSE, xaxt = "n", cex.lab=1.5, cex.axis=1.8, cex.main=1.5, cex.sub=1.5, lwd = 5)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)), labels = monthnames, cex.lab=1.5, cex.axis=1.8, cex.main=1.5, cex.sub=1.5)
}

## Code for County Political Interaction Plots <-------
#for (val in l){
#plot(prep, covariate = "daynum", model = prevFit, topics = val, method = "continuous", xlab = "Time", moderator = "went_to_clinton", moderator.value = "Trump", linecol = "red", ylim = c(0, 0.10), printlegend = FALSE, xaxt = "n", cex.lab=1.5, cex.axis=1.8, cex.main=1.5, cex.sub=1.5, lwd = 4)
#plot(prep, covariate = "daynum", model = prevFit, topics = val, method = "continuous", xlab = "Time", moderator = "went_to_clinton", moderator.value = "Clinton", linecol = "blue", add = TRUE, #printlegend = FALSE, cex.lab=1.5, cex.axis=1.8, cex.main=1.5, cex.sub=1.5, lwd = 4)
#legend("topright", c("Went to Trump", "Went to Clinton"), lwd = 4, col = c("red", "blue"), cex = 1.8, bty = 'n')
#axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)), labels = monthnames, cex.lab=1.5, cex.axis=1.8, cex.main=1.5, cex.sub=1.5)
#}


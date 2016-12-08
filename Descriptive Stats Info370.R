df = read.csv('Clean Filtered Data.csv')

library(nortest)
library(coin)
##install.packages("res")
library(reshape2)

cor(df$overall,df$helpfulnessMeasure)
cor(df$charCount,df$helpfulnessMeasure)
cor(df$wordCount,df$helpfulnessMeasure)
cor(df$averageWordLength,df$helpfulnessMeasure)
cor(df$capitalRatio,df$helpfulnessMeasure)
cor(df$periodRatio,df$helpfulnessMeasure)
cor(df$commaRatio,df$helpfulnessMeasure)
cor(df$exclamationRatio,df$helpfulnessMeasure)
cor(df$sentiment,df$helpfulnessMeasure)
cor(df$subjectivity,df$helpfulnessMeasure)
cor(df$isAnonymous,df$helpfulnessMeasure)

mean(df$helpfulnessMeasure)
median(df$helpfulnessMeasure)
sd(df$helpfulnessMeasure)

mean(df$overall)
median(df$overall)
sd(df$overall)

mean(df$charCount)
median(df$charCount)
sd(df$charCount)

mean(df$wordCount)
median(df$wordCount)
sd(df$wordCount)

mean(df$averageWordLength)
median(df$averageWordLength)
sd(df$averageWordLength)

mean(df$capitalRatio)
median(df$capitalRatio)
sd(df$capitalRatio)

mean(df$periodRatio)
median(df$periodRatio)
sd(df$periodRatio)

mean(df$commaRatio)
median(df$commaRatio)
sd(df$commaRatio)

mean(df$exclamationRatio)
median(df$exclamationRatio)
sd(df$exclamationRatio)

mean(df$sentiment)
median(df$sentiment)
sd(df$sentiment)

mean(df$subjectivity)
median(df$subjectivity)
sd(df$subjectivity)

mean(df$isAnonymous)
median(df$isAnonymous)
sd(df$isAnonymous)






# Pretty Useless Tests
ad.test(df$overall)
ad.test(df$charCount)
ad.test(df$wordCount)
ad.test(df$averageWordLength)
ad.test(df$capitalRatio)
ad.test(df$periodRatio)
ad.test(df$commaRatio)
ad.test(df$exclamationRatio)
ad.test(df$sentiment)
ad.test(df$subjectivity)
ad.test(df$isAnonymous)


# DO NOT RUN THESE! They take forever to run (over a hour each)
wilcox_test(overall ~ helpfulMeasure, data=df)
wilcox_test(charCount ~ helpfulMeasure, data=df)
wilcox_test(wordCount ~ helpfulMeasure, data=df)
wilcox_test(averageWordLength ~ helpfulMeasure, data=df)
wilcox_test(capitalRatio ~ helpfulMeasure, data=df)
wilcox_test(periodRatio ~ helpfulMeasure, data=df)
wilcox_test(commaRatio ~ helpfulMeasure, data=df)
wilcox_test(exclamationRatio ~ helpfulMeasure, data=df)
wilcox_test(sentiment ~ helpfulMeasure, data=df)
wilcox_test(subjectivity ~ helpfulMeasure, data=df)

# GGPlot2



# BoxPlot
boxplot(helpfulnessMeasure ~ sentiment, data=df, xlab="Helpfulness Measure", ylab="Sentiment Value")


# Histograms
summary(df$wordCount)
hist(df$wordCount, breaks = 270, xlim = range(c(0,900)), xlab = "Word Count", main = "Word Count Frequencies")
plot(density(df$wordCount), xlim = range(c(0,900)), main = "Word Count Densities")
Mode(df$wordCount)
summary(df$overall)
hist(df$overall, breaks = 100, xlim = range(c(1,5)), xlab = "Overall Frequencies", main = "Overall Frequencies")


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

summary(df$charCount)
hist(df$charCount, breaks = 1900, xlim = range(c(0,1900)), xlab = "Char Count", main = "Char Count Frequencies")
Mode(df$charCount)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


hist(df$sentiment, breaks = 200, xlim = range(c(-1,1)), xlab = "Sentiment Value Count", main = "Sentiment Value Frequencies")
Mode(df$sentiment)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


## Splitting DF into buckets Sentiment
summary(df$sentiment)

set1a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= -1 & df$sentiment < -.8), B=rep(1, length(subset(df$sentiment, df$sentiment >= -1 & df$sentiment < -.8)))))
set2a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= -.8 & df$sentiment < -.6), B=rep(2, length(subset(df$sentiment, df$sentiment >= -.8 & df$sentiment < -.6)))))
set3a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= -.6 & df$sentiment < -.4), B=rep(3, length(subset(df$sentiment, df$sentiment >= -.6 & df$sentiment < -.4)))))
set4a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= -.4 & df$sentiment < -.2), B=rep(4, length(subset(df$sentiment, df$sentiment >= -.4 & df$sentiment < -.2)))))
set5a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= -.2 & df$sentiment < 0), B=rep(5, length(subset(df$sentiment, df$sentiment >= -.2 & df$sentiment < 0)))))
set6a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= 0 & df$sentiment < .2), B=rep(6, length(subset(df$sentiment, df$sentiment >= 0 & df$sentiment < .2)))))
set7a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= .2 & df$sentiment < .4), B=rep(7, length(subset(df$sentiment, df$sentiment >= .2 & df$sentiment < .4)))))
set8a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= .4 & df$sentiment < .6), B=rep(8, length(subset(df$sentiment, df$sentiment >= .4 & df$sentiment < .6)))))
set9a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= .6 & df$sentiment < .8), B=rep(9, length(subset(df$sentiment, df$sentiment >= .6 & df$sentiment < .8)))))
set10a = do.call(rbind, Map(data.frame, A=subset(df$sentiment, df$sentiment >= .8 & df$sentiment < 1), B=rep(10, length(subset(df$sentiment, df$sentiment >= .8 & df$sentiment < 1)))))


boxplot(set1, at=22)
boxplot(set2, at=2, add=TRUE)

set1 = FALSE



summary(df$subjectivity)
hist(df$subjectivity, breaks = 100, xlim = range(c(0,1)), xlab = "Subjectivity Value Count", main = "Subjectivity Value Frequencies")
Mode(df$subjectivity))
axis(side = 1, labels=seq(0, 122, 500, 1000, 1500))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
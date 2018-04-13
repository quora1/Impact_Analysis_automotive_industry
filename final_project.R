rm(list=ls())
data(mtcars)
mtcars
?mtcars
##Transmission: 0 = automatic, 1 = manual
head(mtcars)
dim(mtcars)

summary(mtcars)



mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Automatic", "Manual")
##Exploratory analysis
library("ggplot2")
violin = ggplot(data = mtcars, aes(y = mpg, x = am))
violin = violin + geom_violin(alpha = 1)
violin = violin + xlab("Transmission Type") + ylab("Meter per Gallon (MPG)")
violin = violin + scale_fill_discrete(name = "Transmission Types", labels=c("Automatic", "Manual"))
violin






##“Is an automatic or manual transmission better for MPG”
aggregate(mpg ~ am, data=mtcars, mean)
## are these values significantly different?

##correlating mpg with remaining variables to pick up candidates for regression
corr <- cor(as.matrix(mtcars[,1]), as.matrix(mtcars[,-1]))
correlations <- as.data.frame(as.table(corr))
names(correlations)[1]<-paste("MPG")
names(correlations)[2]<-paste("Control Variables:")
names(correlations)[3]<-paste("Correlation")
correlations

##regressions:
fit1<-lm(formula = mpg ~ factor(am) -1, data = mtcars)
fit2<-lm(formula = mpg ~ factor(am) + wt -1, data = mtcars)
fit3<-lm(formula = mpg ~ factor(am) + wt + cyl -1, data = mtcars)
fit4<-lm(mpg ~ factor(am) + wt + cyl + disp -1, data = mtcars)
fit5<-lm(mpg ~ factor(am) + wt + cyl + disp + hp -1, data = mtcars)
fit6<-lm(mpg ~ factor(am) + wt + cyl + disp + hp + drat -1, data = mtcars)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

resid<-resid(fit5)
predict<-predict(fit5, data=mtcars)
plot(predict, resid)

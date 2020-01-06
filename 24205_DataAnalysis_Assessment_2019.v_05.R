
#----------------------------------------------------------
# Kultigin Bozdemir  24205
# Semester Assignment 2019
#----------------------------------------------------------
rm(list=ls())
getwd()
setwd("/Users/kultiginbozdemir/R")
getwd()

#----------------------------------------------------------
# Reset graphic device
# As long as there is any dev open (exept "null device")
# close the active one!
# Caution: closes all open plots!!!!
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}
if (!require(psych)) install.packages("psych")
library(psych)
if (!require(splines)) install.packages('splines')
library(splines)
if (!require(leaps)) install.packages('leaps')
library(leaps)
if (!require(olsrr)) install.packages('olsrr')
library(olsrr)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(gam)) install.packages('gam')
library(gam)
if (!require(ggm)) install.packages('ggm')
library(ggm)
if (!require(lsr)) install.packages('lsr')
library(lsr)
if (!require(ISLR)) install.packages('ISLR')
library(ISLR)
attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]
str(assessment_dataframe)

#basic numerical summaries
summary(assessment_dataframe)

table(assessment_dataframe$education)


# draw histograms/barplots to explore the data.
parBackup <- par()
par(mfrow=c(1,2))
AgeHist <- hist(assessment_dataframe$age, breaks=10, main = "age")
# The distribution of the age is close to normal distribution.
WageHist <- hist(assessment_dataframe$wage, breaks=100, main = "wage")
# Wage distribution looks like normal as well, if we exclude the outliers of >250.


par(mfrow=c(1,2))
barplot(table(assessment_dataframe$education), main="education")
barplot(table(assessment_dataframe$year), main = "year")

par(mfrow=c(1,1))
split.screen(c(1, 2))
screen(1)
#Let's see the age.
plot(age, wage, data=assessment_dataframe)
regWageAge<-lm(assessment_dataframe$wage~assessment_dataframe$age,data=assessment_dataframe)
abline(regWageAge, col="red")
#Although we created a linear function, we do not intent to do inferential statistics.
#Actually, the scatter plot tells us that a linear fit would nit be enough.
summary(regWageAge) #It tells us that correlation exists.

screen(2)
plot(year, wage, data=assessment_dataframe)
regWageYear<-lm(assessment_dataframe$wage~assessment_dataframe$year,data=assessment_dataframe)
abline(regWageYear, col="red")
summary(regWageYear) #F test is higher than 1 and p-value is significantly low.
close.screen(all = TRUE)
#The graph shows that there is slightly increase in wages untill 2007, where we have a drop.

#Let's continue to discover the data.
par(mfrow=c(1,2))
boxplot(wage ~ year,data=assessment_dataframe, main="Wage by year",xlab="year", ylab="Wage",horizontal=FALSE)
# From the plot, it seems that there is a constant increase untill 2007 where we see a drop, later on it continues to increase.
boxplot(wage ~ education,data=assessment_dataframe, main="Wage by education",xlab="education", ylab="Wage",horizontal=FALSE)
#Higher education pays off as you might expect.
# All education classes except 1st one (worst one) have also those outliers which discussed above.

# Three dimensional analysis
#ggplot2 package allows to see plots in three dimension with the help of different colors or shapes for the third variable.
par(mfrow=c(1,1))
#Now we will have a look at the wage data in three dimensions with the help og ggplot library.
ggplot(assessment_dataframe, aes(age, wage, colour = education)) +
  geom_point()
#Better education better wages..By the around age of 30, we see advanced degrees.
#Secondly, anvanced degree dominates the highly paid outlier section (>250).

par(mfrow=c(1,1))
split.screen(c(1, 2))
screen(1)
#Interaction effects between categorial variables
#interaction.plot function allows to plot categorial variables to the interaction between them based on respond variable.
interaction.plot(assessment_dataframe$jobclass, assessment_dataframe$education, assessment_dataframe$wage)
screen(2)
interaction.plot(assessment_dataframe$education, assessment_dataframe$jobclass, assessment_dataframe$wage, main="Interaction of education~jobclass")
#From the graphs, we can see the interaction between advanced degree and information job-class at higher wages.
close.screen(all = TRUE)

interaction.plot(assessment_dataframe$education, assessment_dataframe$age, assessment_dataframe$wage)
interaction.plot(assessment_dataframe$age, assessment_dataframe$education, assessment_dataframe$wage)
# It is not easy to see sth from the graph.


#Correlation
#Let's see the correlation between wage and quantitative variables(age and year).
#Linear correrlation/pearson
cor(assessment_dataframe[,c("wage","age","year")], method = "pearson" )
#The most srongest correlation is between age and wage, 0.19.
# it is worth to point out the correlation between two independent variables, age and year,even though it is very small, 0.038.


#We can create dummy variables for qualitative variables to analyze them in terms of correlation.
assessment_dataframeC <- cbind(assessment_dataframe, education_Code = as.numeric(assessment_dataframe[,"education"]))

# call the cor.test function accordingly
cor.test(assessment_dataframeCC[,"age"],
         assessment_dataframeCC[,"wage"],
         method = "pearson" )

cor.test(assessment_dataframeCC[,"year"],
         assessment_dataframeCC[,"wage"],
         method = "pearson" )

cor.test(assessment_dataframeCC[,"education_Code"],
         assessment_dataframeCC[,"wage"],
         method = "pearson" )

cor.test(assessment_dataframeCC[,"education_Code"],
         assessment_dataframeCC[,"wage"],
         method = "spearman" )


#Since, the education and maritl are qualitative and not normal distribution, Spearman rho should give us a more accurate correlation result.
#Remember, their distributions are not also normal, as we have seen above.
# p-value tells us the the rejection of the null hypothesis, because it is very small. (<5%)

#Since we have now a dummy variable named education_Code, we can use cor function consequently.
cor(assessment_dataframeCC[,c("wage","age","year", "education_Code")], method = "pearson" )
# We can interpret that education has higher effect on wage than others.

#partial correlation
#Below, we compare correlation between wage and age with Partial correlation caused by year, education and maritl.
# Remember, we had already created dummy variables to replace qualitative ones; education and maritl.
cor(assessment_dataframe[,c("wage","age")], method = "pearson" )
pcor(c("wage","age", "year", "education_Code"), var(assessment_dataframeCC))
# Partial correlation is less than norman correlation as expected, between wage and age.


#Eta SQUARED --> (Total Variation - Group Variation)/Total Variation
assessment_dataframe_ANOVA <- aov(assessment_dataframe$wage ~ assessment_dataframe$age)
anova(assessment_dataframe_ANOVA)
# anova function shows us that f-test is well larger than 1, which justifies the association between mentioned variables.
# Besides that p-value is very small, which shows us the confidence interval.
etaSquared(assessment_dataframe_ANOVA)
#The results show that there is a small effect size on variance  between age and wage.

assessment_dataframe_ANOVA <- aov(assessment_dataframe$year ~ assessment_dataframe$wage)
anova(assessment_dataframe_ANOVA)
etaSquared(assessment_dataframe_ANOVA)
#etasquared function calculates TypeII error by default.
#the effect size on variance between year and wage is very small 0.004.  P-value of anova is less than 5%, which jecets the null hyphothesis.

#We can see all scaterplots, histograms and correlations together

pairs.panels(assessment_dataframe[c("age", "year", "education", "wage")], stars = T,  ci=T)
# Above diagonol, we see pearson correlations.
# Below the diagonal, scatterplots. REd lines indicate loess (local regression) function with confidence intervals.
# Dot is the mean.
# Eclipse shows how the correlation is strong. More stretched means more correlation. More oval means less correlation.
# On the diagonal, histograms stay.

#The effects of the predictors

#Reference:https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
#We create a primitive model following the previous analysis to decide on the predictors.
reg= lm(wage~age + year  + education ,data=assessment_dataframe)

k<-ols_step_all_possible(reg)
plot(k)
k
data.frame(k)
#From All Possible Regression analysis, we have seen that until 2nd predictor we see an improvement.
#the model with 2 predictors (4th model):  wage~ age  + education.
#nevertheless, we can continue to use all predictors, since we have no problem about complexity of the problem interms of computation resources.


#As a second method, we are using ISLR and leaps library to find the best predictors for our model. James et.al, p.245
regfit.full=regsubsets(wage~age+year+education, data=assessment_dataframe, nvmax = 50)
summary(regfit.full)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq

par(mfrow=c(1,1))
plot(reg.summary$rss, xlab = "Number Of variables", ylab = "RSS")
plot(reg.summary$adjr2, xlab="Number of variables", ylab = "Adjusted RSq")
# Importance of variables are depicted with the sign of"*".
# Results justify the results of the first method.
# Do not be confused, dummy variables are created for every category of education.


#Regression models

#Linear and Polynomial regressions
fit1<-lm(wage~age, data=assessment_dataframe)
summary(fit1)
#plot(fit1)

fit2<-lm(wage~poly(age,2), data=assessment_dataframe)
summary(fit2)
#plot(fit2)

fit3<-lm(wage~poly(age,3), data=assessment_dataframe)
summary(fit3)
#plot(fit3)

fit4<-lm(wage~poly(age,4), data=assessment_dataframe)
summary(fit4)
#plot(fit4)

fit5<-lm(wage~poly(age,5), data=assessment_dataframe)
summary(fit5)
#plot(fit5)

anova(fit1,fit2,fit3,fit4,fit5)

# We have used anova () function to compare the models.
# We want to test null hypthesis that model 1 is sufficient to explain the data against Model 2.
# P-value of which compares Model 1 to Model 2 is 2.2e-16. That says linear fit is insufficient.
# With the same token, p-value which compares model2 to model 3 is also low; 0.001679. That says cubic model performs better than quadratic one.
# P-value which compares cubic model to 4-polynomial is around 5%.
# P-value of model 4-model 5 is 37%.
# This anova tables says either model 3 or model-4 is better than other lower or higher polynomials.


# We are now plotting fit3 model, polynomial degree 3

ggplot(assessment_dataframe, aes(age, wage)) +
  geom_point() +
  geom_smooth(method = glm, formula = y~ poly(x,3))
#Grey zone at the both sides show the confidence interval. As expected, it widens at the right tail obviously.

ageboundary =range(age)
age.grid=seq (from=ageboundary[1], to=ageboundary[2])
# those are for preperation for the plotting

#Comparasion of polynomial regressions

plot(age ,wage , data=assessment_dataframe  ,xlim=ageboundary ,cex =.5, col =" darkgrey ")
title ("Polynomial Regressions ")
preds=predict (fit1 ,newdata =list(age=age.grid),se=TRUE)
lines(age.grid, preds$fit, lwd =2, col ="blue")
preds=predict (fit2 ,newdata =list(age=age.grid),se=TRUE)
lines(age.grid, preds$fit, lwd =2, col ="red")
preds=predict (fit3 ,newdata =list(age=age.grid),se=TRUE)
lines(age.grid, preds$fit, lwd =2, col ="purple")
preds=predict (fit4 ,newdata =list(age=age.grid),se=TRUE)
lines(age.grid, preds$fit, lwd =2, col ="green")
preds=predict (fit5 ,newdata =list(age=age.grid),se=TRUE)
lines(age.grid, preds$fit, lwd =2, col ="orange")
legend ("topright",legend =c("Degree 1" ,"Degree 2","Degree 3" ,"Degree 4", "Degree 5" ), col=c("red "," blue","purple", "green", "orange"),
        lty =1, lwd =2, cex =.6)

#Local regression, the Book: James et al.,"An introduction to stastical learning", p.280
plot(age ,wage , data=assessment_dataframe  ,xlim=ageboundary ,cex =.5, col =" darkgrey ")
title (" Local Regression ")
fit1=loess(wage ~ lage ,span =.1, data=assessment_dataframe)
lines(age.grid,predict(fit1 ,data.frame(age=age.grid)), col ="red",lwd =2)

fit2=loess(wage ~ age ,span =.6, data=assessment_dataframe)
lines(age.grid,predict(fit2 ,data.frame(age=age.grid)), col ="blue",lwd =2)

legend("topright",legend =c("Span =0.1" ," Span =0.6"), col=c("red","blue"),lty =1,
       lwd =2, cex =.8)

# loess () function provides the local regression. span parameter defines the range of neighborhood.
#Lower span factors give a wild fluctuating function, whereas higher ones will give smoother functions.
#Having no reason to have a wild function in our case, we would prefer to be around 0.5.

ggplot(assessment_dataframe, aes(age, wage)) +
  geom_point() +
  geom_smooth(span = 0.5)


# We used ggplot2 library to show the local regression line and its confidence interval.
#Grey zones on the both sides represent the confidence interval.


#Splines
#With the help of splines library, we can use smoothing method for the fit.
#bs() function generates basis functions (by default cubic) at the segments which are divided by the defined knots.
#below, we defined the age knots at 30,65.
#That means we have 6 degrees of freedom in total.  3 coefficient come from 3 different segments which are divided by two knots(30 and 65).
# Since the default function cubic, every polynomial degree has also a different coefficient.
# Now at hand we have  piecewise polynomial regressions. This approach provides us a continues function.
#See James et al., "An Introduction to Statistical Learning", page 270-280 for further details.

# To do that we used ggplot library.
ggplot(assessment_dataframe, aes(age, wage)) +
  geom_point() +
  geom_smooth(method = glm, formula = y~ bs(x, knots = c(30,65)))

#As we discussed above, we define the degree of freedom by ourself.
#Not only knots, but total number of df can be defined as well.

#splines functions in GAM library
plot(age ,wage ,xlim=ageboundary ,cex =.5, col =" darkgrey ")
title (" Smoothing Spline ")
fit1=smooth.spline (age ,wage ,df =30)
fit2=smooth.spline (age ,wage ,cv=TRUE)
fit2
lines(fit1,col ="red ",lwd =2)
lines(fit2,col =" blue",lwd=2)
legend ("topright",legend =c("30 DF " ,"6.8 DF"), col=c("red "," blue "),
        lty =1, lwd =2, cex =.6)
# As seen on the plot, red line shows the model which is defined by 30 df;
# blue line shows the model defined by cross validation. Eventually, it has 6.8 degree of freedoms.




# Comparasion of the models

# Spline regression models have an upperhand on polynomial functions.
# They provide a more stable function, since Spline models mainly use knots to have the df.
# On the other hand, polynomial models use the df resulted from polynomials. To do that, they have use high polynomial degrees.
#Besides that, piecewise approach allows to analyze the data easier.
# Finally, polynomial functions might be wildy on the tails.
# see James et al., p.273.


plot(age ,wage , data=assessment_dataframe,  xlim=ageboundary ,cex =.5, col =" darkgrey ")
title (" Comparasion of wage~age models ")

fit0=lm(wage~age, data=assessment_dataframe)

fit1=lm(wage~poly(age,3), data = assessment_dataframe)
lines(age.grid,predict(fit1 ,data.frame(age=age.grid)), col ="green",lwd =2)

fit2=lm(wage~bs(age,knots =c(25, 42, 65)), data = assessment_dataframe)
preds=predict (fit2 ,newdata =list(age =age.grid),se=T)
lines(age.grid ,preds$fit ,col= "blue",lwd =2)

fit3=loess(wage ~ age ,span =.5, data=assessment_dataframe)
lines(age.grid,predict(fit3 ,data.frame(age=age.grid)), col ="red",lwd =2)

legend ("topright",legend =c( "Polymonial Degree 3","Spline", "Local Regression"), col=c("green",  "blue ", "red"),
        lty =1, lwd =2, cex =.5)

summary(fit0)
summary(fit1)
summary(fit2)
summary(fit3)

anova(fit0, fit1, fit2)
# anova says, linear model is not good. But not much about the rest.
# Since local regression loess is non parametric, anova can not analyze it.
#Then, let's plot the models against their rivals.

plot(fit0$residuals, fit1$residuals)
cor.test(fit0$residuals, fit1$residuals)

plot(fit1$residuals, fit2$residuals)
cor.test(fit1$residuals, fit2$residuals)

plot(fit1$residuals, fit3$residuals)
cor.test(fit1$residuals, fit3$residuals)

#There is no any significant difference between non linear models.

# ggplot can provide 3-dimensional analysis.
# https://ggplot2.tidyverse.org/reference/geom_smooth.html

ggplot(assessment_dataframe, aes(age, wage, color=education)) +
  geom_point() +
  geom_smooth(span=0.5)
# This plot shows the wage~age local regression of every education category.
# Advanced Degrees have more wage fluctuations through the ages.

# wage~age analysis with polynomial degree 3.
ggplot(assessment_dataframe, aes(age, wage,color=education )) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ poly(x,3) , se = T)
# Not suprisingly, results are similar.
# it is worth to point out again that, confidence intervals are extremely enlarging at the tails. No suprise.
ggplot(assessment_dataframe, aes(age, wage, color=education)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)


# wage~year analysis
fit1=lm(wage~year, data = assessment_dataframe)
fit2=lm(wage~poly(year,2), data = assessment_dataframe)
fit3=lm(wage~poly(year,3), data = assessment_dataframe)
fit4=lm(wage~poly(year,4), data = assessment_dataframe)
fit5=lm(wage~poly(year,5), data = assessment_dataframe)
anova(fit1, fit2, fit3, fit4, fit5)
# Anova says, linear model performs well, no need for polynomial degrees in wage~year analysis.


# Comparasion of wage~year models
yearboundary=range(year)
year.grid=seq (from=yearboundary[1], to=yearboundary[2])

plot(year, wage , data=assessment_dataframe, cex =.5, col =" darkgrey ")
title (" Comparasion of the wage~year models ")

#Linear model
fit1=lm(wage~year, data = assessment_dataframe)
preds=predict (fit1 ,newdata =list(year=year.grid),se=TRUE)
lines(year.grid, preds$fit, lwd =2, col ="green")


#Spline with knots
fit2=lm(wage~bs(year ,knots =c(2006,2008) ),data=assessment_dataframe)
preds=predict (fit2 ,newdata =list(year =year.grid),se=T)
lines(year.grid, preds$fit, lwd =2, col ="blue")

# Local regression

fit3=loess(wage ~ year ,span =.5, data=assessment_dataframe)
lines(year.grid,predict(fit3, data.frame(year=year.grid)), col ="orange",lwd =2)
preds=predict (fit3, newdata = assessment_dataframe$year )
summary(preds)


legend ("topright",legend =c( "Linear","Spline", "Local Regression"), col=c("green",  "blue ", "orange"),
        lty =1, lwd =2, cex =.5)



summary(fit1)
summary(fit2)
summary(fit3)

anova(fit1,fit2)
#Since local regression is non-parametric method, it is not possible to analyze it with anova.

#We continue to analyze the differences between models by plotting them and with cor.test () function.
# We are looking especially at Local Rgression model, because anova does not work on it.
plot(fit1$residuals, fit2$residuals)
cor.test(fit1$residuals, fit2$residuals)

plot(fit1$residuals, fit3$residuals)
cor.test(fit1$residuals, fit3$residuals)

plot(fit2$residuals, fit3$residuals)
cor.test(fit2$residuals, fit3$residuals)

#Differences between models are almost invisible.
#However, seeing the poor performance of Spline bs () function, we introduced a new fit with Smoothing Spline s() function.

fit4=lm(wage~s(year,3), data = assessment_dataframe)
summary(fit4)

anova(fit1, fit2, fit4)

#Again, linear Smoothing Spline s() function does not perform better.



par(mfrow = c(1,1))

# It is time to finalize our model
#Before that, we look at interactions again.
fit0= lm(wage~bs(age,knots = c(25,40,65)) * bs(year, knots = c(2006, 2008)) * education, data=assessment_dataframe)
summary(fit0)
# not suprisingly t-values of advanced degrees are larger than +/- 2.
anova(fit0)
# As seen on the anova table from the values of F-test and Pr coloms,
# we have following significant interactions:
# education:age

fit0= lm(wage~bs(age,knots = c(25,40,65)) + bs(year, knots = c(2006, 2008)) + education, data=assessment_dataframe)
summary(fit0)
# From the summary, we see that we have highly significant association between wage and defined values, which shown by stars "*".
# adjusted r-squared is 0.28, which says that the fit explains 28% of the data.
fit0= lm(wage~bs(age,knots = c(25,40,65)) + bs(year, knots = c(2006, 2008))  , data=assessment_dataframe)
summary(fit0)
# To check again the values of the predictors, we drop education predictor. That decreases the adjusted r-square dramaticaly.

#An easier way to see the effect of predictors is to add them into model and check the p-value.

fit0= lm(wage~bs(age,knots = c(25,40,65)) + bs(year, knots = c(2006, 2008)) + education + jobclass+health+health_ins , data=assessment_dataframe)
summary(fit0)
#As seen , they are both statistically significant.
# Overall adjusted r-squared is increased as well.
#Nevertheless, we have predictor constraints in our assignment.

fit0= lm(wage~bs(age,knots = c(25,40,65)) + bs(year, knots = c(2006, 2008)) + education, data=assessment_dataframe)
p<-predict(fit0, type="response")
plot(p, assessment_dataframe$wage)
plot(fit0$fitted.values, assessment_dataframe$wage)
anova(fit0)
#anova table says that there is a significant correlation in our model.
# All F values are higher than 1.
# all Pr values are less than 5%.

plot(fit0$fitted.values,fit0$residuals)
plot(fit0)

# from the plot, we can conclude that the model does not indicate a significant heteroscedasticity.

# Remember, age and education are the most powerfull predictors, as we have mentioned earlier.
# Sign of "*" instead of "+" gives interaction effects among the independent variables.
# Although we have not seen any important difference between non linear wage~age models,
# we have choosen Spline model for age, since they are more stable, as discussed earlier.
# We defined the knots manually, although there are some spline functions which can choose the best knots (number of knots and their locations).
# Because this approach allows the us to do furher analysis on the data for being aware of handlind data such as operation, constraints etc.
# Non linear wage~year models have not given much benefit in terms of the fittness of the models.
# Nevertheless it was clear on the boxlpots that the increase of the wages is not linear. There was a drop in 2007.
# Therefore we did not select the linear one, but again Spline model for the same reasons mentioned above.
# lm () function generates dummy variables for categorial variables such as education or maritl.




# At the beginning, we have noticed the highly paid outliers.
# We will analyze highly paid people (wage>250) with logistic regression.
highlypaid<-assessment_dataframe$wage>250
summary(highlypaid)
# we have 79 highly paid people.

#Prediction

assessment_dataframeH<- cbind(assessment_dataframe,highlypaid )
summary(assessment_dataframeH)

#  We followed the method of James at al., p.156-160
# glm () function includes also logistic regression.
# family=binomial tells R, it is logistic regression.
fit=glm(highlypaid~ bs(age , knots=c(25,40,65))+bs(year,knots=c(2006,2008))+education ,data=assessment_dataframeH ,family =binomial)
# The predict () function can be used to predict the probability that people get a high wage (>250) given the values of defined variables in the model above.
# type="response" tells R to output probabilities of the form P(Y=1/X).
preds=predict (fit , type = "response")
summary(preds)
boxplot(preds, main="Probability of the wage>250",  ylab="Probability")

#As seen in the summary probability of being highlypaid is around 2%. Given the method, it is very hard to predict the highlypaid people.






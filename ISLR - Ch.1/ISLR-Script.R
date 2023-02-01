#### Chapter 1 - Introduction
library(tidyverse)
library(ISLR2)
### Chapter 2 ###
#### Important concepts ####

## Bias - Variance Tradeoff
#   Increases in flexibility results in lower bias
#   but increased variance.
#   Increasing flexibility reduces training error rate
#   but does not necessarily reduce test error rate

## Bayes Classifier
## K-nearest Neighboor Classifier

## R- Lab
#ggplot(Auto, aes(x = as.factor(Auto$cylinders), y = Auto$mpg,
                #fill=as.factor(Auto$cylinders)))+
                #geom_boxplot() + theme(legend.position="none")

#ggplot(Auto, aes(x = mpg))+geom_histogram(fill = "red", bins = 30)

## Excercises Chapter 2

# 1.
# a) A flexible method may take more advantage of the large
#    sample size nad be more accurate.
# b) A less flexible method may be less accurate due to
#    overfitting.
# c) A flexible model can more accurately depict non linear
#   relationships then an inflexible model.
# d) High variance usually results in more flexible models
#   being less accurate due to them fitting for the increased
#   noise.

# 2.
# a) n = 500, p = 3. Regression, Inference problem, interesting in 
#  relationship between predictors(profit, industry, number
#  of employees) and response(CEO salary). 
#
# b) n = 20, p = 13. Classification - prediction problem. Response
# is qualitative or binary, success vs failure. No mention of wanting
# to find relationship of predictors to the response, so prediction.
#
# c) n = 52 weeks, p = 3. Regression - prediction. % change
#  in USD/Euro is continuous response variable so it's regression
#  and there is no indication of wanting to know the relationship
# between predictors and response, just predict the change.

# 3.
# a) 
# b) bayes error curve is constant as the error is independent of the
#  model. The bias decreases with flexibility since more flexible
# models make less assumptions about the form of f. Variance 
# increases with flexibility due to overfit. The Testing error
# declines initially as low flexibility models have large bias from
# assumptions on the form of f, levels off in the middle and then
# increases again as high flexibility models begin to overfit.

# 4.
# a) Predicting the winner of a football game, predicting whether a
# candidate will win or lose an election, determining if there is a
# relationship between, determining which economic factors cause
# the stock market to go up or down.
# b) Predicting the price of food products, identifying if there is 
# a relationship between hours of sleep and test scores in students
# predicting the population of fish in a lake.
# c) Sorting fruits into clusters of the same fruit, grouping 
# similar clothing, sorting different blocks by shape.

# 5.
# The advantage of a more flexible model is that they
# can produce more accurate prediction on non linear data.
# The also produce less bias. If a non linear relationship is
# expected a flexible model is preferred, if the dataset is very 
# large a more flexible model is usually better.

# 6.
# A parametric model makes assumptions about the form of f
# parametric models are usually less flexible. Non parametric models
# can fit a wider range of possible functions but also produce
# larger variance and require more observations.

# 7.
# a) Obs 1 dist = 3. Obs 2, dist = 2. Obs 3, dist = sqrt(10)
# Obs 4 dist = sqrt(5), Obs 5, dist = sqrt(2), obs 6 dist = sqrt(3)
# test <- c(sqrt(10), 3, sqrt(5), 2,sqrt(3),sqrt(2)) 

# b) When k = 1, the nearest neighbor is Obs 5. which is green. It is
# the only observation so Green is the highest probability.
# c) When K=3, Obs 6,5,Red since 2/3 are Green and 1/3 are Green.

# d) K-nearest neighbors reduces in flexibility for large K-values
# therefore a non linear we would expect the best K value to be small

# 8.
# a) View(College)
# b)
# c) summary(College)
# ii) pairs(College[,1:10])
# iii) ggplot(College, aes(Fill = as.factor(Private),
# y = Outstate))+geom_boxplot()
# iv) There are 78 Elite universities.
# ggplot(College, aes(fill = Elite, y = Outstate))+geom_boxplot()
# v) ggplot(College, aes(x = Apps))+geom_histogram()
# ggplot(College, aes(x = Top25perc))+geom_histogram()
# vi) 
College %>% 
  pivot_longer(c(Accept,Apps), names_to = "Apps_Accept",
               values_to = "Count") %>%
  select(Apps_Accept, Private, Count) %>%
  ggplot(aes(x = as.factor(Private), y = Count, fill = Apps_Accept))+
  geom_bar(stat = "identity", position = "dodge")
# We see from this graph that Public universities receive far less
# applications then non private, and that of the applications
# received public universities accept fewer.
College %>%
  ggplot(aes(x = Expend, y = Outstate))+geom_point()
#institutional expenditure and out of state tuition cost seem to be
# linearly related
# 9.
# a) Quantitative predictors are mpg, horsepower, acceleration,
# weight, displacement, cylinders, and year.
# Qualitative predictors are origin
# b)
L <- length(names(Auto))
names <- vector("list",L)
for (i in 1:L){
  names[i] <- round(range(Auto[i]), 3)
}
# gives the ranges of each variable from Auto
# c)
s.d <- apply(Auto, 2, sd)
mn <- sapply(Auto, mean)
# d)
Auto.1 <- Auto[-(10:85),]
summary(Auto1)
# e)
Auto %>%
  ggplot(aes(fill = as.factor(cylinders),x = as.factor(cylinders),
             y = mpg))+geom_boxplot() +
  theme(legend.position = 'none')
# increasing cylinder count reduces the mean mpg
Auto %>%
  group_by(year) %>%
  summarize(mn = mean(mpg)) %>%
  ggplot(aes(x = year, y = mn))+geom_line()
# we see that mpg increases over time with the year a car is
# manufactured.
Auto %>%
  ggplot(aes(x = horsepower, y = mpg))+geom_point()
# there appears to be a non linear decreasing relationship
# between the horsepower of a car and its mpg rating.

# f) As horsepower increases mpg seems to decrease at a decreasing
# rate. The year a car was manufactured appears to have a linearly
# increasing relationship with mpg. And an increases in the number
# of cylinders a car has seems to decrease it's mpg.

# 10.
# a) 
dim(Boston)
# There are 506 rows and 13 columns. The rows represent observations
# and the columns represent variables.
# b)
pairs(Boston)
# Nitrogen oxide and the distance to boston employment centres have
# an inverse relationship, as distance to employment centres
# increases nox levels decrease.
# average rooms per dwelling and median value of homes have a
# positive roughly linear relation.

# c) Crime rate seems to have a weak inversely related to median 
# value of homes. There is a small positive relationship 
# with the percent of the population who is lower status.

# d)
Boston %>%
  ggplot(aes(x=crim))+geom_histogram()
# For most cities the crime rate is very low
Boston %>%
  ggplot(aes(x = medv, y = crim))+geom_point()
# non of these seem to be particularly good predictors of high
# crime rates.

# e) 
Boston %>%
  filter(chas == 1) %>%
  count()
# 35 bound the Charles river.

# f) 
median(Boston[["ptratio"]])
# 19.05

# g)
Boston %>%
  filter(medv == min(medv))
summary(Boston)
# these homes have significantly higher crime rates then the median
# at 38.35 and 67.9 against the global median 0.2. these homes are
# closer to employment centers and have a larger proportion
# of non-retail businesses.

# h)
Boston %>%
  filter(rm > 7) %>%
  count()
# 64
Boston %>%
  filter(rm > 8) %>%
  count()
# 13
Boston_rm_8 <- Boston %>%
  filter(rm > 8)
summary(Boston_rm_8)
# The median value of these homes is significantly higher then
# that of Boston generally. lstat is slightly higher as well.
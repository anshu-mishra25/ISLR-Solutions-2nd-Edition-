
#### 2. Statistical Learning ----------------------------------------------



### 2.4 Exercises  -------------------------------------------------------


# CONCEPTIONAL

## 1. --------------------------------------------------------------------

# (a)
# Better: flexible method incorporates the data better when the sample size is
# large enough

# (b)
# Worse: The data set leads to the 'Curse of Dimensionality', flexible model
# will tend to be more overfitting, meaning it will try to follow the error
# (noise) too closely.

# (c)
# Better: Flexible methods perform better on non-linear datasets as they have 
# more degrees of freedom to approximate a non-linear.

# (d)
# Worse: A flexible model would likely overfit, due to more closely fitting the 
# noise in the error terms than an inflexible method. In other words, the data 
# points will be far from f (ideal function to describe the data) if the 
# variance of the error terms is very high. This hints that that f is linear
# and so a simpler model would be better to be able to estimate f.


## 2. --------------------------------------------------------------------

# (a)
# Regression Problem: Since it is a quantitative problem
# Inference: We are interested in the factors which affect CEO salary
# n = 500
# p = 3 (Profit, number of employees, industry)

# (b)
# Classification Problem: Since it is binary (success or failure)
# Prediction: We are interested in the success or failure of the product
# n = 20
# p = 13 (Price charged, marketing budget, competition price + 10 other)

# (c)
# Regression Problem
# Prediction: We are interested in the % change in the USD/Euro exchange rate
# n = 52
# p = 3 (% change in the US market, % change in the British market, % change in
#        the German market)


## 3. --------------------------------------------------------------------

# (a)
# {SKETCH}

# (b)
# Squared Bias: Decreases with more flexibility (generally more flexible methods
#               results in less bias)
# Variance: Increases with more flexibility
# Training Error: Continues to reduce as flexibility grows, meaning the
#                 model is too close to the training data.
# Test Error: Decreases initially, and reaches the optimal point where it gets
#             flat, then it starts to increase again, meaning an overfitted data
# Bayes (irreducible) Error: Flat/Fixed, since it can't be reduced any more


## 4. --------------------------------------------------------------------

# (a) three real-life applications in which classification might be useful

# (b) three real-life applications in which regression might be useful 

# (c) three real-life applications in which cluster analysis might be useful


## 5. --------------------------------------------------------------------

## 6. --------------------------------------------------------------------

## 7. --------------------------------------------------------------------

# (a)
data.set <- data.frame(
  "X1" = c(0,2,0,0,-1,1),
  "X2" = c(3,0,1,1,0,1),
  "X3" = c(0,0,3,2,1,1),
  "Y" = c("Red","Red","Red","Green","Green","Red")
  ); data.set    # A given data set

Prediction.coordinates <- c(0,0,0)

euclidian.distance <- 
  function(X, pred.data){
    # 'X' here represents a vector of points
    distance = sqrt((X[1]-pred.data[1])^2 +
                      (X[2]-pred.data[2])^2 +
                      (X[3]-pred.data[3])^2) 
    return(distance)
  }

(X <- data.set[,-4])

distance <- numeric()
for(i in 1:nrow(X)){
  distance[i] = euclidian.distance(X[i,], Prediction.coordinates)
}
(distance <- as.matrix(distance))

# (b)

(Y <- data.set[,4])

KNN <- 
  function(K){
    Y[which.min(abs(distance[,1]-K))]
  }

cat("K-Nearest Neighbour for K = 3 predicts it to be",KNN(1))

# (c)

cat("K-Nearest Neighbour for K = 3 predicts it to be",KNN(3))


# APPLIED

## 8. --------------------------------------------------------------------

library(ISLR2)

# (a)
college <- College

# (b)

# rownames(college) <- college[, 1]   {Somehow already named so need}
View(college)

# (c) 
# i.
summary(college)

# ii.
pairs(college[,1:10], cex = 0.2)

# iii.
boxplot(Outstate ~ Private, data = college)

# iv.
college$Elite <- as.factor(ifelse(college$Top10perc > 50, "Yes", "No"))

summary(college$Elite)

# v.
par(mfrow = c(2,2))

# FOR COLLEGE APPLICATIONS RECEIVED
for(n in c(5,10,15,20)){
  hist(college$Apps, 
       xlab = "Number of Applications Received",
       main = "Histogram of College Application",
       breaks = n,
       xlim = c(0,20000))
}

# FOR COLLEGE APPLICATIONS ACCEPTED
for(n in c(5,10,15,20)){
  hist(college$Accept, 
       xlab = "Number of Applications Accepted",
       main = "Histogram of College Application",
       breaks = n,
       xlim = c(0,20000))
}

# vi.
# Making a linear model with all variables
Model1 <- lm(Accept~.,data=College)
summary(Model1)

# Using this model to determine all the significant parameters which comes out 
# to be:
# -> Private
# -> Apps
# -> Enroll
# -> Top10perc
# -> Top25perc
# -> P.Undergrad
# -> Outstate
# -> perc.alumni
# -> Expend
# Making another model with these parameters only:

Model2 <- lm(Accept ~ Private + Apps + Enroll + Top10perc + Top25perc +
               P.Undergrad + Outstate + perc.alumni + Expend,
             data=College)
summary(Model2)
# Adjusted R-squared dropped but only by 0.0004, that doesn't make Model1 any
# better than Model2 with so much less parameters.

AIC(Model1)
AIC(Model2)
# AIC dropped

# Another Model with even less parameters
Model3 <- update(Model2, .~. -Private - P.Undergrad - perc.alumni)
summary(Model3)

AIC(Model3)
# AIC increased by ~2 points

anova(Model2, Model3)

# Another Model
Model4 <- update(Model2, .~. -Private)
summary(Model4)

AIC(Model2); AIC(Model4)

anova(Model2,Model4)

# Overall Model 2 is better, but if I wanted to use as less co-variates as
# possible in order to explain most of the response variable, I can use Model 4
# as well.

## 9. --------------------------------------------------------------------

# (a)
Auto <- Auto
View(Auto)

# Quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration,
#               year
# Qualitative: origin, name

# (b)
apply(Auto[,1:7], 2, range)

# (c)
apply(Auto[,1:7], 2, mean)
apply(Auto[,1:7], 2, sd)

# (d)

Auto.reduced <- Auto[-c(10:85), ]

apply(Auto.reduced[,1:7], 2, range)
apply(Auto.reduced[,1:7], 2, mean)
apply(Auto.reduced[,1:7], 2, sd)


# (e)
pairs(Auto[,1:7], cex = 0.8, pch = 16)
cor(Auto[,1:7])

# The covariates with high positive correlation are (more than 0.7):
# cylinders vs displacement
# cylinders vs horsepower
# cylinders vs weight
# displacement vs horsepower
# displacement vs weight
# horsepower vs weight
# 
# The covariates with high negative correlation are (less than -0.7):
# mpg vs cylinders
# mpg vs displacement
# mpg vs horsepower
# mpg vs weight

# (f)
# Yes, all the other variables except acceleration (maybe even year)
# are highly correlated and can be used to predict mpg.

## 10. -------------------------------------------------------------------

# (a)
Boston <- Boston

cat("Number of rows =",nrow(Boston),"\n",
    "Number of columns =",ncol(Boston))

# Each 13 column suggests a variable affecting the price of the particular 
# suburb
# There are 506 suburbs each with 12 explanatory variables

# (b)
pairs(Boston, cex = 0.5)
cor(Boston)
# medv parameter has good positive correlation with rm and high negative 
# correlation with lstat
# indus parameter has high positive correlation with nox and tax, and high
# negative correlation with dis
# and many more...

# (c)
# crm (per capita crime rate by town) has the highest correlation with rad 
# (index of accesssibility to radial highways) which is positive.

# (d)
# High Crime Rates
High.Crime.Rate <- 
  Boston$crim[Boston$crim > mean(Boston$crim) + 2*sd(Boston$crim)]
length(High.Crime.Rate)
# There are 16 suburbs which have high crime rate
hist(High.Crime.Rate,
     xlab = "Crime Rate",
     main = "Histogram of Suburbs with High Crime Rates")
range(High.Crime.Rate)

# High Tax Rates
High.Tax.Rate <- 
  Boston$tax[Boston$tax > mean(Boston$tax) + 2*sd(Boston$tax)]

cat("There are",length(High.Tax.Rate), "suburbs which have high tax rate")

# High Pupil-teacher ratios
High.Pupil.teacher.ratios <- 
  Boston$ptratio[Boston$ptratio > mean(Boston$ptratio) + 2*sd(Boston$ptratio)]

cat("There are",length(High.Pupil.teacher.ratios),
    "suburbs which have high Pupil-teacher ratio")

# (e)

cat("There are",sum(Boston$chas),"suburbs that bound the Charles river.")

# (f)

cat("Median pupil-teacher ratio in town is",median(Boston$ptratio))

# (g)

which(Boston$medv == min(Boston$medv))
# 399th and 406th

# (h)

sum(Boston$rm > 7)

sum(Boston$rm > 8)

High.dwelling.Boston <- Boston[Boston$rm > 8, ]

summary(Boston)
summary(High.dwelling.Boston)

# More rm, lower crime

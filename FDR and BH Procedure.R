# False Discovery and Benjamini-Hochberg Procedure


### Setting seed number ensures that our code is reproducible 
### i.e. every time we run the code, it should now yield the exact same (“random”) dataset
set.seed(8933)

### Generating the matrix from Normal Distribution with mean of 0 and standard deviation of 1.
my_matrix<-matrix(rnorm(10000*1001,0,1), nrow = 10000)

###Checking number of rows and columns
nrow(my_matrix)
ncol(my_matrix)

### Converting our matrix into a dataframe
my_matrix_df <- as.data.frame(my_matrix)

## Regression of first column treated as Y on all the remaining columns treated as X's.
mymatrix_regress<- glm(V1 ~ .- 1, data = my_matrix_df)

### We will not be including an intercept term in the regression model for our case
## In general, including an intercept in multiple regression model ensures that model will be unbiased. 
## In other words, mean of residual terms will be zero(0).
## Unless, there is a particular theoritical reason known to us that the regression model is forced to go through the origin, or the intercept term is redundant  for the given variables we should never leave the intercept term out of the model, to make sure the coefficients of our model are unbiased.

### However, in our case since we know that dataset is randomly generated from Normal Distribution with mean of 0 and standard deviation of 1, we will force our regression through the origin. 

## Save the summary of the regression in a variable
summary_mm_regress<-summary(mymatrix_regress)

## Save the coefficients of the summary in a separate matrix to pull out the p values
regress_coefficients <- summary_mm_regress[["coefficients"]]

## Pull the p-values from the coefficients matrix
regress_pvals<-regress_coefficients[,4]

plot(hist(regress_pvals), main="Histogram of p-values",
     ylim = c(0,150))
## Histogram of p-values should always be uniformly distributed given the null hypothesis is true, and as we can see from the histogram it looks somewhat uniformly distributed.

## Let's confirm our finding using a Smirnov test.
ks.test(regress_pvals,"punif") ## Null hypothesis is that output dist. is uniform

## As we can see from the result of the formal-test, we fail to reject the null hypothesis and confirm that the distribution of p-values is actually uniform


sum(regress_pvals<0.01)
## Apparently, there were 9 significant variables at 0.01 significance level, out of all 1000 variables.
## The function below will apply the BH procedure 
## it takes vector of p-values and q - the accepted False discovery rate as an argument,  and returns the number of significant variables. 

fdr <- function(pvals, q, plotit=FALSE){
  pvals <- pvals[!is.na(pvals)]
  N <- length(pvals)
  
  k <- rank(pvals, ties.method="min")
  
  
  alpha <- length(pvals[ pvals <= (q*k/N) ])
  
  if(plotit){
    sig <- factor(pvals <= alpha)
    o <- order(pvals)
    plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]], pch=20, 
         ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
    lines(1:N, q*(1:N) / N)
  }
  
  return(alpha)
}

### Call the function.
true_disc<-fdr(regress_pvals,0.1,plotit=FALSE)
true_disc

## As a result of BH procedure we found out that there were no true discoveries at all, 
## Which is aligned with what I originally thought off. 
## All the 9 variables that we found to be significant from the results of 
## original regression were false discoveries. 
## Again, this finding totally makes sense since the variables used for this
## regression model were all random.


### Explore the “autos.csv” data. Include any metrics and / or plots you find interesting.
autos<- read.csv("autos.csv")

### Convert all the relevant variables to factors
autos$make<-as.factor(autos$make)
autos$fuel_type<-as.factor(autos$fuel_type)
autos$aspiration<-as.factor(autos$aspiration)
autos$num_of_doors<-as.factor(autos$num_of_doors)
autos$body_style<-as.factor(autos$body_style)
autos$drive_wheels<-as.factor(autos$drive_wheels)
autos$engine_location<-as.factor(autos$engine_location)
autos$engine_type<-as.factor(autos$engine_type)
autos$fuel_system<-as.factor(autos$fuel_system)
autos$num_of_cylinders<-as.factor(autos$num_of_cylinders)

## Let's Explore the dataset a bit

## Size of the dataset
nrow(autos)
## Summary of each variable
summary(autos)
## Let's see how many different makes we have in this dataset
levels(autos$make)
## we have 21 different makes in this dataset

## Let's take a look at the distribution of prices
hist(autos$price,ylim = c(0,100))
## most of the cars are within $0 to $10,000. 
## This indicates that probably most cars in this dataset, if not all
## are used cars. 
## There is also a possibility that this car prices are for older model cars, or 
## in other words from many years ago, when cars did not cost as much as it is
## today.

## Let's take a look at price make relationship.
plot(price ~ make, data=autos)
## Since there are too many car makes here, we cannot exactly convey the information
## about each make, but what we can tell for sure is that make definitely
## affects the price of a car.

## Let's take a look at some other variables which I think definitely affect
## the price of a car. 
plot(price~length, data=autos) 
## As we can clearly see from this scatterplot, as the length of a car increases,
## the price of that car increases as well. That is intuitively true as well, 
## since the bigger the car is, the more it costs.
plot(price~curb_weight, data=autos) 
## Same thing is confirmed by the price~weight scatterplot as well. 

## Let's explore some variables related to car's engine
plot(price ~ num_of_cylinders, data=autos)
## We can clearly see that as the number of cylinders increase, the price of a
## car increases as well. That totally makes sense since the cars with bigger
## engines tend to cost more.
## Note that I converted the no. of cylinders variable to a factor, since the 
## number of car cylinders is not a numerical value. Rather, most car engines
## come in 4,6,8,10 or 12 cylinder configurations excluding some car manufacturers
## like Audi that makes legendary 5 cylinder engines as well.

plot(price~horsepower, data=autos) 
## Again we can clearly see that as the car gets more powerful, its price increases too. 

plot(price~ highway_mpg , data=autos) 
## Here we can clearly see that as the MPG goes up, the price increases.
## that is due to the fact that cars with bigger more powerful engines has lower mpg.
plot(price~ city_mpg , data=autos) 
## Same thing here

## Last but not the least let's look at the num of doors - price relationship.
plot(price~ num_of_doors , data=autos) 
## This definitely does not affect car's price too much.

###### Question 8 ######

## Since the main purpose of this homework is to learn more about FDR and
## BH procedure, we will start by regressing the price of a car on all other
## available variables to understand the FDR and BH process a bit better.
autos_regress <- glm(price ~., data=autos)

autos_regress2<- lm(price~., data= autos)

summary(autos_regress)
summary(autos_regress2)

## Results of our regression model shows that overall F-statistic is statistically significant.
## Additionally, R^2 and adjusted R^2 values show that the model is able to explain
## 95% percent of the unexplained variation from the dependent variable(price).
## Also, at 5% significance level, 19 of the variables are significant. 
## At 1% significance level, 10 of the variables are significant.
## In the upcoming questions we will further explore how many of those variables
## are actually true discoveries.

## Save the summary of the regression in a variable
summary_autos_regress<-summary(autos_regress)

## Save the coefficients of the summary in a separate matrix to pull out the p values
auto_regress_coefficients <- summary_autos_regress[["coefficients"]]

## Pull the p-values from the coefficients matrix
autos_regress_pvals<-auto_regress_coefficients[,4]

## Let's see how many significant variables did we get at 1% sig. level
sum(autos_regress_pvals<0.01)
## We got 10 significant.

## What about 5% sig. level.
sum(autos_regress_pvals<0.05)
## We got 19 significant.

###  Why might false discoveries be an issue?
## Since we are using many variables in our regression model, we are conducting
## a hypothesis testing for each one of those variables. Conducting many hypothe
## sis tests increases the probability of false discovery rate everytime we do so. 
## Formally this is called the problem of multiplicity.
## alpha value we set is for a single test. 
## Suppose we repeat the same test for many variables, about alpha*100% of the
## null tests will pop us as statistically significant.
## Suppose that 5 of 100 regression coefficients are actually influential, 
## and that you find all of them significant.
## Test the rest of them at 𝛼 = 0.05:
## Since you reject h0 for 5% of the useless 95 variables,
## 4.75 / 9.75 ≈ 50% of significant tests are false discoveries!
##(Referred to lecture notes for answering this question)


### Use the BH procedure to control the FDR with a q of 0.1. How many true discoveries 
### do you estimate? Plot the cutoff line together with the significant and insignificant p-values.

## Remember that originally at 1% significance level we found 10 variables to 
## be statistically significant.

## Let's run the fdr function
autos_true_disc<-fdr(autos_regress_pvals,0.1,plotit=TRUE)
autos_true_disc
## We get 13 true discoveries which is more than what we originally found at
## 1% significance level.

## Let's take a look at the BH-adjusted alpha value we got as a result of FDR function
autos_BH_alpha<-fdr2(autos_regress_pvals,0.1,plotit=TRUE)
autos_BH_alpha
## As we can clearly see the maximum of p-values that are below the BH-adjusted
## p-value counterparts is 0.02292686. 
## We can consider this as our new alpha value. 
## Since the original alpha value we set(0.01) was already so low, and below the
## BH adjusted alpha value, we get even more true discoveries than what we 
## originally considered to be statistically significant at 1% sig. level. 
## Since BH procedure does not consider any significance value that we set for
## ourselves, this finding does not add any value to our original findings. 

## However, if we were to consider 5% as our significance level originally,
## where we found 19 significant coefficients. 
## As a result of this BH-procedure we can conclude that only 13 of them were
## actual true discoveries, and 6 values came out to be false discoveries. 

## Since we were not specified to consider any alpha threshold for this part of
## assignment, I included my interpretations for both 1% and 5% significance level
##thresholds.
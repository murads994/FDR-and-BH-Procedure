# False Discovery and Benjamini-Hochberg Procedure

In this project we will be looking at the concept of False Discovery and how we can control for it using Benjamini-Hochberg Procedure. 

When we run a linear regression model, we usually assess how many of our covariates are significant, by looking at the p-values of the coefficients. 

A p-value is the probability to observe a test statistic farther in the
distribution tail than what we observe, given the null hypothesis is true(Which is usually $\beta = 0$ in regression). 

The way we use p-values is that we choose an $\alpha$, cut-off for the p-value $"p"$, and conclude significance for $p < \alpha$.

This is equivalent to giving only $\alpha$ probability of a false-positive to your individual tests of significance. For example, in our case of regression, $\beta \neq 0$ only if its p-value is less than the accepted risk of a false discovery for each coefficient.

We use $\alpha$ for a single test. If we repeat the same test many times, about $\alpha × 100\%$ of the null tests will erroneously pop up as significant. 

Suppose that we know out of 100 variables in our regression model, 5 are actually significant.

When we test the rest of them at $\alpha = 0.05$, since we reject null hypothesis for 5% of the useless 95 variables, 4.75 of them will come out as significant. Now, out of 100 we have 4.75+5 ~ 10 significant variables. But since we know that only 5 of them are actually significant, 4.75/9.75 ~ 50% of the findings are false discoveries. This concept is called **False Discovery Proportion(FDP)** .

$$FDP = \frac{\text{no of false positives}}{\text{no  of variables considered significant}}$$

In real world settings, we almost never know which variables in our model are actually significant or false positives i.e we never know the actual False Discovery Proportion. However, we can control for its expectation since by definition we know that:

False Discovery Rate(FDR) = $\mathbb{E}[FDP]$

To control for the False Discovery Rate, we can use **Benjamini-Hochberg Procedure**. 

Suppose we want to be sure that False Discovery Rate $q$ is less than 0.1(10%).

BH Procedure works as following: 

* We start by ranking our p-values smallest to largest, $p(1),p(2),...p(k)$.
* Then we set the p-value cut-off as $p^{\*} = max[ p(k): p(k)\leq q* \frac{k}{N}]$

where, $k$ is rank and, $N$ is the number of variables.

* Choosing p-values that are less than $p^*$ will ensure that False Discovery Rate is below $q$

In this project we will be exploring the concept of False Discovery Rate, and will be applying the BH procedure to control for it, in a randomly generated and a real dataset and interpret our findings.

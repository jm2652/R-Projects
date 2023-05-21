# Data science class project on confidence intervals, posted with permission of instructor

# load libraries
library(tidyverse)

# Create confidence intervals for three cases:
# Case 1: large sample
# Case 2: normal population
# Case 3: bootstrap

# create function that constructs CI for each case
CI1 = function(data,alpha) {
    mean(data) + c(-1,1)*qnorm(1-alpha/2)*sd(data)/sqrt(length(data))
}

CI2 = function(data,alpha) {
    mean(data) + c(-1,1)*qt(1-alpha/2, df = length(data) - 1 )*sd(data)/sqrt(length(data))
}

CI3 = function(data,alpha, n.boot) {
    dev = map_dbl(1:n.boot,function(x) mean(sample(data, size = length(data),
                                                   replace = TRUE))) - mean(data)
    unname(mean(data) - quantile(dev, probs = c(1-alpha/2, alpha/2)))
    
}

# test
set.seed(133)
sample = runif(50)
alpha = 0.05
n.boot = 100
CI1(sample, alpha)
CI2(sample, alpha)
CI3(sample, alpha, n.boot)

# create auxiliary function `aux` that receives numerical vector input x
# of size 3 and returns numerical vector y of size 2, 
# where first element of y is 1 if first element of x lies between
# last two inclusive and 0 otherwise; the second element of y must be 
# the difference between the third and second elements of x
aux = function(x) {
    return(c(x[1] >= x[2] & x[1] <=x [3], x[3] - x[2]))
}
# test
x1 = c(1,2,3)
x2 = c(0.5,0,0.5)
x3 = c(1,1,1)
aux(x1)
aux(x2)
aux(x3)

# Use `CI1`, `CI2`,`CI3` and `aux` to write 3 functions:
# `core1`, `core2`, `core3` which compute CI, check if it contains the mean 
# of a population (parameter), and computes length

# create labels
labels = c('cover','length')

core1 = function(population, n, alpha) {
    mu = mean(population)
    data = sample(population, size = n)
    out = aux(c(mu, CI1(data,alpha)))
    names(out) = labels
    return(out)
}

core2 = function(population, n, alpha) {
    mu = mean(population)
    data = sample(population, size = n)
    out = aux(c(mu, CI2(data,alpha)))
    names(out) = labels
    return(out)
}

core3 = function(population, n, alpha, n.boot) {
    mu = mean(population)
    data = sample(population, size = n)
    out = aux(c(mu, CI3(data,alpha,n.boot)))
    names(out) = labels
    return(out)
}

# test
set.seed(133) 
population = rnorm(1000)
n = 30
alpha = 0.05
n.boot = 100
core1(population, n, alpha)
core2(population, n, alpha)
core3(population, n, alpha, n.boot)

# create functions `empirical1`, `empirical2`,`empirical3` that calculate
# empirical coverage and the average length for the sample average based on
# the average of n.samples samples of size n taken from the population

empirical1 = function(population, n, alpha,n.samples) {
    # run core1 on samples
    df = map_df(1:n.samples, function(x) core1(population, n, alpha))
    # return average
    return(colMeans(df))
}

empirical2 = function(population, n, alpha,n.samples) {
    # run core2 on samples
    df = map_df(1:n.samples, function(x) core2(population, n, alpha))
    # return average
    return(colMeans(df))
}

empirical3 = function(population, n, alpha, n.boot, n.samples) {
    # run core3 on samples
    df = map_df(1:n.samples, function(x) core3(population, n, alpha, n.boot))
    # return average
    return(colMeans(df))
}

# test
n.samples = 100
empirical1(population, n, alpha, n.samples)
empirical2(population, n, alpha, n.samples)
empirical3(population, n, alpha, n.boot, n.samples)

# investigate effect of sample size on empirical coverage

set.seed(133)
n.pop = 1e5
pop0 = rchisq(n = n.pop, df = 5)
alpha = 0.1
n.samples = 10000
n = c(5,6,7,8,9,10,12,15,20,25,30,40,50,100)

# new data
n.data = map_df(n, function(x) {
    empirical1(pop0, x, alpha, n.samples)
})

# line plot of empirical coverage with line for 90% CI for reference
ggplot(n.data) +
    geom_line(aes(x = n, y = cover), color = "red") +
    geom_hline(yintercept = 1-alpha) +
    labs(title = 'Empirical Coverage vs. Sample Size in Case 1',
         x = 'Sample Size',
         y = 'Empirical Coverage')

# investigate influence of distribution on average length and empirical coverage

# create 5 test populations
pop1 = rgamma(n.pop, shape  = 0.1, rate = 1)
pop2 = rgamma(n.pop, shape  = 0.5, rate = 1) 
pop3 = rgamma(n.pop, shape  = 1, rate = 1)
pop4 = rgamma(n.pop, shape  = 2, rate = 1)
pop5 = rgamma(n.pop, shape  = 5, rate = 1)

# function creating histogram with normal curve as reference
normal_hist = function(population) {
    ggplot(data.frame(population), aes(x = population)) + 
        geom_histogram(aes(y = after_stat(density)), color = "white", fill="blue") +
        geom_density(aes(color = "Sample data")) +
        stat_function(fun = dnorm, 
                      args = list(mean = mean(population), sd = sd(population)),
                      mapping = aes(color = "Normal distribution"), 
                      size = 1.2) +
        scale_colour_manual("Density Curve", values = c("Normal distribution" ="pink",
                                                        "Sample data" = "yellow"))
}
# test
normal_hist(pop1)
normal_hist(pop2)
normal_hist(pop3)
normal_hist(pop4)
normal_hist(pop5)

qqplot = function(population) {
    ggplot(data.frame(population), aes(sample=population)) + 
        stat_qq() +
        stat_qq_line(color = "red") + 
        labs(x = "Theoretical Quantiles", 
             y = "Sample Quantiles", title = "Normal QQ Plot ")
}
# test
qqplot(pop1)
qqplot(pop2)
qqplot(pop3)
qqplot(pop4)
qqplot(pop5)

# Use empirical2 to estimate coverage for 10000 samples of size 10
n.samples = 10000
n = 10
alpha = .05
empirical2(pop1, n, alpha, n.samples)
empirical2(pop2, n, alpha, n.samples)
empirical2(pop3, n, alpha, n.samples)
empirical2(pop4, n, alpha, n.samples)
empirical2(pop5, n, alpha, n.samples)
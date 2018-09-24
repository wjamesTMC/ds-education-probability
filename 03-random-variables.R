# --------------------------------------------------------------------------------
#
# Random Variables
#
# --------------------------------------------------------------------------------

# Setup
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(RColorBrewer)
library(Lahman)
library(HistData)

# Random variables

# Random variables are numeric outcomes resulting from random processes. We can
# easily generate random variables using some of the simple examples we have
# shown. For example, define X to be 1, if a bead is blue and red otherwise.

beads <- rep( c("red", "blue"), times = c(2,3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)

# Here X is a random variable: every time we select a new bead the outcome
# changes randomly. See below:

ifelse(sample(beads, 1) == "blue", 1, 0)
#> [1] 0
ifelse(sample(beads, 1) == "blue", 1, 0)
#> [1] 1
ifelse(sample(beads, 1) == "blue", 1, 0)
#> [1] 1

# Sometimes it’s 1 and sometimes it’s 0.

# In data science, we often deal with data that is affected by chance in some
# way: the data comes from a random sample, the data is affected by measurement
# error or the data measures some outcome that is random in nature. Being able
# to quantify the uncertainty introduced by randomness is one of the most
# important jobs of a data scientist. Statistical inference offers a framework,
# as well as several practical tools, for doing this. The first step is to learn
# how to mathematically describe random variables. We start with games of
# chance.

# 28.1 Sampling models

# Many data generation procedures, those that produce the data we study, can be
# modeled quite well as draws from a urn. For instance, we can model the process
# of polling likely voters as drawing 0s (Republicans) and 1s (Democrats) from
# an urn containing the 0 and 1 code for all likely voters. In epidemiological
# studies, we often assume that the subjects in our study are a random sample
# from the population of interest. The data related to a specific outcome can be
# modeled as a random sample from an urn containing that outcome for the entire
# population of interest. Similarly, in experimental research, we often assume
# that the individual organisms we are studying, for example worms, flies, or
# mice, are a random sample from a larger population. Randomized experiments can
# also be modeled by draws from an urn given the way individuals are assigned
# into groups: when getting assigned, you draw your group at random. Sampling
# models are therefore ubiquitous in data science. Casino games offer a plethora
# of examples of real world situations in which sampling models are used to
# answer specific questions. We will therefore start with such examples.

# Suppose a very small casino hires you to consult on whether they should set up
# roulette wheels. To keep the example simple, we will assume that 1,000 people
# will play and that the only game you can play on the roulette wheel is to bet
# on red or black. The casino wants you to predict how much money they will make
# or lose. They want a range of values and, in particular, they want to know
# what’s the chance of losing money. If this probability is too high, they will
# pass on installing roulette wheels.

# We are going to define a random variable S that will represent the casino’s
# total winnings. Let’s start by constructing the urn. A roulette wheel has 18
# red pockets, 18 black pockets and 2 green ones. So playing a color in one game
# of roulette is equivalent to drawing from this urn:

color <- rep(c("Black","Red","Green"), c(18,18,2))

# The 1,000 outcomes from 1,000 people playing are independent draws from this
# urn. If red comes up, the gambler wins and the casino loses a dollar, so we
# draw a -$1. Otherwise, the casino wins a dollar and we draw a $1. We code
# these draws like this:

# To construct our random variable S we can use this code:

n <- 10000
X <- sample(ifelse( color=="Red", -1, 1),  n, replace = TRUE)
X[1:10]
#>  [1]  1 -1 -1 -1 -1  1  1  1 -1  1

# Because we know the proportions of 1s and -1s, we can generate the draws with
# one line of code, without defining color:

X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))

# We call this a sampling model since we are modelling the random behavior of
# roulette with the sampling of draws from an urn. The total winnings S is
# simply the sum of these 1,000 independent draws:

X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
S <- sum(X)
S
#> [1] 664

# 28.2 The probability distribution of a random variable

# If you run the code above, you see that SS changes every time. This is, of
# course, because S is a random variable. The probability distribution of a
# random variable tells us the probability of the observed value falling at any
# given interval. So, for example, if we want to know the probability that we
# lose money, we are asking the probability that S is in the interval S<0S<0.

# Note that if we can define a cumulative distribution function
# F(a)=Pr(S≤a)F(a)=Pr(S≤a), then we will be able to answer any question related
# to the probability of events defined by our random variable SS, including the
# event S<0S<0. We call this FF the random variable’s distribution function.

# We can estimate the distribution function for the random variable SS by using
# a Monte Carlo simulation to generate many realizations of the random variable.
# With this code, we run the experiment of having 1,000 people play roulette,
# over and over, specifically B=10,000 times:

n <- 1000
B <- 10000
roulette_winnings <- function(n){
     X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
     sum(X)
}
S <- replicate(B, roulette_winnings(n))

# So now we can ask the following: in our simulations, how often did we get sums
# less than or equal toa?

mean(S <= a)

# This will be a very good approximation of F(a)F(a). In fact, we can visualize
# the distribution by creating a histogram showing the probability
# F(b)−F(a)F(b)−F(a) for several intervals (a,b](a,b]:


# Now we can easily answer the casino’s question: how likely is it that we will
# lose money?

mean(S<0)
#> [1] 0.046

# We can see it is quite low.

# In the histogram above, we see that the distribution appears to be
# approximately normal. A qq-plot will confirm that the normal approximation is
# close to perfect. If in fact the distribution is normal, then all we need to
# define the distribution is the average and the standard deviation. Because we
# have the original values from which the distribution is created, we can easily
# compute these:

mean(S)
#> [1] 52.5
sd(S)
#> [1] 31.7

# If we add a normal density with this average and standard deviation to the
# histogram above, we see that it matches very well:

# This average and this standard deviation have special names. They are referred
# to as the expected valueand standard error of the random variable SS. We will
# say more about these in the next section.

# It turns out that statistical theory provides a way to derive the distribution
# of random variables defined as independent random draws from an urn.
# Specifically, in our example above, we can show that (S+n)/2(S+n)/2follows a
# binomial distribution. We therefore do not need to run for Monte Carlo
# simulations to know the probability distribution of SS. We did this for
# illustrative purposes.

# We can use the function dbinom and pbinom to compute the probabilities
# exactly. For example, to know compute Pr(S<0)Pr(S<0) we note that:

# Pr(S<0)=Pr((S+n)/2<(0+n)/2)

# and we can use the pbinom to computePr(S≤0)Pr(S≤0)

pbinom(n/2, size = n, prob = 9/19)
#> [1] 0.955

# Because this is a discrete probability function, to get Pr(S<0)Pr(S<0) rather
# than Pr(S≤0)Pr(S≤0) we write:

pbinom(n/2-1, size = n, prob = 9/19)
#> [1] 0.949

# For the details of the binomial distribution you can consult any basic
# probability book or even Wikipedia.

# Here we do not cover these details. Instead, we will discuss an incredibly
# useful approximation provided by mathematical theory that applies generally to
# sums and averages of draws from any urn: the Central Limit Theorem (CLT).

# 28.3 Distributions versus probability distributions

# Before we continue, let’s make an important distinction and connection between
# the distribution of a list of numbers and a probability distribution. In the
# visualization chapter, we described how any list of numbers x1,…,xnx1,…,xn has
# a distribution. The definition is quite straightforward. We define F(a)F(a) as
# the function that answers the question: what proportion of the list is less
# than or equal to aa?. Because they are useful summaries when the distribution
# is approximately normal, we define the average and standard deviation. These
# are defined with a straightforward operation of the vector containing the list
# of numbers x:

avg <- sum(x)/length(x)
s <- sqrt(sum((x - avg)^2) / length(x))

# A random variable XX has a distribution function. To define this we do not
# need a list of numbers. It is a theoretical concept. In this case, we define
# the distribution as the F(a)F(a) that answers the question: what is the
# probability that XX is less than or equal to aa? There is no list of numbers.

# However, if XX is defined by drawing from an urn with numbers in it, then
# there is a list: the list of numbers inside the urn. In this case, the
# distribution of that list is the probability distribution of XX and the
# average and standard deviation of that list are the expected value and
# standard error of the random variable.

# Another way to think about it, that does not involve an urn, is to run a Monte
# Carlo simulation and generate a very large list of outcomes of XX. These
# outcomes are a list of numbers. The distribution of this list will be a very
# good approximation of the probability distribution of XX. The longer the list,
# the better the approximation. The average and standard deviation of this list
# will approximate the expected value and standard error of the random variable.

# 28.4 Notation for random variables

# In statistical textbooks, upper case letters are used to denote random
# variables and we follow this convention here. Lower case letters are used for
# observed values. You will see some notation that includes both. For example,
# you will see events defined as X≤xX≤x. Here XX is a random variable, making it
# a random event, and xx is an arbitrary value and not random. So, for example,
# XX might represent the number on a die roll and xx will represent an actual
# value we see. So in this case, the probability of X=xX=xis 1/6 regardless of
# the value of xx. This notation is a bit strange because, when we ask questions
# about probability, XX is not an observed quantity. Instead it’s a random
# quantity that we will see in the future. We can talk about what we expect it
# to be, what values are probable, but not what it is. But once we have data, we
# do see a realization of XX. So data scientists talk of what could have been
# after we see what actually happened.

# 28.5 Central Limit Theorem

# The Central Limit Theorem (CLT) tells us that when the number of draws, also
# called the sample size, is large, the probability distribution of the sum of
# the independent draws is approximately normal. Because sampling models are
# used for so many data generation processes, the CLT is considered one of the
# most important mathematical insights in history.

# Previously, we discussed that if we know that the distribution of a list of
# numbers is approximated by the normal distribution, all we need to describe
# the list are the average and standard deviation. We also know that the same
# applies to probability distributions. If a random variable has a probability
# distribution that is approximated with the normal distribution, then all we
# need to describe the probability distribution are the average and standard
# deviation, referred to as the expected value and standard error.

# 28.6 The expected value and standard error

# We have described sampling models for draws. We will now go over the
# mathematical theory that lets us approximate the probability distributions for
# the sum of draws. Once we do this, we will be able to help the casino predict
# how much money they will make. The same approach we use for the sum of draws
# will be useful for describing the distribution of averages and proportion
# which we will need to understand how polls work.

# The first important concept to learn is the expected value. In statistics
# books, it is common to use letter EElike this:

# E[X]E[X]

# to denote the expected value of the random variable XX.

# A random variable will vary around its expected value in a way that if you
# take the average of many, many draws, the average of the draws will
# approximate the expected value, getting closer and closer the more draws you
# take.

# Theoretical statistics provides techniques that facilitate the calculation of
# expected values in different circumstances. For example, a useful formula
# tells us that the expected value of a random variable defined by one draw is
# the average of the numbers in the urn. So in the urn used to model betting on
# red in roulette, we have 20 one dollars, and 18 negative one dollars. The
# expected value is thus:

# E[X]=(20+−18)/38

# which is about 5 cents. It is a bit counterintuitive to say that XX varies
# around 0.05, when the only values it takes is 1 and -1. One way to make sense
# of the expected value in this context is by realizing that if we play the game
# over and over, the casino wins, on average, 5 cents per game. A Monte Carlo
# simulation confirms this:

B <- 10^6
X <- sample(c(-1,1), B, replace = TRUE, prob=c(9/19, 10/19))
mean(X)
#> [1] 0.0515

# In general, if the urn has two possible outcomes, say aa and bb, with
# proportions pp and 1−p1−p respectively, the average is:

# ap+b(1−p).

# To see this, notice that if there are nn beads in the urn, then we have npnp
# aas, n(1−p)n(1−p) bbs and because the average is the sum,
# n×a×p+n×b×(1−p)n×a×p+n×b×(1−p), divided by the total nn, we get that the
# average is ap+b(1−p)ap+b(1−p).

# Now the reason we define the expected value is because this mathematical
# definition turns out to be useful for approximating the probability
# distributions of sum, which then is useful for describing the distribution of
# averages and proportions. The first useful fact is that the expected value of
# the sum of the draws is:

# number of draws × average of the numbers in the urnnumber of draws × average
# of the numbers in the urn

# So if 1,000 people play roulette, the casino expects to win, on average, about
# 1,000 ×× $0.05 = $50. But this is an expected value. How different can one
# observation be from the expected value? The casino really needs to know this.
# What is the range of possibilities? If negative numbers are too likely, they
# will not install roulette wheels. Statistical theory once again answers this
# question. The standard error (SE) gives us an idea of the size of the
# variation around the expected value. In statistics books, it’s common to use:

# SE[X]SE[X]

# to denote the standard error of a random variable.

# If our draws are independent, then the standard error of the sum is given by
# the equation:

# √number of draws × standard deviation of the numbers in the urnnumber of draws
# × standard deviation of the numbers in the urn

# Using the definition of standard deviation, we can derive, with a bit of math,
# that if a jar contains two values aa and bb with proportions pp and (1−p)(1−p)
# respectively, the standard deviation is:

# ∣b−a∣√p(1−p).

# So in our roulette example, the standard deviation of the values inside the
# urn is: ∣1−(−1)∣√10/19×9/19∣1−(−1)∣10/19×9/19 or

2 * sqrt(90)/19
#> [1] 0.999

# The standard error tells us the typical difference between a random variable
# and its expectation. Since one draw is obviously the sum of just one draw, we
# can use the formula above to calculate that the random variable defined by one
# draw has an expected value of 0.05 and a standard error of about 1. This makes
# sense since we either get 1 or -1, with 1 slightly favored over -1.

# Using the formula above, the sum of 1,000 people playing has standard error of
# about $32:

n <- 1000
sqrt(n) * 2 * sqrt(90)/19
#> [1] 31.6

# As a result, when 1,000 people bet on red, the casino is expected to win $50
# with a standard error of $32. It therefore seems like a safe bet. But we still
# haven’t answered the question: how likely is it to lose money? Here the CLT
# will help.

# Advanced note: Before continuing we should point out that exact probability calculations for the casino winnings can be performed with the binomial distribution. However, here we focus on the CLT which can be generally applied to sums of random variables in a way that the binomial distribution can’t.

# 28.7 Central Limit Theorem approximation

# The Central Limit Theorem (CLT) tells us that the sum S is approximated by a
# normal distribution. Using the formulas above, we know that the expected value
# and standard error are:

n * (20-18)/38 
#> [1] 52.6
sqrt(n) * 2 * sqrt(90)/19 
#> [1] 31.6

# The theoretical values above match those obtained with the Monte Carlo
# simulation:

mean(S)
#> [1] 52.5
sd(S)
#> [1] 31.7

# Using the CLT, we can skip the Monte Carlo simulation and instead compute the
# probability of the casino losing money using this approximation:

mu <- n * (20-18)/38
se <-  sqrt(n) * 2 * sqrt(90)/19 
pnorm(0, mu, se)
#> [1] 0.0478

# which is also in very good agreement with our Monte Carlo result:

mean(S < 0)
#> [1] 0.046

# 28.8 Statistical properties of averages

# There are two useful mathematical results that we used above and often employ
# when working with data. We list them below.

#  1. The expected value of the sum of random variables is the sum of each random
# variable’s expected value. We can write it like this:
#  2. E[X1+X2+⋯+Xn]=E[X1]+E[X2]+⋯+E[Xn]
#  3. If the XX are independent draws from the urn, then they all have the same
# expected value. Let’s call it μμ and thus:
#  4. E[X1+X2+⋯+Xn]=nμ
# which is another way of writing the result we show above for the sum of draws.
#  5. The expected value of a non-random constant times a random variable is the 
#     non-random constant times the expected value of a random variable. This is
#     easier to explain with symbols:E[aX]=a×E[X]E[aX]=a×E[X]
#  6. To see why this is intuitive, consider change of units. If we change the 
#     units of a random variable, say from dollars to cents, the expectation 
#     should change in the same way. A consequence of the above two facts is that 
#     the expected value of the average of independent draws from the same urn is 
#     the expected value of the urn, call it μμ again:
#  7. E[(X1+X2+⋯+Xn)/n]=E[X1+X2+⋯+Xn]/n=nμ/n=μ
#  8. The square of the standard error of the sum of independent random variables 
#     is the sum of the square of the standard error of each random variable. This 
#     one is easier to understand in math form:
#     SE[X1+X2+⋯+Xn]=√SE[X1]2+SE[X2]2+⋯+SE[Xn]2
#  9. The square of the standard error is referred to as the variance in statistical 
#     textbooks.
# 10. The standard error of a non-random constant times a random variable is the 
#     non-random constant times the random variable’s standard error. As with the 
#     expectation:SE[aX]=a×SE[X]SE[aX]=a×SE[X]
# 11. To see why this is intuitive, again think of units.
# 12. A consequence of 3 and 4 is that the standard error of the the average of
#     independent draws from the same urn is the standard deviation of the urn
#     divided by the square root of nn (the number of draws), call it σσ:
# 13. SE[(X1+X2+⋯+Xn)/n]=SE[X1+X2+⋯+Xn]/n=√SE[X1]2+SE[X2]2+⋯+SE[Xn]2/n=√σ2+σ2+⋯+σ2/n=√nσ2/n=σ/√n

# If XX is a normally distributed random variable, then if aa and bb are
# non-random constants, aX+baX+b is also a normally distributed random variable.
# All we are doing is changing the units of the random variable by multiplying
# by aa, then shifting the center by bb.

# Why we use μμ and σσ

# Statistical textbooks use the Greek letters μμ and σσ to denote the expected
# value and standard error respectively. This is because μμ is the Greek letter
# for mm, the first letter of mean, which is another term used for expected
# value. Similarly, σσ is the Greek letter for ss, the first letter of standard
# error.

# 28.9 Law of large numbers

# An important implication of the final result is that the standard error of the
# average becomes smaller and smaller as nn grows larger. When nn is very large,
# then the standard error is practically 0 and the average of the draws
# converges to the average of the urn. This is known in statistical textbooks as
# the law of large numbers or the law of averages.

# 28.9.1 Misinterpreting law of averages

# The law of averages is sometimes misinterpreted. For example, if you toss a
# coin 5 times and see a head each time, you might hear someone argue that the
# next toss is probably a tail because of the law of averages: on average we
# should see 50% heads and 50% tails. A similar argument would be to say that
# red “is due” on the roulette wheel after seeing black come up five times in a
# row. These events are independent so the chance of a coin landing heads is 50%
# regardless of the previous 5. This is also the case for the roulette outcome.
# The law of averages applies only when the number of draws is very large and
# not in small samples. After a million tosses, you will definitely see about
# 50% heads regardless of the outcome of the first five tosses.

# Another funny misuse of the law of averages is in sports when TV sportscasters
# predict a player is about to succeed because they have failed a few times in a
# row.

# 28.10 How large is large in CLT?

# The CLT works when the number of draws is large. But large is a relative term.
# In many circumstances as few as 30 draws is enough to make the CLT useful. In
# some specific instances, as few as 10 is enough. However, these should not be
# considered general rules. Note, for example, that when the probability of
# success is very small, we need larger sample sizes.

# By way of illustration, let’s consider the lottery. In the lottery, the
# chances of winning are less than 1 in a million. Thousands of people play so
# the number of draws is very large. Yet the number of winners, the sum of the
# draws, range between 0 and 4. This sum is certainly not well approximated by a
# normal distribution so the CLT does not apply, even with the very large sample
# size. This is generally true when the probability of a success is very low. In
# these cases, the Poisson distribution is more appropriate.

# You can examine the properties of the Poisson distribution using dpois and
# ppois. You can generate random variables following this distribution with
# rpois. However, we do not cover the theory here. You can learn about the
# Poisson distribution in any probability textbook and even Wikipedia.

# 28.11 Population SD versus the sample SD

# The standard deviation of a list x (we use heights as an example) is defined
# as the square root of the average of the squared differences:

library(dslabs)
x <- heights$height
m <- mean(x)
s <- sqrt(mean((x-m)^2))

# Using mathematical notation we write:

# μ=1nn∑i=1xiσ=⎷1nn∑i=1(xi−μ)2

# However, be aware that the sd function returns a slightly different result:

identical(s,sd(x))
#> [1] FALSE
s-sd(x)
#> [1] -0.00194

# This is because the sd function R does not return the sd of the list, but
# rather uses a formula that estimates standard deviations of a population from
# a random sample X1,…,XNX1,…,XN which, for reasons not discussed here, divide
# by the N−1N−1.

# ¯X=1NN∑i=1Xis=⎷1N−1N∑i=1(Xi−¯X)

# You can see that this is the case by typing:

N <- length(x)
s-sd(x)*sqrt((N-1)/N)
#> [1] 0

# For all the theory discussed here, you need to compute the actual standard
# deviation as defined:

sqrt(mean((x-m)^2))

# So be careful when using the sd function in R. However, keep in mind that
# throughout the book we sometimes use the sd function when we really want the
# actual SD. This is because when the list size is big, these two are
# practically equivalent since √(N−1)/N(N−1)/N is close to 1.



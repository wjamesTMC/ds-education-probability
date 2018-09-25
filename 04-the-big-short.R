# --------------------------------------------------------------------------------
#
# Case study: The Big Short
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

# 29.1 Interest rates explained with chance model

# More complex versions of the sampling models we have discussed are also used
# by banks to decide interest rates. Suppose you run a small bank that has a
# history of identifying potential homeowners that can be trusted to make
# payments. In fact, historically, in a given year, only 2% of your customers
# default, meaning that they don’t pay back the money that you lent them.
# However, you are aware that if you simply loan money to everybody without
# interest, you will end up losing money due to this 2%. Although you know 2% of
# your clients will probably default, you don’t know which ones. Yet by charging
# everybody just a bit extra in interest, you can make up the loses incurred due
# to that 2% and also cover your operating costs. You can also make a profit,
# but if you set the interest rates too high, your clients will go to another
# bank. We use all these facts and some probability theory to decide what
# interest rate you should charge.

# Suppose your bank will give out 1,000 loans for $180,000 this year. Also,
# after adding up all costs, suppose your bank loses $200,000 per foreclosure.
# For simplicity, we assume this includes all operational costs. A sampling
# model for this scenario can be coded like this:

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02 
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)
#> [1] -3e+06

# Note that the total loss defined by the final sum is a random variable. Every
# time you run the above code you get a different answer. We can easily
# construct a Monte Carlo simulation to get an idea of the distribution of this
# random variable.

B <- 10000
losses <- replicate(B, {
     defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
     sum(defaults * loss_per_foreclosure)
})

# Here is the distribution of this random variable:

# We don’t really need a Monte Carlo simulation though. Using what we have
# learned, the CLT tells us that because our losses are a sum of independent
# draws, its distribution is approximately normal with expected value and
# standard errors given by:

n*(p*loss_per_foreclosure + (1-p)*0)
#> [1] -4e+06
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))
#> [1] 885438

# We can now set an interest rate to guarantee that, on average, we break even.
# Basically, we need to add a quantity xx to each loan, which in this case are
# represent by draws, so that the expected value is 0. If we define ll to be the
# loss per foreclosure, we need:

# lp+x(1−p)=0

# which implies xx is 

- loss_per_foreclosure*p/(1-p)
#> [1] 4082

# or an interest rate of 0.023.

#  that makes it unlikely for this to happen. At the same time, if the interest
#  rate is too high, our clients will go to another bank so we must be willing
#  to take some risks. So let’s say that we want our chances of losing money to
#  be 1 in 100, what does the xx quantity need to be now? This one is a bit
#  harder. We want the sum SS to have:

# Pr(S<0)=0.01Pr(S<0)=0.01

# We know that SS is approximately normal. The expected value of SS
# is{lp+x(1−p)}n{lp+x(1−p)}n with nn the number of draws, which in this case
# represents loans. The standard error is |x−l|√np(1−p)|x−l|np(1−p). Because xx
# is positive and ll negative |x−l|=x−l|x−l|=x−l. Note that these are just the
# formulas from above, but using more compact symbols.

# Now we are going to use a mathematical “trick” that is very common in
# statistics. We are going add and subtract the same quantities to both sides of
# the event S<0S<0 so that the probability does not change and we end up with a
# standard normal random variable on the left, which will then permit us to
# write down an equation with only xx as an unknown. This “trick” is as follows:

# If Pr(S<0)=0.01Pr(S<0)=0.01 then Pr(S−E[S]SE[S]<−E[S]SE[S])
#
# And remember E[S]E[S] and SE[S]SE[S] are the expected value and standard error
# of SS respectively.

# All we did above was add and divide by the same quantity on both sides. We did
# this because now the term on the left is a standard normal random variable,
# which we will rename ZZ. Now we fill in the blanks with the actual formula for
# expected value and standard error:

# (Z<−{lp+x(1−p)}n(x−l)√np(1−p))=0.01Pr(Z<−{lp+x(1−p)}n(x−l)np(1−p))=0.01

# It may look complicated, but remember that ll, pp and nn are all known amounts
# so eventually we will turn them into numbers.

# Now because the term on the left side is a normal random with expected value 0
# and standard error 1, it means that the quantity on the left must be equal to:

qnorm(0.01)
#> [1] -2.33
for the equation to hold true. Remember that z=z=qnorm(0.01) gives us the 
# value of zz for which:
Pr(Z≤z)=0.01Pr(Z≤z)=0.01

# So this means that right side of the complicated equation must be
# zz=qnorm(0.01).

# −{lp+x(1−p)}N(x−l)√Np(1−p)=z

# The trick works because we end up with an expression containing xx that we
# know has to be equal to a known quantity zz. Solving for xx is now simply
# algebra:

# x=−lnp−z√np(1−p)N(1−p)+z√np(1−p)

# which is:
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
#> [1] 6249

# Our interest rate now goes up to 0.035. This is still a very competitive
# interest rate. By choosing this interest rate, we now have an expected profit
# per loan of:

loss_per_foreclosure*p + x*(1-p)
#> [1] 2124

# which is a total expected profit of about:

n*(loss_per_foreclosure*p + x*(1-p)) 
#> [1] 2124198

# dollars!

# We can run a Monte Carlo simulation to double check our theoretical
# approximations:

B <- 100000
profit <- replicate(B, {
     draws <- sample( c(x, loss_per_foreclosure), n, 
                      prob=c(1-p, p), replace = TRUE) 
     sum(draws)
})
mean(profit)
#> [1] 2122436
mean(profit<0)
#> [1] 0.0128

# 29.2 The Big Short

# One of your employees points out that since the bank is making 2124 dollars
# per loan that you should give out more loans! Why just n? You explain that
# finding those n clients was hard. You need a group that is predictable and
# that keeps the chances of defaults low. He then points out that even if the
# probability of default is higher, as long as our expected value is positive,
# you can minimize your chances of losses by increasing nn and relying on the
# law of large numbers.

# He claims that even if the default rate is twice as high, say 4%, if we set
# the rate just a bit higher than:

p <- 0.04
r <- (- loss_per_foreclosure*p/(1-p)) / 180000
r
#> [1] 0.0463

# At 5%, we are guaranteed a positive expected value of:

r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x * (1-p)
#> [1] 640

# and can minimize our chances of losing money by simply increasing nn since:

# Pr(S<0)=Pr(Z<−E[S]SE[S]) with Z a standard normal random variable as above. If
# we define μμ and σσ to be the expected value and standard deviation of the urn
# respectively (that is of a single loan), using the formulas above we have:
# E[S]=nμE[S]=nμ and SE[S]=√nσSE[S]=nσ. So if we define zz=qnorm(0.01), we
# have:−nμ√nσ=−√nμσ=z−nμnσ=−nμσ=zwhich implies that if we let:

# n≥z2σ2/μ2 we are guaranteed to have a probability of less than 0.01. The
# implication is that, as long as μμ is positive, we can find an nn that
# minimizes the probability of a loss. This is a form of the law of large
# numbers: when nn is large, our average earnings per loan converges to the
# expected earning μμ.

# With xx fixed, now we can ask what nn do we need for the probability to be
# 0.01? In our example, if we give out:

z <- qnorm(0.01)
n <- ceiling( (z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2 )
n
#> [1] 22163

# loans, the probability of losing is about 0.01 and we are expected to earn a
# total of:

n*(loss_per_foreclosure*p + x * (1-p))
#> [1] 14184320

# dollars! We can confirm this with a Monte Carlo simulation:
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
     draws <- sample( c(x, loss_per_foreclosure), n, 
                      prob=c(1-p, p), replace = TRUE) 
     sum(draws)
})
mean(profit<0)
#> [1] 0.0107

# This seems like a no brainer. As a result, your colleague decides to leave
# your bank and start his own high risk mortgage company. A few months later,
# your colleague’s bank has gone bankrupt. A book is written and eventually a
# movie is made relating the mistake your friend, and many others, made. What
# happened?

# Your colleague’s scheme was mainly based on this mathematical
# formula:SE[(X1+X2+⋯+Xn)/n]=σ/√nSE[(X1+X2+⋯+Xn)/n]=σ/n

# making nn large, we minimize the standard error of our per-loan profit.
# However, for this rule to hold, the XXs must be independent draws: one person
# defaulting must be independent of others defaulting. Note that in the case of
# averaging the same event over and over, an extreme example of events that are
# not independent, we get a standard error that is √nn times
# bigger:SE[(X1+X1+⋯+X1)/n]=SE[nX1/n]=σ>σ/√nSE[(X1+X1+⋯+X1)/n]=SE[nX1/n]=σ>σ/n

# To construct a more realistic simulation than the original one your colleague
# ran, let’s assume there is a global event that affects everybody with high
# risk mortgages and changes their probability. We will assume that with 50-50
# chance, all the probabilities go up or down slightly to somewhere between 0.03
# and 0.05. But it happens to everybody at once, not just one person. These
# draws are no longer independent.

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
     new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
     draws <- sample( c(x, loss_per_foreclosure), n, 
                      prob=c(1-new_p, new_p), replace = TRUE) 
     sum(draws)
})

# Note that our expected profit is still large:
mean(profit)
#> [1] 14188864
However, the probability of the bank having negative earning shoots up to:
     mean(profit<0)
#> [1] 0.346

# Even scarier is that the probability of losing more than 10 million dollars is:
mean(profit < -10000000)
#> [1] 0.238

# To understand how this happens look at the distribution:
data.frame(profit_in_millions=profit/10^6) %>% ggplot(aes(profit_in_millions)) + geom_histogram(color="black", binwidth = 5)

# The theory completely breaks down and the random variable has much more
# variability than expected. The financial meltdown of 2007 was due, among other
# things, to financial “experts” assuming independence when there was none.


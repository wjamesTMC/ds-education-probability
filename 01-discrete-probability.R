# --------------------------------------------------------------------------------
#
# Discrete Probability
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

# 26 - Discrete probability

# We start by covering some basic principles related to categorical data. The
# subset of probability is referred to as discrete probability. It will help us
# understand the probability theory we will later introduce for numeric and
# continuous data, which is much more common in data science applications.
# Discrete probability is more useful in card games and therefore we use these
# as examples.

# 26.1 Relative frequency

# The word probability is used in everyday language. Answering questions about
# probability is often hard, if not impossible. Here we discuss a mathematical
# definition of probability that does permit us to give precise answers to
# certain questions.

# For example, if I have 2 red beads and 3 blue beads inside an urn and I pick
# one at random, what is the probability of picking a red one? Our intuition
# tells us that the answer is 2/5 or 40%. A precise definition can be given by
# noting that there are five possible outcomes of which two satisfy the
# condition necessary for the event “pick a red bead”. Since each of the five
# outcomes has the same chance of occurring, we conclude that the probability is
# .4 for red and .6 for blue.

# A more tangible way to think about the probability of an event is as the
# proportion of times the event occurs when we repeat the experiment over and
# over, independently, and under the same conditions.

# 26.2 Notation

# We use the notation Pr(A)Pr(A) to denote the probability of event AA
# happening. We use the very general term event to refer to things that can
# happen when something occurs by chance. For example, in our previous example,
# the event was “picking a red bead”. In a political poll in which we call 100
# likely voters at random, an example of an event is “calling 48 Democrats and
# 52 Republicans”.

# In data science applications, we will often deal with continuous variables. In
# these cases, events will often be things like “is this person taller than 6
# feet”. In this case, we write events in a more mathematical form: X≥6X≥6. We
# will see more of these examples later. Here we focus on categorical data.

# 26.3 Monte Carlo simulations

# Computers provide a way to actually perform the simple random experiment
# described above: pick a bead at random from a bag that contains three blue
# beads and two red ones. Random number generators permit us to mimic the
# process of picking at random.

# An example is the sample function in R. We demonstrate its use in the code
# below. First, we use the function rep to generate the urn:

beads <- rep( c("red", "blue"), times = c(2,3))
beads
#> [1] "red"  "red"  "blue" "blue" "blue"

# and then use sample to pick a bead at random:

sample(beads, 1)
#> [1] "blue"

# This line of code produces one random outcome. We want to repeat this
# experiment “over and over”, but it is impossible to repeat forever. Instead,
# we repeat the experiment a large enough number of times to make the results
# practically equivalent. This is an example of a Monte Carlo simulation.

# Much of what mathematical and theoretical statisticians study, which we do not
# cover in this book, relates to providing rigorous definitions of “practically
# equivalent” as well as studying how close a large number of experiments gets
# us to what happens in the limit. Later in this section, we provide a practical
# approach to deciding what is “large enough”.

# To perform our first Monte Carlo simulation, we use the replicate function,
# which permits us to repeat the same task any number of times. Here we repeat
# the random event B=B= 10,000 times:

B <- 10000
events <- replicate(B, sample(beads, 1))

# We can now see if our definition actually is in agreement with this Monte
# Carlo simulation approximation. We can use table to see the distribution:

tab <- table(events)
tab
#> events
#> blue  red 
#> 5954 4046

# and prop.table gives us the proportions:

prop.table(tab)
#> events
#>  blue   red 
#> 0.595 0.405

# The numbers above are the estimated probabilities provided by this Monte Carlo
# simulation. Statistical theory, not covered here, tells us that BB gets larger
# as the estimates get closer to 3/5=.6 and 2/5=.4.

# Although this is a simple and not very useful example, we will use Monte Carlo
# simulations to estimate probabilities in cases in which it is harder to
# compute the exact ones. Before delving into more complex examples, we use
# simple ones to demonstrate the computing tools available in R.

# 26.4 Setting the random seed

# Before we continue, we will briefly explain the following important line of
# code:

set.seed(1)

# Throughout this book, we use random number generators. This implies that many of the results presented can actually change by chance, which then suggests that a frozen version of the book may show a different result than what you obtain when you try to code shown in the book. This is actually fine since the results are random and change from time to time. However, if you want to to ensure that results are exactly the same every time you run them, you can set R’s random number generation seed to a specific number. Above we set it to 1.

?set.seed

# In the exercises we may ask you to set the seed to assure that the results you
# obtain are exactly what we expect them to be.

# With and without replacement

# The function sample has an argument that permits us to pick more than one
# element from the urn. However, by default, this selection occurs without
# replacement: after a bead is selected, it is not put back in the bag. Notice
# what happens when we ask to randomly select five beads:

sample(beads, 5)
#> [1] "red"  "blue" "blue" "blue" "red"
sample(beads, 5)
#> [1] "blue" "blue" "red"  "blue" "red"
sample(beads, 5)
#> [1] "red"  "red"  "blue" "blue" "blue"

# This results in rearrangements that always have three blue and two red beads.
# If we ask that six beads be selected, we get an error:

sample(beads, 6)

# Error in sample.int(length(x), size, replace, prob) : cannot take a sample
# larger than the population when 'replace = FALSE'

# However, the sample function can be used directly, without the use of
# replicate, to repeat the same experiment of picking 1 out of the 5 beads,
# continually, under the same conditions. To do this we sample with replacement:
# return the bead back to the urn after selecting it.

# We can tell sample to do this changing the replace argument , which defaults
# to FALSE, to replace=TRUE:

events <- sample(beads, B, replace = TRUE)
prop.table(table(events))
#> events
#>  blue   red 
#> 0.597 0.403

# Not surprisingly, we get results very similar to those previously obtained
# with replicate.

# 26.5 Probability distributions

# Defining a distribution for categorical outcomes is relatively
# straightforward. We simply assign a probability to each category. In cases
# that can be thought of as beads in an urn, for each bead type, their
# proportion defines the distribution.

# If we are randomly calling likely voters from a population that is 44%
# Democrat, 44% Republican, 10% undecided and 2% Green Party, these proportions
# define the probability for each group. The probability distribution is:

# Pr(picking a Republican)=0.44Pr(picking a Democrat)=0.44Pr(picking an
# undecided)=0.10Pr(picking a Green)=0.02Pr(picking a Republican)=0.44Pr(picking
# a Democrat)=0.44Pr(picking an undecided)=0.10Pr(picking a Green)=0.02

# 26.6 Independence

# We say two events are independent if the outcome of one does not affect the
# other. The classic example are coin tosses. Every time we toss a fair coin,
# the probability of seeing heads is 1/2 regardless of what previous tosses have
# revealed. The same is true when we pick beads from an urn with replacement. In
# the example above, the probability of red is 0.40 regardless of previous
# draws.

# Many examples of events that are not independent come from card games. When we
# deal the first card, the probability of getting a King is 1/13 since there are
# thirteen possibilities: Ace, Deuce, Three, ……, Ten, Jack, Queen, King, and
# Ace. Now if we deal a King for the first card, and don’t replace it into the
# deck, the probabilities of a second card being a King is less because there
# are only three Kings left: the probability is 3 out of 51. These events are
# therefore not independent. The first outcome affected the next one.

# To see an extreme case of non-independent events, consider our example of
# drawing five beads at random without replacement:

x <- sample(beads, 5)

# If you have to guess the color of the first bead, you will predict blue since
# blue has a 60% chance. But if I show you the result of the last four outcomes:

x[2:5]
#> [1] "blue" "blue" "blue" "red"

# would you still guess blue? Of course not. Now you know that the probability
# of red is 1. The events are not independent so the probabilities changes.

# 26.7 Conditional probabilities

# When events are not independent, conditional probabilities are useful. We
# already saw an example of a conditional probability: we computed the
# probability that a second dealt card is a King given that the first was a
# King. In probability, we use the following notation:

# Pr(Card 2 is a king∣Card 1 is a king)=3/51Pr(Card 2 is a king∣Card 1 is a
# king)=3/51

# We use the ∣∣ as shorthand for “given that” or “conditional on”.

# When two events, say AA and BB, are independent, we have:

# Pr(A∣B)=Pr(A)Pr(A∣B)=Pr(A)

# This the mathematical way of saying: the fact that BB happened does not affect
# the probability of AAhappening. In fact, this can be considered the
# mathematical definition of independence.

# 26.8 Multiplication rule

# If we want to know the probability of two events, say AA and BB, occurring, we
# can use the multiplication rule:

# Pr(A and B)=Pr(A)Pr(B∣A)Pr(A and B)=Pr(A)Pr(B∣A)

# Let’s use Blackjack as an example. In Blackjack, you are assigned two random
# cards. After you see what you have, you can ask for more. The goal is to get
# closer to 21 than the dealer, without going over. Face cards are worth 10
# points and Aces are worth 11 or 1 (you choose).

# So, in a Blackjack game, to calculate the chances of getting a 21 by drawing
# an Ace and then a face card, we compute the probability of the first being and
# Ace and multiply by the probability of drawing a face card given that the
# first was an Ace: 1/13×12/52≈0.021/13×12/52≈0.02

# The multiplicative rule also applies to more than two events. We can use
# induction to expand for more events:

# Pr(A and B and C)=Pr(A)Pr(B∣A)Pr(C∣A and B)Pr(A and B and
# C)=Pr(A)Pr(B∣A)Pr(C∣A and B)

# Multiplication rule under independence

# When we have independent events, then the multiplication rule becomes simpler:

# Pr(A and B and C)=Pr(A)Pr(B)Pr(C)Pr(A and B and C)=Pr(A)Pr(B)Pr(C)

# But we have to be very careful before using this, as assuming independence can
# result in very different and incorrect probability calculations when we don’t
# actually have independence.

# As an example, imagine a court case in which the suspect was described as
# having a mustache and a beard. The defendant has a mustache and a beard and
# the prosecution brings in an “expert” to testify that 1/10 men have beards and
# 1/5 have mustaches so using the multiplication rule we conclude that only
# 1/10×1/51/10×1/5 or 0.02 have both.

# But to multiply like this we need to assume independence! The conditional
# probability of a man having a mustache conditional on him having a beard is
# .95. So the correct calculation probability is much higher: 0.09.

# The multiplication rule also gives us a general formula for computing
# conditional probabilities:

# Pr(B∣A)=Pr(A and B)Pr(A)Pr(B∣A)=Pr(A and B)Pr(A)

# To illustrate how we use these formulas and concepts in practice, we will use
# several examples related to card games.

# 26.9 Combinations and permutations

# In our very first example we imagined an urn with five beads. As a reminder,
# to compute the probability distribution of one draw, we simply listed out all
# the possibilities. There were 5 and so then, for each event, we counted how
# many of these possibilities were associated with the event. The resulting
# probability of choosing a blue bead is 3/5 because out of the five possible
# outcomes, three were blue.

# For more complicated cases the computations are not as straightforward. For
# instance, what is the probability that if I draw five cards without
# replacement, I get all cards of the same suit, what is known as a “flush”" in
# Poker? In a Discrete Probability course you learn theory on how to make these
# computations. Here we focus on how to use R code to compute the answers.

# First let’s construct a deck of cards. For this we will use the expand.grid
# and paste functions. We use paste to create strings by joining smaller
# strings. To do this, we take the number and suit of a card and create the card
# name like this:

number <- "Three"
suit <- "Hearts"
paste(number, suit)
#> [1] "Three Hearts"

# paste also works on pairs of vectors performing the operation element-wise:

paste(letters[1:5], as.character(1:5))
#> [1] "a 1" "b 2" "c 3" "d 4" "e 5"

# The function expand.grid gives us all the combinations of entries of two
# vectors. For example, if you have blue and black pants and white, grey and
# plaid shirts, all your combinations are:

expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))
#>   pants shirt
#> 1  blue white
#> 2 black white
#> 3  blue  grey
#> 4 black  grey
#> 5  blue plaid
#> 6 black plaid

# So here is how we generate a deck of cards:

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)

# With the deck constructed, we can now double check that the probability of a
# King in the first card is 1/13. We simply compute the proportion of possible
# outcomes that satisfy our condition:

kings <- paste("King", suits)
mean(deck %in% kings)
#> [1] 0.0769

# which is 1/13.

# Now, how about the conditional probability of the second card being a King
# given that the first was a King? Earlier we deduced that if one King is
# already out of the deck and there are 51 left then this probability is 3/51.
# Let’s confirm by listing out all possible outcomes.

# To do this, we can use the permutations function from the gtools package. This
# function computes, for any list of size n, all the different combinations we
# can get when we select r items. So here are all the ways we can choose two
# numbers from a list consisting of 1,2,3:

library(gtools)
permutations(3, 2)
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    3
#> [3,]    2    1
#> [4,]    2    3
#> [5,]    3    1
#> [6,]    3    2

# Notice that the order matters here: 3,1 is different than 1,3. Also, note that
# (1,1), (2,2) and (3,3) do not appear because once we pick a number, it can’t
# appear again.

# Optionally, we can add a vector. So if you want to see five random seven digit
# phone numbers out of all possible phone numbers, you can type:

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

# Instead of using the numbers 1 through 10, the default, it uses what we
# provided through v: the digits 0 through 9.

# To compute all possible ways we can choose two cards when the order matters,
# we type:

hands <- permutations(52, 2, v = deck)

# This is a matrix with two columns and 2652 rows. With a matrix we can get the
# first and second card like this:

first_card <- hands[,1]
second_card <- hands[,2]

# Now the cases for which the first hand was a King can be computed like this:

kings <- paste("King", suits)
sum(first_card %in% kings)
#> [1] 204

# To get the conditional probability we compute what fraction of these have a
# King in the second card:

sum(first_card %in% kings & second_card %in% kings) /
     sum(first_card %in% kings)
#> [1] 0.0588

# which is exactly 3/51 as we had already deduced. Notice that the code above is
# equivalent to:

mean(first_card %in% kings & second_card %in% kings) /
     mean(first_card %in% kings)
#> [1] 0.0588

# which uses mean instead of sum and is an R version of:

# Pr(A and B)Pr(A)Pr(A and B)Pr(A)

# How about if the order doesn’t matter? For example, in Blackjack if you get an
# Ace and a face card in the first draw, it is called a Natural 21 and you win
# automatically. If we wanted to compute the probability of this happening, we
# would enumerate the combinations, not the permutations, since the order does
# not matter. Below are the differences:

permutations(3,2)
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    3
#> [3,]    2    1
#> [4,]    2    3
#> [5,]    3    1
#> [6,]    3    2

combinations(3,2)
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    3
#> [3,]    2    3

# In the second line the outcome does not include (2,1) because the (1,2)
# already was enumerated. The same applies to (3,1) and (3,2).

# So to compute the probability of a Natural 21 in Blackjack, we can do this:

aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid( number=facecard, suit=suits)
facecard <- paste( facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
#> [1] 0.0483

# In the last line we assume the Ace comes first. This is only because we know
# the way combinationgenerates enumerates possibilities and it will list this
# case first. But to be safe we could have written this and produced the same
# answer:

mean((hands[,1] %in% aces & hands[,2] %in% facecard) |
          (hands[,2] %in% aces & hands[,1] %in% facecard))
#> [1] 0.0483

# Monte Carlo example

# Instead of using combinations to deduce the exact probability of a Natural 21,
# we can use a Monte Carlo to estimate this probability. In this case, we draw
# two cards over and over and keep track of how many 21s we get. We can use the
# function sample to draw to cards without replacements:

hand <- sample(deck, 2)
hand
#> [1] "Eight Spades" "Ten Spades"

# And then check if one card is an Ace and the other a face card or a 10. Going
# forward, we include 10 when we say face card. Now we need t0 check both
# possibilities:

(hands[1] %in% aces & hands[2] %in% facecard) | 
     (hands[2] %in% aces & hands[1] %in% facecard)
#> [1] FALSE

# If we repeat this 10,000 times, we get a very good approximation of the
# probability of a Natural 21.

# Let’s start by writing a function that draws a hand and returns TRUE if we get
# a 21. The function does not need any arguments because it uses objects defined
# in the global environment.

blackjack <- function(){
     hand <- sample(deck, 2)
     (hand[1] %in% aces & hand[2] %in% facecard) | 
          (hand[2] %in% aces & hand[1] %in% facecard)
}

# Here we do have to check both possibilities: Ace first or Ace second because
# we are not using the combinations function. The function returns TRUE if we
# get a 21 and FALSE otherwise:

blackjack()
#> [1] FALSE

# Now we can play this game, say, 10,000 times:

B <- 10000
results <- replicate(B, blackjack())
mean(results)
#> [1] 0.0488

# 26.10 Birthday problem

# Suppose you are in a classroom with 50 people. If we assume this is a randomly
# selected group of 50 people, what is the chance that at least two people have
# the same birthday? Although it is somewhat advanced, we can deduce this
# mathematically. We will do this later. Here we use a Monte Carlo simulation.
# For simplicity, we assume nobody was born on February 29. This actually
# doesn’t change the answer much.

# First, note that birthdays can be represented as numbers between 1 and 365, so
# a sample of 50 birthdays can be obtained like this:

n <- 50
bdays <- sample(1:365, n, replace = TRUE)

# To check if in this particular set of 50 people we have at least two with the
# same birthday, we can use the function duplicated which returns TRUE whenever
# an element of a vector is a duplicate. Here is an example:

duplicated(c(1,2,3,1,4,3,5))
#> [1] FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE

# The second time 1 and 3 appear we get a TRUE. So to check if two birthdays
# were the same we simply use the any and duplicated functions like this:

any(duplicated(bdays))
#> [1] TRUE

# In this case, we see that it did happen. At least two people had the same
# birthday.

# To estimate the probability, we repeat this experiment by sampling 50
# birthdays, over and over:

same_birthday <- function(n){
     bdays <- sample(1:365, n, replace=TRUE)
     any(duplicated(bdays))
}

B <- 10000
results <- replicate(B, same_birthday(50))
mean(results)
#> [1] 0.97

# Where you expecting the probability to be this high?

# People tend to underestimate these probabilities. To get an intuition as to
# why it is so high, think about what happens when the group is close to 365. At
# this stage, we run out of days and the probability is one.

# Say we want to use this knowledge to bet with friends about two people having
# the same birthday in a group of people. When are the chances larger than 50%?
# Larger than 75%?

# Let’s create a look-up table. We can quickly create a function to compute this
# for any group size:

compute_prob <- function(n, B=10000){
     results <- replicate(B, same_birthday(n))
     mean(results)
}

# Using the function sapply, we can perform element-wise operations on any
# function:

n <- seq(1,60)
prob <- sapply(n, compute_prob)

# We can now make a plot of the estimated probabilities of two people having the
# same birthday in a group of size nn:

prob <- sapply(n, compute_prob)
qplot(n, prob)

# Now let’s compute the exact probabilities rather than use Monte Carlo
# approximations. Not only do we get the exact answer using math, but the
# computations are much faster since we don’t have to generate experiments.

# To make the math simpler, instead of computing the probability of it
# happening, we will compute the probability of it not happening. For this we
# use the multiplication rule.

# Let’s start with the first person. The probability that person 1 has a unique
# birthday is 1. The probability that person 2 has a unique birthday, given that
# person 1 already took one, is 364/365. Then, given that the first two people
# have unique birthdays, person 3 is left with 363 days to choose from. We
# continue this way and find the chances of all 50 people having a unique
# birthday is:

# 1×364365×363365…365−n+13651×364365×363365…365−n+1365

# We can write a function that does this for any number:

exact_prob <- function(n){
     prob_unique <- seq(365,365-n+1)/365 
     1 - prod( prob_unique)
}
eprob <- sapply(n, exact_prob)

qplot(n, prob) + 
     geom_line(aes(n, eprob), col = "red")

# This plot shows that the Monte Carlo simulation provided a very good estimate
# of the exact probability. Had it not been possible to compute the exact
# probabilities, we would have still been able to accurately estimate the
# probabilities.

# 26.11 How many Monte Carlo experiments are enough

# In the examples above, we used $B=$10,000 Monte Carlo experiments. It turns
# out that this provided very accurate estimates. But in more complex
# calculations, 10,000 may not nearly enough. Also, for some calculations,
# 10,000 experiments might not be computationally feasible. In practice, we
# won’t know what the answer is so we won’t know if our Monte Carlo estimate is
# accurate. We know that the larger BB, the better the approximation. But how
# big do we need it to be? This is actually a challenging questions and
# answering it often requires advanced theoretical statistics training.

# One practical approach we will describe here is to check for the stability of
# the estimate. Here is an example with the birthday problem for a group of 22
# people.

B <- 10^seq(1, 5, len = 100)
compute_prob <- function(B, n=25){
     same_day <- replicate(B, same_birthday(n))
     mean(same_day)
}
prob <- sapply(B, compute_prob)
qplot(log10(B), prob, geom = "line")

# In this plot, we can see that the values start to stabilize, that is, they
# vary less than .01, around 1000. Note that the exact probability, which we
# know in this case, is 0.569.

# 26.12 Addition rule

# Another way to compute the probability of a Natural 21 is to notice that it is
# the probability of an Ace followed by face card or a face card followed by an
# Ace. Here we use the addition rule:

# Pr(A or B)=Pr(A)+Pr(B)−Pr(A and B)Pr(A or B)=Pr(A)+Pr(B)−Pr(A and B)

# This rule is intuitive: think of a Venn diagram. If we simply add the
# probabilities, we count the intersection twice.

#> Loading required package: grid
#> Loading required package: futile.logger
#> 
#> Attaching package: 'futile.logger'
#> The following object is masked from 'package:gtools':
#> 
#>     scat
#> (polygon[GRID.polygon.13166], polygon[GRID.polygon.13167], polygon[GRID.polygon.13168], polygon[GRID.polygon.13169], text[GRID.text.13170], text[GRID.text.13171], text[GRID.text.13172], text[GRID.text.13173], text[GRID.text.13174])

# In the case of a Natural 21, the intersection is empty since both hands can’t
# happen simultaneously. The probability of an Ace followed by a face card is
# 1/13×16/511/13×16/51 and the probability of a face card followed by an Ace is
# 16/52×4/5116/52×4/51. These two are actually the same, which makes sense due
# to symmetry. In any case, we get the same result using the addition rule:

1/13*16/51 + 16/52*4/51 + 0
#> [1] 0.0483

# 26.13 Monty Hall problem

# In the 1970s there was a game show called “Let’s Make a Deal”. Monty Hall was
# the host. At some point in the game, contestants were asked to pick one of
# three doors. Behind one door there was a prize. The other doors had a goat
# behind them to show the contestant they had lost. If the contestant did not
# pick the prize door on his or her first try, Monty Hall would open one of the
# two remaining doors and show the contestant there was no prize. Then he would
# ask “Do you want to switch doors?” What would you do?

# We can use probability to show that if you stick with the original door
# choice, your chances of winning a prize remain 1 in 3. However, if you switch
# to the other door, your chances of winning double to 2 in 3! This seems
# counter intuitive. Many people incorrectly think both chances are 1 in 2 since
# you are choosing between 2 options. You can watch a detailed explanation here
# or read one here. Below we use a Monte Carlo simulation to see which strategy
# is better. Note that this code is written longer than it should be for
# pedagogical purposes.

# Let’s start with the stick strategy:

B <- 10000
stick <- replicate(B, {
     doors <- as.character(1:3)
     prize <- sample(c("car","goat","goat"))
     prize_door <- doors[prize == "car"]
     my_pick  <- sample(doors, 1)
     show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
     stick <- my_pick
     stick == prize_door
})
mean(stick)
#> [1] 0.336

# As we write the code, we note that the lines starting with my_pick and show
# have no influence on the last logical operation. From this we should realize
# that the chance is 1 in 3, what we began with.

# Now let’s repeat the exercise, but consider the switch strategy:

switch <- replicate(B, {
     doors <- as.character(1:3)
     prize <- sample(c("car","goat","goat"))
     prize_door <- doors[prize == "car"]
     my_pick  <- sample(doors, 1)
     show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
     stick <- my_pick
     switch <- doors[!doors%in%c(my_pick, show)]
     switch == prize_door
})
mean(switch)
#> [1] 0.672

# The Monte Carlo estimate confirms the 2/3 calculation. The helps us gain some
# insight by showing that we are removing a door, show, that is definitely not a
# winner from our choices. We also see that unless we get it right when we first
# pick, you win: 1 - 1/3 = 2/3.









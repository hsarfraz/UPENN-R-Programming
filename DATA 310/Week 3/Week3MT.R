#### Data 301 Synchronous Session
##January 27, 2022
## M. Trussler

setwd("C:/Users/trussler-adm/Dropbox (PORES)/PORES/DATA Certificate/DATA 310 (201)/Week 3/9. Synchronous Session")


#Why all this fuss about the sampling distribution? 
#In short, the sampling distribution is at the heart of everything that is
#to come in statistics. It really underlies everything. No lie, theoretically
#this is likely the most important thing to learn in this class.


  
#Consider what we get in the real world:

#(Pretend we can't see this population)
set.seed(21051988)
pop <- c(runif(100000, min=-10,max=10), rnorm(10000, mean=6, sd=.002))
plot(density(pop))

#In the real world we get a sample from some population

samp <- sample(pop, 100)
samp

#That will have some mean and variance
mean(samp)
var(samp)
sd(samp)


#We know some things
#(1) There exists out there a "truth", the population, which is unknowable
#(2) If we sampled again and again and again we would get different answers each time

#It is critical for us to answer:
#(a) What is the relationship between OUR sample mean and the population mean?
#(b) What does the distribution of all possible sample means look like? (the sampling distribution)

#In statistics we have good answers to both of these questions. The sample mean equals the population
#mean (in expectation!), and the sampling distribution will be normally distributed with a shape
#that we can estimate given one sample. That second part we'll derive next week, but what's important
#now is that it's critical to understand sampling distributions.

#The reason that we spent so much time last week working with the normal distribution is that
#the sampling distribution for everything we care about will be normally distributed.

#Note that the "estimate" we are discussing here is the sample mean because that is the easiest 
#to understand, but every statistic we calculate (correlation, regression coefficient, factor loading)
#has the same logic: there exists a theoretical distribution of all possible answers that we would get if
#we sampled again and again, and that distribution can be estimated.


######Motivating Example#######

#Look, we're all tired of flipping coins, but I want to give one more example of what we are 
#talking about here.

#Consider flipping 6 coins
#Our "estimate" will be the number of heads we get on each flip
#One "sample" is a flip of 6 coins
#The "sampling distribution" is the probability of each number of 
#heads 0-6.

#In this case, how can we estimate a sampling distribution?

#There are two ways: simulation and theory.

#Let's start with the theory method
#Coin flipping is something we understand, and we
#can use dbinom() to estimate what the probability of
#each number of successes is in 6 bernoulli trials

#So for 0:
dbinom(0,size=6, prob=.5)

#And we can estimate for each number of heads

dbinom(seq(0,6),size=6, prob=.5)

#Let's plot that simply:

plot(seq(0,6), dbinom(seq(0,6),size=6, prob=.5), pch=16, col="firebrick",
     xlab="Number of Heads", ylab="Probability",
     main="Sampling Distribution for 6 Coin Flips")

#Again, because coins are simple, and because we theoretically understand them, we can easily
#calculate what would happen if we flipped 6 coins an infinite amount of times.


#Let's do the same thing with a simulation:

runs <- 10000
num.heads <- rep(NA, length(runs))

for(i in 1:runs){
  samp <- sample(c(0,1), 6, replace=T)
  num.heads[i] <- sum(samp)
}


#Here I simply sampling 6 "coins" which is just a vector with (0,1) 10000 times and recording
#how many heads come up in each flip of 6 coins.

#Here is the resulting distribution:
prop.table(table(num.heads))

#Let's see how this compares:

plot(seq(0,6), dbinom(seq(0,6),size=6, prob=.5), pch=16, col="firebrick",
     xlab="Number of Heads", ylab="Probability", main="Sampling Distribution for 6 Coin Flips")
points(seq(0,6), prop.table(table(num.heads)), pch=16, col="darkblue")
legend("topright",c("Theory","Sample"),pch=c(16,16), col=c("firebrick","darkblue"))

#The two answers give us an (approximately) similar sampling distribution.
#Which shows us that both theory and sampling can help us estimate the sampling distribution.


#################
#Back to the (more) real world

#Let's consider our real world example again where we are sampling from some population:
set.seed(21051988)
pop <- c(runif(100000, min=-10,max=10), rnorm(10000, mean=6, sd=.002))
plot(density(pop))

#In the real world we get a sample from some population

samp <- sample(pop, 100)
samp

#That will have some mean and variance
mean(samp)
var(samp)
sd(samp)

#Again, there are two ways to understand the sampling distribution of this sample mean: simulation and theory.

#This time, let's start with simulation

runs <- 10000
sample.mean <- rep(NA, length(runs))

for(i in 1:runs){
  samp <- sample(pop, size=100, replace=T)
  sample.mean[i] <- mean(samp)
}

plot(density(sample.mean), col="darkblue", main="Sampling Distribution of Sample Mean", 
     xlab="Sample Mean", ylab="Density")

#This is an approximation of the sampling distribution: it is the distribution of all possible means
#that we could get if we sampled again, and again, and again.


##
#How do we create a sampling distribution for this variable with theory??
#In the above demonstration with coins, we know how coins work and can just use dbinom()
#But in the real world our underlying distributions are much more messy.
#I mean look at this one!
plot(density(pop))

#Some sort of weird mix of uniform and normal. Nothing like a coin....

#The key insight to this week is that the sampling distribution
#of the mean for *any underlying distribution*
#will be normally distributed with mean=population mean
#and variance equal to population variance/sample size

#In other words, because of statistical magic (really the LLN and the CLT), we can use theory to understand the sampling
#distribution of the mean of our weird population in the same way we can theoretically
#understand the sampling distribution of coin flips.

#One quick terminology note: The standard error is the standard deviation of the sampling distribution.
#Drill that sentence into your head!

#Why is this the case? 

#It's due to the Law of Large Numbers and the Central Limit Theorem

#The law of large numbers says that as the sample size increases the sample mean
#will converge on the true population mean, and quite quickly!

#Here is the creation of a figure that shows this

#You can uncomment this code and run it, but note that it takes about an hour....
sample.sizes <- seq(3,1000,1)
runs <- 1000
range.sample.means <- matrix(NA, nrow=length(sample.sizes),ncol=2)
#
#for(j in 1:length(sample.sizes)){
#  sample.means <- rep(NA, length(runs))
#  for(i in 1:runs){
#    samp <- sample(pop, size=sample.sizes[j])
#    sample.means[i] <- mean(samp)
#  }
#  range.sample.means[j,] <- range(sample.means) 
#}
#save(range.sample.means,file="LNNestimate.Rdata")
load("LNNestimate.Rdata")


plot(range(sample.sizes), c(-10,10), type="n", xlab="Sample Size", ylab="Range of Sample means from 1000 simulations")
segments(sample.sizes, range.sample.means[,1], sample.sizes, range.sample.means[,2], col="firebrick")
abline(h=mean(pop), lty=2, lwd=3)

#What do we see here? 

#First, each of the red bars is centered on the black dotted line, which is the population mean. 
#While in each sample we'll get something different, in *expectation* we will return the population mean.

#Second, We can see that as the sample size increases, the likelihood that any particular sample mean will be close
#to the population mean increases. The range of answers gets smaller and smaller as we add more data.
#Note, however, that we actually get quite quickly to a thin band, and after about 300 we don't get much better....

#Again, this is what we mean by the law of large numbers.


#What about the central limit theorem? This states that as sample size increases the sampling distribution of the mean becomes more and
#more normally distributed.

#Theoretically, we've been told that the sampling distribution of the mean will be normally distributed
#With mean=population mean and se = standard deviation of the population/sqrt(sample size)


#So the next thing we're going to do is to estimate sampling distributions via both simulation
#and theory at different sample sizes:

sample.sizes <- c(3,5,10,20,30,50,100,1000)
runs <- 1000

pdf(file="CentralLimitTheory.pdf", height=11, width=8.5)
par(mfrow=c(4,2))
for(j in 1:length(sample.sizes)){
  sample.means <- rep(NA, length(runs))
  for(i in 1:runs){
    samp <- sample(pop, size=sample.sizes[j])
    sample.means[i] <- mean(samp)
  }
plot(density(sample.means), col="darkblue", main=paste("n =", sample.sizes[j]), xlab="Sample Mean", ylab="Density", lwd=2)
lines(seq(-10,10,.001),dnorm(seq(-10,10,.001), mean=mean(pop), sd=sqrt(var(pop)/sample.sizes[j])), col="firebrick", lwd=2)
}
dev.off()

#We can look at the resulting document to see the degree to which the sampling distribution we arrived at theoretically
#Approximates what we get from simulation. 

#In this case.... actually all of them look pretty good (Which is good news!). But just know that the ones
#that are under 30 are actually not well fitted in a systematic way that matters. And as sample size increases
#The difference between what we estimate from theory and what we estimate from simulation converge better and better.


### So what!?
#Again, let's consider our original sample: 
#
set.seed(21051988)
pop <- c(runif(100000, min=-10,max=10), rnorm(10000, mean=6, sd=.002))
plot(density(pop))

#In the real world we get a sample from some population

samp <- sample(pop, 100)
samp

#SIDEBAR: What is this IID business???

#These things work when our sample is "independent and identically distributed".
#I just remember this like a roulette wheel.

#When samples are *independent* what you get on one draw doesn't affect what you
#get on the next draw.

#A roulette wheel doesn't care what you got last time, or the last 10 times,
#Each time it spins each number has an even probability (including what you got last time).

#When samples are *identically distributed* you are drawing from the same population
#each time. Each spin of the roulette wheel we are spinning the same wheel, the same pop.


#That will have some mean and variance
mean(samp)
var(samp)
sd(samp)

#What is the sampling distribution for the sample mean for this underlying population?
#We can get there via simulation:

runs <- 10000
sample.mean <- rep(NA, length(runs))

for(i in 1:runs){
  samp <- sample(pop, size=100)
  sample.mean[i] <- mean(samp)
}

plot(density(sample.mean), col="darkblue", main="Sampling Distribution of Sample Mean", 
     xlab="Sample Mean", ylab="Density")

#But because of the LLN and the CLT, we can also just get there via what we know about the 
#sampling distribution theoretically:

#The critical thing that we have learned is that the sampling distribution will be normally distributed
#with a mean equal to the population mean, and a variance equal to the population
#variance divided by the population size. 

#That is, everything that we learned last week about normal distributions apply to the sampling distribution
#but for the sampling distribution the variance/standard deviation is determined by the population we are sampling
#from and the size of our sample.

#The standard error (the standard deviation of the sampling distribution) in this case is

se <- sqrt(var(pop)/100)
#OR
se <- sd(pop)/sqrt(100)

lines(seq(-5,5,.001), dnorm(seq(-5,5,.001), mean=mean(pop), sd=se), col="firebrick")


#The two distributions are effectively the same.

#This means that we can use all the tools that we learned about normal distributions to 
#investigate what is likely to happen if we draw a new sample and calculate a new mean

#What is the probability of obtaining a mean that is less than 0?
se <- sqrt(var(pop)/100)
pnorm(0, mean(pop), sd=se)


#And if we want to calculate a 95% symmetrical CI around the mean?

#From the simulation:
quantile(sample.mean, probs=c(.025,.975))

#From the theory:
qnorm(p=c(.025,.975), mean=mean(pop), sd=se)

#Ehhhhh, close enough!

#The astute among you may be thinking: Ok fine Dr. Trussler, that's all good, but the whole point is 
#that we don't know the population mean and variance!

#Absolutely! And that's what we're going to learn next, that we can actually estimate
#the population variance very well from just *one sample*.

#One more piece of practice:

#What is the probability of obtaining a mean less than 0 if the population has the mean and variance above,
#but we take a sample of 1000 instead?

se <- sqrt(var(pop)/1000)
pnorm(0, mean(pop), sd=se)



################
#An addition, if there's time!

#It's worth pausing to think about the magic of the standard error formula.
#Remember, the standard error is the standard deviation of the sampling distribution.

#sd(pop)/sqrt(n)

#What does this mean? There are two ingredients for how spread out a sampling distribution is
#The underlying randomness of the population, and the sample size.

#As the underlying randomness of the population (the numerator) decreases the sampling distribution
#gets tighter. 

#As the number of people sampled gets higher (the denominator) increases the sampling distribution
#gets tighter

#The following creates pdfs which show both of these features:

#Hold constant the sample size and decrease the variance

sample.size <- 100
pop.sd <- seq(10,1,-1)

pdf(file="SDistDecreasingVar.pdf", height=20, width=20)
par(mfrow=c(5,2))
for(i in 1:length(pop.sd)){
plot(seq(-10,10,.001), dnorm(seq(-10,10,.001), mean=0, sd=pop.sd[i]/sqrt(sample.size)), type="l",
     xlim=c(-3,3), ylim=c(0,2), main=paste("Population SD = ", pop.sd[i], "; Sample Size = 100"))
}
dev.off()


#Hold constant the sample size and decrease the variance

sample.size <- seq(100,1000,100)
pop.sd <- 4

pdf(file="SDistIncreasingN.pdf", height=20, width=20)
par(mfrow=c(5,2))
for(i in 1:length(sample.size)){
  plot(seq(-10,10,.001), dnorm(seq(-10,10,.001), mean=0, sd=pop.sd/sqrt(sample.size[i])), type="l",
       xlim=c(-1,1), ylim=c(0,3), main=paste("Population SD = 4", "; Sample Size = ", sample.size[i]))
}
dev.off()

#Note that because of the squares and square roots, the improvements to "efficiency" 
#(our fancy term for the width of the sampling distribution) increase at a decreasing rate
#Improving the variance from 10-8 matters, but not as much as improving from 12-10
#Increasing the sample from 200-300 matters, but not as much as improving from 100-200...

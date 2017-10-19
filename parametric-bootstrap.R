
# Parametric Bootstrap Approach -------------------------------------------

# Assume data com from random variables from a particular distribution.
# Use data to estimate the parameters of the distribution.
# Use a random number generator to create samples from the pattern distribution with the same size as original.
# Calculate the sample mean or the statitstics of interest. Confidence intervals and so also.



# Example: Sample  of 15 individuals, 3 of them sick. ---------------------

prevalence <- 3/15

# We simulate samples of size 15 coming from a Bernoulli distribution with p= prevalence and we count the number of successes.
pr <- rbinom(1000, 15, prevalence)/15

# Bootstrap estimate
p <- mean(pr)

# Expected Bias
mean(pr)-3/15

# Estimated Standdard error
sd(pr)

# A confidence interval of 95% comes from a 1.96SE (standard error) -> p +- 1.96SE


hist(pr)
quantile(pr, prob=c(0.025, 0.975))


# Example: Capture Re-capture fishes --------------------------------------

# 17 fishes, mark 5, recapture 8, m marked from recaptured
N <- 17 # Estimated total fishes
n1 <- 50 # Marked fishes
n2 <- 80 # Recaptured fishes

N <- ((n1+1)*(n2+1)/(m+1))-1
N

x <- rhyper(1000, 5, N-n1, n2) # Hypergeometric distribution

npop <- ((n1+1)*(n2+1)/(x+1))-1 # Chapman estimator
mean(npop)
1.96*sd(npop)
quantile(npop, probs=c(0.025, 0.975))

vector <- c()
mvector <- c()
m<- 1
for(m in 1:n1){
	N <- ((n1+1)*(n2+1)/(m+1))-1
	x <- rhyper(1000, n1, N-n1, n2)
	npop <- ((n1+1)*(n2+1)/(x+1))-1
	vector[m] <- N/mean(npop)-1
	mvector[m] <- m
}

ggplot()+
	geom_point(aes(y=vector, x=mvector)) +
	geom_hline(yintercept = 0)

ggplot()+
	geom_histogram(aes(vector))


x <- 1-rpois(1000000,3.15)+9
hist(x)
mean(x)
sd(x)


# Real Example: Capture Re-capture almonds --------------------------------------

# 17 fishes, mark 10, recapture 20, m marked from recaptured

n1 <- 10 # Marked almonds
n2 <- 20 # Recaptured almonds
m <- 1

N <- ((n1+1)*(n2+1)/(m+1))-1
N

x <- rhyper(1000, 5, N-n1, n2) # Hypergeometric distribution

npop <- ((n1+1)*(n2+1)/(x+1))-1 # Chapman estimator
mean(npop)
1.96*sd(npop)
quantile(npop, probs=c(0.025, 0.975))

vector <- c()
mvector <- c()
m<- 1
for(m in 1:n1){
	N <- ((n1+1)*(n2+1)/(m+1))-1
	x <- rhyper(1000, n1, N-n1, n2)
	npop <- ((n1+1)*(n2+1)/(x+1))-1
	vector[m] <- N/mean(npop)-1
	mvector[m] <- m
}

real <- 133

N
mean(npop)
real


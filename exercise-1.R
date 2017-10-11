
# Data Visualization: Exercise 1 ------------------------------------------
# Author: Alejandro Jiménez  Rico

library('combinat')
library(ggplot2)



# Volume of Air Experiment ------------------------------------------------


# Take a look at the data
x <- c(39,29, 60, 40, 32)
y <- c(11, 5, 20, 8, 6)
spearson <- c()
sim.spear <- c()
df <- as.data.frame(x)
df$y <- y
df$dx <- 0
df$dy <- 0
alpha <- 0.05
ggplot() +
	geom_point(aes(x=x, y=y))

# Compute Pearson Coefficient
cov <- cov(x,y)
sdx <- sd(x)
sdy <- sd(y)
pearson <- cov/(sdx*sdy)

# Clear variables
rm(cov,sdx,sdy)

# Ranking values
for(i in 1:length(x)){
	a <- sort(x, decreasing = TRUE)[i]
	df[df$x == a, ]$dx <- i
	a <- sort(y, decreasing = TRUE)[i]
	df[df$y == a, ]$dy <- i
}

# Compute difference of ranks
df$diff <- df$dx - df$dy
df$diff2 <- df$diff^2

# Compute Spearman Coefficient
t <- table(df$diff)
t <- t[names(t) == 0]
t <- as.data.frame(t)
n <- length(df$diff2) - t$t
n <- length(df$diff2)
spearman <- 1 - 6*(sum(df$diff2))/(n^3-n)

# Clear variables
rm(a,i,t,n)

# Permutation test
cnt <- 0
cnt2 <- 0
w <- permn(y)
for (i in 1:length(w)){
	y <- as.vector(w[[i]])

	# Compute Pearson Coefficient from randomized data
	cov <- cov(x,y)
	sdx <- sd(x)
	sdy <- sd(y)
	spearson[i] <- cov/(sdx*sdy)
	if (spearson[i] > pearson){cnt = cnt + 1}
	
	sspearman <- cor(x,y, method='spearman')
	sim.spear[i] <- sspearman
	if (sim.spear[i] > spearman){cnt2 = cnt2+1}
}
pvalue1 <- cnt/length(w)
pvalue2 <- cnt2/length(w)

pearson.hist <- ggplot() +
	geom_histogram(aes(x=spearson), binwidth = 0.09, colour='black', fill='white') +
	geom_vline(aes(xintercept=quantile(1-pvalue1), colour='p-value'),  size=1, linetype=1, show.legend=TRUE) +
	geom_vline(aes(xintercept=quantile(1-alpha), colour='alpha'), size=1, linetype=2, show.legend=TRUE) +
	scale_colour_manual(values=c('red','blue')) +
	ggtitle('Pearson Coefficient') +
	theme(plot.title = element_text(hjust = 0.5))

spearman.hist <- ggplot() +
	geom_histogram(aes(x=sim.spear), binwidth = 0.2, colour='black', fill='white') +
	geom_vline(aes(xintercept=quantile(1-pvalue2), colour='p-value'),  size=1, linetype=1, show.legend=TRUE) +
	geom_vline(aes(xintercept=quantile(1-alpha), colour='alpha'), size=1, linetype=2, show.legend=TRUE) +
	scale_colour_manual(values=c('red','blue')) +
	ggtitle('Spearman Coefficient') +
	theme(plot.title = element_text(hjust = 0.5))

pearson.hist
spearman.hist

rm(list=ls(all=TRUE))


# Weight Experiment -------------------------------------------------------

nr <- 100000 # Number of rearrangements
st <- numeric(nr)
standard <- c(2.5,3.4,2.9,4.1,5.3,3.4,1.9,3.3,1.8)
additive <- c(3.5,6.3,4.2,4.3,3.8,5.7,4.4)
n1 <- length(standard)
n2 <- length(additive)
total <- n1+n2
alpha <- 0.05

m.st <- mean(standard)
m.add <- mean(additive)

sttrue <- m.add - m.st
cnt <- 0
# Put both sets of observations in a single vector
vect <- c(standard, additive)

# Perform permutations and compare means
for (i in 1:nr){
	d <- sample(vect, total)
	s.st<- d[1:n1]
	a <- n1+1
	s.add <- d[a:total]
	st[i] <- mean(s.add)-mean(s.st)
	if(st[i] > sttrue){cnt = cnt + 1}
}

pvalue <- cnt/nr

# Plot histogram
histogram <- ggplot() +
	geom_histogram(aes(x=st), binwidth = 0.1, colour='black', fill='white') +
	geom_vline(aes(xintercept=quantile(1-pvalue), colour='p-value'),  size=1, linetype=1, show.legend=TRUE) +
	geom_vline(aes(xintercept=quantile(1-alpha), colour='alpha'), size=1, linetype=2, show.legend=TRUE) +
	scale_colour_manual(values=c('red','blue')) +
	ggtitle('T-Test on mean difference') +
	theme(plot.title = element_text(hjust = 0.5))

histogram

sttrue <- median(additive) - median(standard)

# Perform permutation test comparing medians
for (i in 1:nr){
	d <- sample(vect, total)
	s.st<- d[1:n1]
	a <- n1+1
	s.add <- d[a:total]
	st[i] <- median(s.add)-median(s.st)
	if(st[i] > sttrue){cnt = cnt + 1}
}

pvalue <- cnt/nr

# Plot histogram
histogram <- ggplot() +
	geom_histogram(aes(x=st), binwidth = 0.1, colour='black', fill='white') +
	geom_vline(aes(xintercept=quantile(1-pvalue), colour='p-value'),  size=1, linetype=1, show.legend=TRUE) +
	geom_vline(aes(xintercept=quantile(1-alpha), colour='alpha'), size=1, linetype=2, show.legend=TRUE) +
	scale_colour_manual(values=c('red','blue')) +
	ggtitle('T-Test on mean difference') +
	theme(plot.title = element_text(hjust = 0.5))

histogram

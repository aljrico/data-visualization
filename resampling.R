####### WORK IN PROGRESS ######

# Resampling test --------------------------------------------------------

library(ggplot2)

conventional <- c(65, 79,90,75,61,85,98,80,97,75)
new <- c(90,98,73,79,84,81,98,90,83,88)
nr = 10000
st <- numeric(nr)
alpha <- 0.05

sttrue <- -mean(conventional)+ mean(new)
n1 <- length(new)
n2 <- length(conventional)
total <- n1+n2
cnt <- 0

vect = c(new,conventional)
for(i in 1:nr){
d = sample(vect,n1+n2)
ne <- d[1:n1]
a <- n1+1
co <- d[a:total]
st[i] <- mean(ne) - mean(co)
if (st[i] > sttrue) cnt = cnt+1
}

ggplot() +
  geom_histogram(aes(st), binwidth=1) +
  geom_vline(aes(xintercept=quantile(st,prob=1-alpha)), colour = 'red') +
  geom_vline(aes(xintercept=quantile(st,prob=1-(cnt/nr))), colour = 'blue') +
  labs(title = paste("Permutation test;", "p-value =", cnt/nr, "    Result:", ifelse(quantile(st,prob=1-(cnt/nr)) > quantile(st,prob=1-alpha) ,"YEA BOI!", "FAIL!")))
  


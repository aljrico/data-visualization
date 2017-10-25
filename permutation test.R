
# Permutation test --------------------------------------------------------

before = c(120,124,130,118,140,128,140,135,126,130,126,127)
after = c(128,131,131,127,132,125,141,137,118,132,129,135)

diff = before - after
nr = 5000 # Number of rearrangements to be examined
st = numeric(nr)
sttrue = mean(diff)
n = length(diff)
nf = 2^n
stat = numeric(n)
cnt = 0
alpha = 0.05

for (i in 1:nr){
  for(j in 1:n){stat[j] = ifelse(runif(1)<0.5, diff[j], -diff[j])}
  st[i]=mean(stat)
  if (st[i]<sttrue){cnt=cnt+1}
}
cnt/nr #p-value

ggplot() +
  geom_histogram(aes(st), binwidth=1) +
  geom_vline(aes(xintercept=quantile(st,prob=1-alpha)), colour = 'red') +
  geom_vline(aes(xintercept=quantile(st,prob=1-(cnt/nr))), colour = 'blue') +
  labs(title = paste("Permutation test;", "p-value =", cnt/nr, "    Result:", ifelse(quantile(st,prob=1-(cnt/nr)) > quantile(st,prob=1-alpha) ,"YEA BOI!", "FAIL!")))


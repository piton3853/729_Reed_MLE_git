Clinton <- c(18,31,34,33,27,33,28,23,33,12,19,25,14,4,22,7)
Trump <- c(11,22,27,29,24,29,25,26,38,14,23,31,20,6,34,12)
Y <- Clinton
N <- Clinton + Trump
# Calculate Vote Share by State
Clinton / N
# Calcuate Total Vote Share
sum(Y) / sum(N)
# Do it with MLE
bernoulli.lik <- function(p) {
  LL<-sum(Y*log(p)+(N-Y)*log(1-p))
}
#plot LL:
p.seq <- seq(0.01, 0.99, 0.01)
plot(p.seq, sapply(p.seq, bernoulli.lik), type="l")
#optimum:
optimize(bernoulli.lik, lower=0, upper=1, maximum=TRUE)


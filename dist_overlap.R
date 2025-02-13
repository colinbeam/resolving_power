#-----
# uniform_overlap
#
# Sample positive and negative class scores from overlapping uniform distributions
#
## Arguments
#
# auroc     distance between signal and noise distributions to give target AUROC
# n_ones    Number of positive cases
# n_zeros   Number of negative cases
#
# Notes: Shift parameter is derived from formula in Marzban (2004)
uniform_overlap <- function(auroc, n_ones, n_zeros) {
  
  # shift parameter
  shift <- 1 - sqrt(2 - 2*auroc)
  
  # sample from negative class
  neg_scores <- runif(n = n_zeros, min = 0, max = 1)
  
  # sample from positive class
  pos_scores <- runif(n = n_ones, min = 0 + shift, max = 1 + shift)
  
  # model output
  score <- c(pos_scores, neg_scores)
  outcome <- c(rep(1, n_ones), rep(0, n_zeros))
  
  output <- data.frame(outcome=outcome, score=score)
  output
  
}

#----
# gauss overlap
#
# Sample positive and negative class scores from overlapping uniform normal distributions.
#
## Arguments
#
# auroc     distance between signal and noise distributions to give target AUROC
# n_ones    number of positive cases
# n_zeros   number of negative cases

gauss_overlap <- function(auroc, n_ones, n_zeros) {
  
  # find shift parameter
  dprime <- sqrt(2)*qnorm(auroc)
  
  # sample from negative class
  neg_scores <- rnorm(n = n_zeros, mean = 0, sd = 1)
  
  # sample from positive class
  pos_scores <- rnorm(n = n_ones, mean = 0 + dprime, sd = 1)
  
  # model output
  score <- c(pos_scores, neg_scores)
  outcome <- c(rep(1, n_ones), rep(0, n_zeros))
  
  output <- data.frame(outcome=outcome, score=score)
  output
  
}


#----
# betamix_overlap
#
# Sample positive class scores from a beta mixture model and negative scores from a beta model
#
## Arguments
#
# nclass      negative class shape parameters
# pclass1     positive class shape parameters for first beta distribution
# pclass2     positive class shape parameters for second beta distribution
# mixer       weight given to first beta distribution. must be between 0 and 1.
# n_ones      number of positive cases
# n_zeros     number of negative cases

betamix_overlap <- function(nclass, pclass1, pclass2, mixer, n_ones, n_zeros) {
  
  neg_scores <- rbeta(n = n_zeros, shape1 = nclass[1], shape2 = nclass[2])
  
  n_ones_class1 <- round(mixer*n_ones); n_ones_class2 <- round((1-mixer)*n_ones)
  
  pos_scores1 <- rbeta(n = n_ones_class1, shape1 = pclass1[1], shape2 = pclass1[2])
  pos_scores2 <- rbeta(n = n_ones_class2, shape1 = pclass2[1], shape2 = pclass2[2])
  
  # model output
  score <- c(pos_scores1, pos_scores2, neg_scores)
  outcome <- c(rep(1, n_ones), rep(0, n_zeros))
  
  output <- data.frame(outcome=outcome, score=score)
  output
  
}
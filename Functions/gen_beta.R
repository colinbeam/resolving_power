#----
# gen_beta
#
# Generate scores from two classes of overlapping beta distributions
#
# For analytic AUC, choose mu and v to give integer products (i.e. use rational numbers)

gen_beta <- function(n_ones, n_zeros, mu1, v1, mu0, v0) {
  
  # positive class distribution
  a1 <- mu1*v1
  b1 <- (1-mu1)*v1
  scores_ones <- rbeta(n_ones, a1, b1)
  
  # negative class ditribution
  a0 <- mu0*v0
  b0 <- (1-mu0)*v0
  scores_zeros <- rbeta(n_zeros, a0, b0)
  
  outcome <- c(rep(0,times=n_zeros), rep(1,times=n_ones))
  score <- c(scores_zeros, scores_ones)
  
  model_data <- data.frame(outcome, score)
  
  ###
  # analytic ROC if whole number
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {  
    abs(x - round(x)) < tol }
  
  if(all(is.wholenumber(c(a1, b1, a0, b0)))) {
    
    auc <- 0
    
    for(k in 1:a1){
      add_auc <- 1/(a1+b1)*1/beta(a0,b0)*(beta(a0+a1-k,b0+b1+k-1)/beta(a1-k+1, b1+k))
      auc <- auc + add_auc
    } 
    
  } else {
    auc <- NA
  }
  
  
  out <- list(model_data=model_data, auc=auc)
  
}


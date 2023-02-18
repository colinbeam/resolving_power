#----
# coarsen_score
#
# Form robust scores by dividing scores into bins and assigning equal scores across bins
#
## arguments
#
# scores          vector of sorted scores
# percent_bin     percentage to include in each bin size
#
# Notes: Try setting percent_bin = min(prevalence/2, 0.01) for the smoothing parameter. Note that if positive and negative scores are randomly distributed in each bin within the population, then the estimator will be unbiased. However, if positives are higher than negatives, then estimator will be biased towards the random estimator.

coarsen_score <- function(scores, percent_bin) {
  
  score_rank <- rank(scores)
  nbins <- prop_bin*length(scores)
  score_bin <- cut(score_rank, breaks = nbins)
  coarsen_score <- as.numeric(score_bin)
  coarsen_score
  
}


#----
# coarsen_diagnostic

# apply AUROC to values in each of the bins

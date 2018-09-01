likRatioTest <- function(large,small)
  {
    ##----------------------------------------------------------------
    ## Perform a likelihood ratio test: lambda = lik(smallerModel)/lik(largerModel) ,
    ## where the smallerModel is submodel of the largerModel and lambda is chi2(f)
    ## distributed with f=dim(smallerModel)-dim(largerModel). Page 20 in Madsen2006.

    ## Calculate the logLikelihood for both models from their fit
    logLikSmallModel <- small$loglik
    logLikLargeModel <- large$loglik
    ## Calculate lambda
    chisqStat <- -2 * (logLikSmallModel - logLikLargeModel)
    ## It this gives a p-value smaller than confidence limit, i.e. 5\%, then the
    ## larger model is significant better than the smaller model
    prmDiff <- large$model$NPARAM - small$model$NPARAM
    ## The p-value of the test
    1 - pchisq(chisqStat, prmDiff)
    ##----------------------------------------------------------------
  }

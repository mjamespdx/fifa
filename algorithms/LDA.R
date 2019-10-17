LDA <- function(train, test, var)
{
  #for linear discriminant analysis
  #train is the training set
  #test is the test set
  #var is the variable to predict
  require(MASS)
  
  #class probability function
  LDAprobs <- function(data)
  {
    predProbs <- mapply(means, probs, FUN = function(x,y) 
      { t(x)%*%ginv(avCov)%*%t(data) - (1/2)*t(x)%*%ginv(avCov)%*%x + log(y) })
    return(which.max(predProbs)) #returns entry with highest probability
  }
  
  #internal variables
  test <- test[!names(test) %in% var]
  factorsInOrder <- unique(train[var])[order(unique(train[var])),] #for assigning class later
  byClass <- split(train,train[var]) #divide by variable values, ordered by ascending class
  prediction <- c()
  
  #create mean vectors for y classes, excluding variable to predict on
  means <- lapply(byClass, function(z) {colSums(z[!names(z) %in% var])/nrow(z)})
  
  #create probabilities for y classes
  probs <- lapply(byClass, function(z) {nrow(z)/nrow(train)})
  
  #create covariance matrices w/ added error
  covars <- lapply(byClass, FUN = function(z) 
  { cov(z[!names(z) %in% var]) + 10^-3*diag(nrow(cov(z[!names(z) %in% var]))) })
  
  #create average covariance
  covars <- mapply(byClass, covars, FUN = function(x,y)
    { (nrow(x) - 1)*y }, SIMPLIFY = F) #needs SIMPLIFY = F to preserve matrices
  avCov <- as.matrix(Reduce('+',covars)/(nrow(train)-length(byClass)))
  avCov <- avCov + 10^(-3)*diag(nrow(avCov)) #add noise to prevent inverse problems
  
  #make predictions
  test <- split(test, 1:nrow(test)) #split into list by rows for sapply
  prediction <- sapply(test, LDAprobs)
  prediction <- factorsInOrder[prediction]
  
  #return test data
  return(prediction)
}
QDA <- function(train, test, var)
{
  #for quadratic discriminant analysis
  #train is the training set
  #test is the test set
  #var is the variable to predict

  require(MASS)
  
  #class probability function
  QDAprobs <- function(data)
  {
    predProbs <- mapply(means, probs, covars, FUN = function(x,y,z)
      {(-1/2)*log(det(z)) - length(data)/2*log(2*pi) - 
        (1/2)*as.matrix(data - x)%*%ginv(z)%*%t(as.matrix(data - x)) + log(y)})
    return(which.max(predProbs)) #returns class with highest probability
  }
  
  #internal variables
  test <- test[!names(test) %in% var] #remove variable to predict from test set
  factorsInOrder <- unique(train[var])[order(unique(train[var])),] #order factors, ascending
  byClass <- split(train,train[var]) #split by variable values, ascending order
  prediction <- c()
  
  #create mean vectors for test classes, excluding variable to predict on
  means <- lapply(byClass, function(z) {colSums(z[!names(z) %in% var])/nrow(z)})
  
  #create probabilities for test classes
  probs <- lapply(byClass, function(z) {nrow(z)/nrow(train)})
  
  #create covariance matrices w/ added noise
  covars <- lapply(byClass, FUN = function(x) 
  { cov(x[!names(x) %in% var]) + 10^(-3)*diag(nrow(cov(x[!names(x) %in% var]))) })
  
  #make predictions
  test <- split(test, 1:nrow(test)) #split into list by rows for sapply
  prediction <- sapply(test, QDAprobs)
  prediction <- factorsInOrder[prediction]
  
  #return test data
  return(prediction)
}
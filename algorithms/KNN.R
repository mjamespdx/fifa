KNN <- function(train, test, var, k = 3)
{
  #THIS IS THE RIGHT ONE!
  #train is training set
  #test is testing set
  #var is the variable to predict, must be in 'var' format
  #k is k value, 3 default
  
  #remove variable to predict
  trainSub <- train[!names(train) %in% var] #need train to grab indices later
  test <- test[!names(test) %in% var]
  
  #for finding the mode of some data
  mode <- function(z)
  {
    uz <- unique(z)
    uz[which.max(tabulate(match(z, uz)))]
  }
  
  getPred <- function(data) #for getting predictions
  {
    #data is the data point
    distances <- apply(trainSub, 1, function(x) { sum((data - x)^2)})
    distances <- distances[order(distances, decreasing = F)]
    classes <- train[rownames(train) %in% names(distances[1:k]), names(train) %in% var] %>% 
      mode()
    return(classes)
  }
  
  prediction <- apply(test, 1, getPred) %>% as.vector() 
  
  return(prediction)
}
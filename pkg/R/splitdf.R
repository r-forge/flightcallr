splitdf <- function(
### This will randomly split a data frame into training and test sets. I borrowed it from
### http://gettinggeneticsdone.blogspot.com/2011/02/split-data-frame-into-testing-and.html
### The caret packages has a similar facility using its createDataPartition() function.
### Perhaps even more appealing, there is sample.split() in caTools.
dataframe, 
### this is the data frame you want to split
weight = 2/3, 
### the weights for the different classes
seed=NULL
### an optional seed
) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index) * weight))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
### A list with two members; the training set and the test set
}

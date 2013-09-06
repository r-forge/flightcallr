rejigger.perf = function (
### Rejigger a perf object from ROCR so that it is no longer sparse.
perf
### The perf object to be rejiggered.
) {
  alphas = c()
  n.fold  = length(perf@alpha.values)
  for (i in 1:n.fold) {
    alphas = c(alphas, perf@alpha.values[[i]])
  }
  alphas = sort(unique(alphas), decreasing=T)
  df = alphas
  df= rep(df, each=n.fold)
  df = data.frame(df)
  x.values = c()
  y.values = c()
  previous.valid.x = c()
  previous.valid.y = c()
  for (i in 1:length(alphas)) {
    valid.x = c()
    valid.y = c()
    for (j in 1:n.fold) {
      if (alphas[i] %in% perf@alpha.values[[j]]) {
        x.values = c(x.values, 
                     perf@x.values[[j]][which(perf@alpha.values[[j]] == alphas[i])])
        valid.x = c(valid.x, 
                    perf@x.values[[j]][which(perf@alpha.values[[j]] == alphas[i])])
        y.values = c(y.values, 
                     perf@y.values[[j]][which(perf@alpha.values[[j]] == alphas[i])])
        valid.y = c(valid.y, 
                    perf@y.values[[j]][which(perf@alpha.values[[j]] == alphas[i])])
      } else {
        x.values = c(x.values, previous.valid.x[j])
        y.values = c(y.values, previous.valid.y[j])
        valid.x = c(valid.x, previous.valid.x[j])
        valid.y = c(valid.y, previous.valid.y[j])
      }
    }
    previous.valid.x = valid.x
    previous.valid.y = valid.y
  }
  df$x.values = x.values
  df$y.values = y.values
  names(df) = c("alpha.values", "x.values", "y.values")
  return(df)
### The non-sparse representation of that perf object.
}

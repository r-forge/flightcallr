generate.forest.predictions = function(
### Generate the predictions for a selection table and a random forest model.
ubertable, 
### The selection table, with feature columns added by 
### generate.seewave.measures()
forest, 
### The random forest model.
perf=NULL, 
### A perf object, if bootstrapping has been done.
order.by = "percent"
### Whether to order the results by percent or rank within day.
) {
  require(randomForest)
  my.pred = predict(forest, ubertable, type="prob")
  ubertable$RF_Score = my.pred[,2]
  if (is.null(perf)) {
    cat("No perf object available; cannot make explicit predictions. Doing rankings only!\n")
  } else {
    cat("Calculating exact operating point\n")
    my.alpha = calculate.operating.parameters(perf=perf, method="frequency")$op
    ubertable$RF_Prediction = ifelse(my.pred[,2] <= my.alpha, "Call", "Noise")
  }
  ubertable2 = ubertable[0, ]
  ubertable2$RF_Rank_Within_Day = numeric()
  for (day in unique(ubertable$Begin.File)) {
    subtable = ubertable[ubertable$Begin.File == day, ]
    subtable = subtable[order(subtable$RF_Score), ]
    subtable$RF_Rank_Within_Day = 1:nrow(subtable)
    ubertable2 = rbind(ubertable2, subtable)
  }
  if(order.by == "percent") {
    ubertable2 = ubertable2[order(ubertable2$Random.Percent, ubertable2$Begin.File), ]
  } else if (order.by == "rank_within_day") {
    ubertable2 = ubertable2[order(ubertable2$RF_Rank_Within_Day), ]
  } else {
    stop(paste("unknown value for order.by:", order.by))
  }
  ubertable2
### The selection table, with columns added for RF_Score and RF_Rank_Within_Day
}

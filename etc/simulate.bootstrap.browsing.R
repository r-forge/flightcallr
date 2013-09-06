simulate.bootstrap.browsing = function(events = michigan, band="Sparrow") {
  require(randomForest)
  seewave.measures = c("Rugosity", "Crest_Factor", "Temporal_Entropy", "Shannon_Entropy", "Shannon_Entropy_Bandlimited", 
                       "Spectral_Flatness_Measure", "Spectral_Flatness_Measure_Bandlimited", "Spectrum_Roughness", 
                       "Spectrum_Roughness_Bandlimited", "Autocorrelation_Mean", "Autocorrelation_Median", 
                       "Autocorrelation_Standard_Error", "Dominant_Frequency_Mean", "Dominant_Frequency_Standard_Error", 
                       "Specprop_Mean", "Specprop_SD", "Specprop_SEM", "Specprop_Median", "Specprop_Mode", "Specprop_Q25", 
                       "Specprop_Q75", "Specprop_IQR", "Specprop_Cent", "Specprop_Skewness", "Specprop_Kurtosis", 
                       "Specprop_SFM", "Specprop_SH")
  prev.reject = F
  # NOTE: in previous events test runs we've worked on the whole dataset, thrush
  # as well as sparrow. Using just sparrow here to keep it similar to the NS
  # stuff.
  events = events[events$Detector == band, ]
  #events = events[ , !names(events) %in% c("Detector", "Detector_numeric")]
  events$Random.Percent = floor(100 * runif(dim(events)[1]) + 1)
  perf = list()
  fp.left = c()
  actual.fp.left=c()
  op = c()
  for (i in 1:100) {  # potentially browse the whole thing if all models stink
    cat("\n")
    print(paste("Bootstrap percent:", i))
    my.perf = nfold.xval(events[events$Random.Percent <= i, ], 
        fold=10, annotation=paste(i, "% Bootstrap Sample", sep=""))[[1]]
    perf[[i]] = rejigger.perf(my.perf)
    op.2 = calculate.operating.parameters(perf[[i]], method="confidence.range")$op
    op.3 = calculate.operating.parameters(perf[[i]], method="frequency")$op
    op[i] = ifelse(op.2 < op.3, op.2, op.3)
    bs.noise = dim(events[events$Random.Percent <= i & events$Call_vs_Noise == "Noise",])[1]
    bs.call = dim(events[events$Random.Percent <= i & events$Call_vs_Noise == "Call",])[1]
    print(paste("Bootstrap sample:", bs.noise, "noise,", bs.call, "call."))
    n.left = dim(events)[1] * (100 - i)/100 * bs.noise / (bs.noise + bs.call)
    fpr.mean = mean(perf[[i]][perf[[i]]$alpha.values == op[i], "x.values"])
    print(paste("Mean Estimated FPR:", round(fpr.mean, 3)))
    fpr.median = median(perf[[i]][perf[[i]]$alpha.values == op[i], "x.values"])
    print(paste("Median Estimated FPR:", round(fpr.median, 3)))
    fp.left[i] = floor(n.left * (bs.noise / (bs.noise + bs.call)) * fpr.mean)
    print(paste("Estimated FP left:", fp.left[i]))
    # heck, let's generate an actual FPR!
    # YO - do I really need to use only 70%? I will presumably be getting a 
    # better model if I use the whole thing.
    # Let's try it!
    #rf = randomForest(Call_vs_Noise ~ ., data= events[events$Random.Percent <= i, ], sampsize = 0.99 * nrow(events[events$Random.Percent <= i, ]))
    splits = splitdf(events[events$Random.Percent <= i, ], weight=7/10)
    rf = randomForest(Call_vs_Noise ~ ., data= splits$trainset[, c(seewave.measures, "Call_vs_Noise", "Detector")], 
                      sampsize = 0.99 * nrow(splits$trainset))
    # use it to predict the validation set
    pred = predict(rf,  events[events$Random.Percent > i, ], type="prob")
    pred.rocr = prediction(pred[,2],  events[events$Random.Percent > i, ]$Call_vs_Noise)
    actual.perf = performance(pred.rocr, measure="tpr", x.measure="fpr")
    actual.perf = rejigger.perf(actual.perf)
    actual.tpr = round(actual.perf[actual.perf$alpha.values == op[i], ]$y.values, 3)
    actual.fpr = round(actual.perf[actual.perf$alpha.values == op[i], ]$x.values, 3)
    print(paste("Actual TPR: ", actual.tpr, ". Actual FPR: ", actual.fpr,
	".", sep = ""))
    if (i < 4) next # we need at least n=4 to guess at done-ness
    n.avoid = fp.left[i - 1] - fp.left[i]
    percentile.size = round(dim(events)[1] / 100)
    if (n.avoid < percentile.size) {
      print(paste("Avoided ", n.avoid, ": less than percentile size ", percentile.size, "; bootstrap complete!", sep=""))
      if (prev.reject) {
        print("Two rejects in a row; bootstrap complete!")
      } else {
        prev.reject = T
      }
    } else {
      print(paste("Avoided ", n.avoid, ": more than percentile size ", percentile.size, "; recommend continuing.", sep=""))
      prev.reject = F
    }
    avg.n.avoid = round(((fp.left[i - 1] - fp.left[i]) + (fp.left[i-2] - fp.left[i-1]) + (fp.left[i-3] - fp.left[i-2]))/3, 2)
    if (avg.n.avoid < percentile.size) {
      print(paste("Average avoidance ", avg.n.avoid, ": less than percentile size ", percentile.size, "; bootstrap complete!", sep=""))
    } else {
      print(paste("Average avoidance ", avg.n.avoid, ": more than percentile size ", percentile.size, "; recommend continuing.", sep=""))
    }
  }
}

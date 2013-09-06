\name{combined.thrush.model}
\alias{combined.thrush.model}
\docType{data}
\title{
A Random Forest model for low-band detections.
}
\description{
This model is built on call-vs-noise data from 13 NFC deployments in the northeastern US.
}
\usage{data(combined.thrush.model)}
\format{
  The format is:
List of 19
 $ call           : language randomForest(formula = Call_vs_Noise ~ ., data = bootstrap.all[, c(seewave.measures,      "Call_vs_Noise", "Event_Duration")], na.action = na.omit)
 $ type           : chr "classification"
 $ predicted      : Factor w/ 2 levels "Call","Noise": 2 2 1 1 1 2 1 2 1 1 ...
  ..- attr(*, "names")= chr [1:11495] "1" "2" "3" "4" ...
 $ err.rate       : num [1:500, 1:3] 0.0628 0.0595 0.06 0.0607 0.0604 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr [1:3] "OOB" "Call" "Noise"
 $ confusion      : num [1:2, 1:3] 7.28e+02 1.39e+02 3.17e+02 1.03e+04 3.03e-01 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2] "Call" "Noise"
  .. ..$ : chr [1:3] "Call" "Noise" "class.error"
 $ votes          : matrix [1:11495, 1:2] 0.362 0.333 0.857 0.645 0.572 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:11495] "1" "2" "3" "4" ...
  .. ..$ : chr [1:2] "Call" "Noise"
  ..- attr(*, "class")= chr [1:2] "matrix" "votes"
 $ oob.times      : num [1:11495] 196 195 175 197 180 184 187 184 205 173 ...
 $ classes        : chr [1:2] "Call" "Noise"
 $ importance     : num [1:28, 1] 125 40.3 205.7 69.8 48.5 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:28] "Rugosity" "Crest_Factor" "Temporal_Entropy" "Shannon_Entropy" ...
  .. ..$ : chr "MeanDecreaseGini"
 $ importanceSD   : NULL
 $ localImportance: NULL
 $ proximity      : NULL
 $ ntree          : num 500
 $ mtry           : num 5
 $ forest         :List of 14
  ..$ ndbigtree : int [1:500] 693 699 687 707 733 715 705 687 693 741 ...
  ..$ nodestatus: int [1:795, 1:500] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ bestvar   : int [1:795, 1:500] 5 4 3 16 18 20 4 11 22 5 ...
  ..$ treemap   : int [1:795, 1:2, 1:500] 2 4 6 8 10 12 14 16 18 20 ...
  ..$ nodepred  : int [1:795, 1:500] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ xbestsplit: num [1:795, 1:500] 0.771 0.832 0.982 206.071 2540 ...
  ..$ pid       : num [1:2] 1 1
  ..$ cutoff    : num [1:2] 0.5 0.5
  ..$ ncat      : Named int [1:28] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "names")= chr [1:28] "Rugosity" "Crest_Factor" "Temporal_Entropy" "Shannon_Entropy" ...
  ..$ maxcat    : int 1
  ..$ nrnodes   : int 795
  ..$ ntree     : num 500
  ..$ nclass    : int 2
  ..$ xlevels   :List of 28
  .. ..$ Rugosity                             : num 0
  .. ..$ Crest_Factor                         : num 0
  .. ..$ Temporal_Entropy                     : num 0
  .. ..$ Shannon_Entropy                      : num 0
  .. ..$ Shannon_Entropy_Bandlimited          : num 0
  .. ..$ Spectral_Flatness_Measure            : num 0
  .. ..$ Spectral_Flatness_Measure_Bandlimited: num 0
  .. ..$ Spectrum_Roughness                   : num 0
  .. ..$ Spectrum_Roughness_Bandlimited       : num 0
  .. ..$ Autocorrelation_Mean                 : num 0
  .. ..$ Autocorrelation_Median               : num 0
  .. ..$ Autocorrelation_Standard_Error       : num 0
  .. ..$ Dominant_Frequency_Mean              : num 0
  .. ..$ Dominant_Frequency_Standard_Error    : num 0
  .. ..$ Specprop_Mean                        : num 0
  .. ..$ Specprop_SD                          : num 0
  .. ..$ Specprop_SEM                         : num 0
  .. ..$ Specprop_Median                      : num 0
  .. ..$ Specprop_Mode                        : num 0
  .. ..$ Specprop_Q25                         : num 0
  .. ..$ Specprop_Q75                         : num 0
  .. ..$ Specprop_IQR                         : num 0
  .. ..$ Specprop_Cent                        : num 0
  .. ..$ Specprop_Skewness                    : num 0
  .. ..$ Specprop_Kurtosis                    : num 0
  .. ..$ Specprop_SFM                         : num 0
  .. ..$ Specprop_SH                          : num 0
  .. ..$ Event_Duration                       : num 0
 $ y              : Factor w/ 2 levels "Call","Noise": 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:11495] "1" "2" "3" "4" ...
 $ test           : NULL
 $ inbag          : NULL
 $ terms          :Classes 'terms', 'formula' length 3 Call_vs_Noise ~ Rugosity + Crest_Factor + Temporal_Entropy + Shannon_Entropy +      Shannon_Entropy_Bandlimited + Spectral_Flatness_Measure + Spectral_Flatness_Measure_Bandlimited +  ...
  .. ..- attr(*, "variables")= language list(Call_vs_Noise, Rugosity, Crest_Factor, Temporal_Entropy, Shannon_Entropy,      Shannon_Entropy_Bandlimited, Spectral_Flatness_Measure, Spectral_Flatness_Measure_Bandlimited,  ...
  .. ..- attr(*, "factors")= int [1:29, 1:28] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:29] "Call_vs_Noise" "Rugosity" "Crest_Factor" "Temporal_Entropy" ...
  .. .. .. ..$ : chr [1:28] "Rugosity" "Crest_Factor" "Temporal_Entropy" "Shannon_Entropy" ...
  .. ..- attr(*, "term.labels")= chr [1:28] "Rugosity" "Crest_Factor" "Temporal_Entropy" "Shannon_Entropy" ...
  .. ..- attr(*, "order")= int [1:28] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "intercept")= num 0
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(Call_vs_Noise, Rugosity, Crest_Factor, Temporal_Entropy, Shannon_Entropy,      Shannon_Entropy_Bandlimited, Spectral_Flatness_Measure, Spectral_Flatness_Measure_Bandlimited,  ...
  .. ..- attr(*, "dataClasses")= Named chr [1:29] "factor" "numeric" "numeric" "numeric" ...
  .. .. ..- attr(*, "names")= chr [1:29] "Call_vs_Noise" "Rugosity" "Crest_Factor" "Temporal_Entropy" ...
 - attr(*, "class")= chr [1:2] "randomForest.formula" "randomForest"
}
\examples{
data(combined.thrush.model)
## maybe str(combined.thrush.model) ; plot(combined.thrush.model) ...
}
\keyword{datasets}

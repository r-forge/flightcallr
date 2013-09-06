\name{danby}
\alias{danby}
\docType{data}
\title{Sample NFC Detections}
\description{
This is a very small subset of the detections from a 2009 set of overnight recordings from Danby, NY, recorded by the Conservation Science department at the Cornell Lab of Ornithology.
}
\usage{data(danby)}
\format{
  A data frame with 150 observations on the following 33 variables.
  \describe{
    \item{\code{Begin.Time..s.}}{a numeric vector}
    \item{\code{Detector}}{a factor with levels \code{Sparrow}}
    \item{\code{Random.Percent}}{a numeric vector}
    \item{\code{Event_Duration}}{a numeric vector}
    \item{\code{Call_vs_Noise}}{a factor with levels \code{Call} \code{Noise}}
    \item{\code{Rugosity}}{a numeric vector}
    \item{\code{Crest_Factor}}{a numeric vector}
    \item{\code{Temporal_Entropy}}{a numeric vector}
    \item{\code{Shannon_Entropy}}{a numeric vector}
    \item{\code{Shannon_Entropy_Bandlimited}}{a numeric vector}
    \item{\code{Spectral_Flatness_Measure}}{a numeric vector}
    \item{\code{Spectral_Flatness_Measure_Bandlimited}}{a numeric vector}
    \item{\code{Spectrum_Roughness}}{a numeric vector}
    \item{\code{Spectrum_Roughness_Bandlimited}}{a numeric vector}
    \item{\code{Autocorrelation_Mean}}{a numeric vector}
    \item{\code{Autocorrelation_Median}}{a numeric vector}
    \item{\code{Autocorrelation_Standard_Error}}{a numeric vector}
    \item{\code{Dominant_Frequency_Mean}}{a numeric vector}
    \item{\code{Dominant_Frequency_Standard_Error}}{a numeric vector}
    \item{\code{Specprop_Mean}}{a numeric vector}
    \item{\code{Specprop_SD}}{a numeric vector}
    \item{\code{Specprop_SEM}}{a numeric vector}
    \item{\code{Specprop_Median}}{a numeric vector}
    \item{\code{Specprop_Mode}}{a numeric vector}
    \item{\code{Specprop_Q25}}{a numeric vector}
    \item{\code{Specprop_Q75}}{a numeric vector}
    \item{\code{Specprop_IQR}}{a numeric vector}
    \item{\code{Specprop_Cent}}{a numeric vector}
    \item{\code{Specprop_Skewness}}{a numeric vector}
    \item{\code{Specprop_Kurtosis}}{a numeric vector}
    \item{\code{Specprop_SFM}}{a numeric vector}
    \item{\code{Specprop_SH}}{a numeric vector}
    \item{\code{Detector_numeric}}{a numeric vector}
  }
}
\examples{
data(danby)
head(danby)
}
\keyword{datasets}

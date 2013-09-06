\name{bandlimit.spectrum}
\alias{bandlimit.spectrum}
\title{bandlimit spectrum}
\description{This function takes a spectrum matrix and chops off the rows which are not within the specified band.
It's not clear that it's useful overall, since seewave's specprop() has the ability to do band-
limiting. Nevertheless we're holding on to it until we can verify that it doesn't provide anything 
useful}
\usage{bandlimit.spectrum(spectrum, band)}
\arguments{
  \item{spectrum}{The spectrum to be band-limited.}
  \item{band}{The name of the band, either "Sparrow" or "Thrush".}
}

\value{The band-limited spectrum.}

\author{Jesse C. Ross}






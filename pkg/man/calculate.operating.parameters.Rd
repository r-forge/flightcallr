\name{calculate.operating.parameters}
\alias{calculate.operating.parameters}
\title{calculate operating parameters}
\description{Calculate the operating point for a given ROCR perf object.}
\usage{calculate.operating.parameters(perf, method = "frequency", desired.tpr = "0.95")}
\arguments{
  \item{perf}{The perf object.}
  \item{method}{The confidence estimation method}
  \item{desired.tpr}{The desired TPR. The operating point returned will have a 95\% chance of 
achieving this TPR or higher.}
}

\value{A vector containing the operating point, as well as estimates of TPR and FPR.}

\author{Jesse C. Ross}






\name{save.selection.table}
\alias{save.selection.table}
\title{save selection table}
\description{Save a selection table to a file.
Multi-line documentation!}
\usage{save.selection.table(selection.table = "", filename = "", path = "", 
    include.measures = F, include.call.vs.noise = F, include.predictions = F)}
\arguments{
  \item{selection.table}{The selection table data frame.}
  \item{filename}{The desired filename}
  \item{path}{The desired path}
  \item{include.measures}{Include generated features in the table?}
  \item{include.call.vs.noise}{Include the Call_vs_Noise column?}
  \item{include.predictions}{Include prediction columns RF_Score and RF_Prediction?}
}

\value{The return value is the same as that of write.table()}

\author{Jesse C. Ross}






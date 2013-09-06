\name{generate.forest.predictions}
\alias{generate.forest.predictions}
\title{generate forest predictions}
\description{Generate the predictions for a selection table and a random forest model.}
\usage{generate.forest.predictions(ubertable, forest, perf = NULL, order.by = "percent")}
\arguments{
  \item{ubertable}{The selection table, with feature columns added by 
generate.seewave.measures()}
  \item{forest}{The random forest model.}
  \item{perf}{A perf object, if bootstrapping has been done.}
  \item{order.by}{Whether to order the results by percent or rank within day.}
}

\value{The selection table, with columns added for RF_Score and RF_Rank_Within_Day}

\author{Jesse C. Ross}






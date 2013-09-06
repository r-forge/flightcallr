\name{splitdf}
\alias{splitdf}
\title{splitdf}
\description{This will randomly split a data frame into training and test sets. I borrowed it from
http://gettinggeneticsdone.blogspot.com/2011/02/split-data-frame-into-testing-and.html
The caret packages has a similar facility using its createDataPartition() function.
Perhaps even more appealing, there is sample.split() in caTools.}
\usage{splitdf(dataframe, weight = 2/3, seed = NULL)}
\arguments{
  \item{dataframe}{this is the data frame you want to split}
  \item{weight}{the weights for the different classes}
  \item{seed}{an optional seed}
}

\value{A list with two members; the training set and the test set}

\author{Jesse C. Ross}






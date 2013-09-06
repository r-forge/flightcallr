\name{read.selection.table}
\alias{read.selection.table}
\title{read selection table}
\description{Read a Raven selection table into a data frame.}
\usage{read.selection.table(filename, Sound_File_Path, detector)}
\arguments{
  \item{filename}{The selection table to read.}
  \item{Sound_File_Path}{The path of the sound file(s) referred to in the selection table. If the
Begin Path measurement is present in the selection table, it is not 
necessary to include this argument. If sound files are at multiple paths,
it is necessary to include the Begin Path measurement instead of using this
argument.}
  \item{detector}{
}
}

\value{The data frame representation of the selection table.}

\author{Jesse C. Ross}






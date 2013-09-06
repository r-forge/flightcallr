\name{generate.seewave.measures}
\alias{generate.seewave.measures}
\title{generate seewave measures}
\description{Extract acoustic features from selections in a selection table. Requires
that SoX (http://sox.sourceforge.net) be installed and callable as "soxi".}
\usage{generate.seewave.measures(ubertable, sf_length, save_progress_file, 
    save_every = 5000, sampling_rate = 24000)}
\arguments{
  \item{ubertable}{A selection table. In order to deal with selections which span sound files,
generate.seewave.measures() can run soxi to tell the lengths of sound files.
In this case, it is necessary that the "File Offset (s)" column be present,
as well as either "Begin File" or "Begin Path".}
  \item{sf_length}{As an alternative to running soxi, if all the sound files are of the same
length, this can be specified (in seconds).}
  \item{save_progress_file}{Name of file to save intermediate progress to. If absent, autosave is not
done.}
  \item{save_every}{How many rows to calculate before saving intermediate progress file.}
  \item{sampling_rate}{The sampling rate of the file(s) containing selections. Necessary in order to choose a suitable FFT window size.}
}

\value{The original selection table, with columns added for generated features.}

\author{Jesse C. Ross}






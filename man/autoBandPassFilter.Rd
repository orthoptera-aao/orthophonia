\name{autoBandPassFilter}
\alias{autoBandPassFilter}
\title{Applies automatic bandpass filter to an AM sound}
\usage{
autoBandPassFilter(wave, bps = 2, min_freq = 1000, max_freq = NULL,
  wl = 2^10, plot=FALSE)
}
\arguments{
\item{wave}{an R object or path to a wave file}

\item{bps}{the bandpass size relative to \deqn{\sigma}{sigma}}

\item{min_freq}{the lowest expected frequency, in Hz}

\item{max_freq}{the highest expected frequency, in Hz. If NULL the Nyquist frequency of the file will be used.}

\item{wl}{the window length for spectrogram generation}

\item{plot}{Is set to TRUE the function will plot the output.}
}

\value{
a bandpass filter wave of the same type as \code{wave}
}
\description{
This function assumes that the wave is a recording containing principally 
a single source and amplitude modulated song. 
It tries to find and apply automatic bandpass filter in order to reduce noise.
}
\details{
The \code{bps} arguments refers to the selectivity of the filter.
It represents the width of the bandpass filter relative to
the standard deviation of the PDF, in frequency range.
In the original publication \code{bps} = 2 (i.e. \deqn{2\sigma}{2 sigmas}, see eq. 3).
The present implementation allow to alter this parameter. 
Higher values will result in broader (i.e. less selective) filter.
}
\note{
todo ref (Dietrich et al., 2004)
}


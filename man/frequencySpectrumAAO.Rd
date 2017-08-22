\name{frequencySpectrumAAO}
\alias{frequencySpectrumAAO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the frequency spectrum (50pc/3dB)
}
\description{
This function returns (1) the lowest frequency with an amplitude 25\% of the amplitude of the peak frequency; (2) the lowest frequency with an amplitude of 50\% of that of the peak frequency; (3) the peak (by amplitude) frequency; (4) the highest frequency with an amplitude of 50\% of that of the peak frequency; (5) the highest frequency with an amplitude of 25\% of that of the peak frequency.
}
\usage{
frequencySpectrumAAO(wave, min_freq=1000, plot=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wave}{
    A wave object
  }
  \item{min_freq}{
  Freuqencies below this value will be discarded (Hertz).
  }
  \item{plot}{
    If TRUE plots the output.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of the above described values.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Ed Baker
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{frequencySpectrumPowerPercentile} \link{frequencySpectrumPowerQuartiles}}
}
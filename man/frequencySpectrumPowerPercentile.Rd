\name{frequencySpectrumPowerPercentile}
\alias{frequencySpectrumPowerPercentile}
\alias{frequencySpectrumPowerQuartiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to find the bandwidth containing x\% of the total energy.
}
\description{
The bandwidth containing a specified percentage of the energy of a wave is returned. e.g. the default value for \code{pc} of 50 will return the central bandwidth that cotains 50\% of the total energy.
}
\usage{
frequencySpectrumPowerPercentile(wave, pc=50, min_freq = 1000, plot=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wave}{
    A wave object
  }
  \item{pc}{
  Percentage value of the bandwidth to find.
  }
  \item{min_freq}{
  Frequencies below this value will be ignored in the calculations.
  }
  \itemn{plot}{
  If \code{TRUE} the power spectrum and percentage lines will be plotted.
  }
}
\details{
\code{frequencySpectrumPowerQuartiles()} allows the commonly used power quartiles (\code{pc=50}) to be calculated without specifying a value for \code{pc}.
}
\value{
A vector of the minium and maximum frequency values calculated.
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
\code{\link{frequencySpectrumAAO} \link{frequencySpectrumPowerQuartiles}}
}

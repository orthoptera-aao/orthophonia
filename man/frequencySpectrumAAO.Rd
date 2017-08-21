\name{frequencySpectrumAAO}
\alias{frequencySpectrumAAO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Computes the frequency spectrum (50pc/3dB)
}
\description{
Calculates the peak frequency and minimum and maximum values of the 50\%frequency bandwidth.
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
A vector of the 50\% frequency spectrum, and the peak frequency.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
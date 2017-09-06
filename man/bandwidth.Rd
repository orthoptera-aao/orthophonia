\name{bandwidth}
\alias{bandwidth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate sthe bandwidth of a wave
}
\description{
Claculate the bandwith of a wave.
}
\usage{
bandwidth(wave, min_freq=1000, plot = FALSE, method = "quartile")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wave}{
    A wave object.
  }
  \item{min_freq}{
    Frequencies below this will be filtered out.
  }
  \item{plot}{
    If true the freuqncy spectrum and bandwidth will be plotted.
  }
  \item{method}{
    Method used to calculate the bandwidth.
  }
}

\value{
A numeric value in kHz.
}
\author{
Ed Baker
}

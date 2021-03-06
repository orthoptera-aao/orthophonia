\name{beatSpectrum}
\alias{beatSpectrum}
\title{Computes a beat spectrum}
\usage{
beatSpectrum(wave, min_period = 5e-04, max_period = 30, dj = 1/32, ...)
}
\arguments{
\item{wave}{an R object or path to a wave file}

\item{min_period}{the minimal rythmicity period expected, in seconds}

\item{max_period}{the maximal rythmicity period expected, in seconds}

\item{dj}{the frequency resolution of the cwt (in voices per octave)}

\item{...}{extra arguments passed to \code{analyze.wavelet()}}
}
\value{
a spectrum as a \code{data.table}. 
It contains two columns: \code{power} and \code{period}. 
The number of rows depend on the resolution and frequency range.
}
\description{
Beat spectra represent the periodicity in signal amplitude.
It is computed by performing a continuous wavelet transform on
the envelope of a preprocessed signal, and processing 
the average power per frequency band.
}


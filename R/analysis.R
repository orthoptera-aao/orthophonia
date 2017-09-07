bandwidth <- function(wave, min_freq=1000, plot = FALSE, method = "quartile") {
  if (method == "quartile") {
    a <- orthophonia::frequencySpectrumPowerQuartiles(wave, min_freq, plot)
    if (is.null(a)) {
      return(NULL)
    }
    return (a[[2]] - a[[1]])
  }
}
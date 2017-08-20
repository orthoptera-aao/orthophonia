frequencySpectrum50pc <- function(wave, plot=FALSE) {
  s <- meanspec(wave, norm=TRUE, PSD=TRUE, plot=plot)
  s50 <- s[(s[,2]) >= 0.5,]
  s50_range <- c(s50[[1,1]], fpeaks(s, nmax=1, plot=FALSE)[1,1], s50[[nrow(s50),1]])
  if (plot == TRUE) {
    abline(v=s50_range, col="red")
  }
  return(s50_range)
}
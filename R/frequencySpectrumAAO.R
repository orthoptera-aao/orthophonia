frequencySpectrumAAO <- function(wave, min_freq = 1000, plot=FALSE) {
  wl_values <- c(10,12,14)
  for (i in wl_values) {
    s <- seewave::meanspec(wave, wl=2^i, norm=TRUE, PSD=TRUE, plot=plot)
    #Discard frequencies below min_freq
    s <- s[s[,1] > min_freq/1e3,]
    s_max <- max(as.numeric(s[,2]))
    
    s50 <- s[(s[,2]) >= 0.5*s_max,]
    if (length(s50) == 0 || !is.matrix(s50) || nrow(s50) < 3) {
      if (i == wl_values[[length(wl_values)]]) {
        return(c())
      } else {
        next
      }
    }
    s50_range <- c(s50[[1,1]], seewave::fpeaks(s, nmax=1, plot=FALSE)[1,1], s50[[nrow(s50),1]])
    s25 <- s[(s[,2]) >= 0.25*s_max,]
    s25_range <- c(s25[[1,1]], s25[[nrow(s25),1]])
    if (plot == TRUE) {
      abline(v=s50_range, col="red")
      abline(v=s25_range, col="blue")
    }
    return(c(s25_range[1], s50_range, s25_range[2]))
  }
}
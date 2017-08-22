frequencySpectrumPowerPercentile <- function(wave, pc=50, min_freq = 1000, plot=FALSE) {
    s <- seewave::meanspec(wave, wl=2^10, norm=TRUE, PSD=TRUE, plot=plot)
    #Discard frequencies below min_freq
    s <- s[s[,1] > min_freq/1e3,]
    
    total <- sum(s[,2])
    
    cumulative <- 0
    
    values <- c()
    
    target <- (100 - pc) / 200
    
    for (i in 1:nrow(s)) {
      cumulative <- cumulative + s[[i,2]]
      if (cumulative >= target * total) {
        values <- c(values, s[[i,1]])
        if (target == 0.5 + pc/200) {
          break()
        } else {
          target <-  0.5 + pc/200
        }
      }
    }
    if (plot==TRUE) {
      abline(v=values, col="red")
    }
    return(values)
}

frequencySprectrumPowerQuartiles <- function(wave, min_freq = 1000, plot=FALSE) {
  return(orthophonia::frequencySpectrumPowerPercentile(wave, pc=50, min_freq = min_freq, plot=plot))
}
#' Locates pulses
#' 
#' test
#' 

#' @param wave an R object
#' @param alpha the coefficient for the upper dynamic threshold
#' @param gamma see eq.8
#' @param u the duration, in ms, of the short#' @param v the duration, in ms, of the long-time window-time window
#' @param v the duration, in ms, of the long-time window
#' @param beta see eq.
#' @param zeta see eq.
#' @return a \code{data.table} with the onset and duration of detected pulses
#' #' @param v the duration, in ms, of the long-time window
#' #' @param v the duration, in ms, of the long-time window
#' @return a bandpass filter wave of the same type as \code{wave}
#' 
#' @export
pulseSegmentation <- function(wave, 
                              alpha=1.4, 
                              gamma=.05,
                              u=2.7, 
                              v=10.8, 
                              beta=u/4, 
                              zeta=v/u ){
  
  signal = standardiseWave(wave)
  u_n_points = round(u * 1e-3 * wave@samp.rate /2) * 2 + 1
  
  v_n_points = round(v * 1e-3 * wave@samp.rate /2) * 2 + 1
  short_time_energy = filter(abs(signal@left), rep(1,u_n_points))
#  plot(short_time_energy)
  long_time_energy = filter(abs(signal@left), rep(1,v_n_points))
  
  theta = min(na.omit(short_time_energy)) + gamma*(max(na.omit(short_time_energy)) - min(na.omit(short_time_energy)))
  low_threshold <- theta *  1 * long_time_energy/zeta
  high_threshold <-theta * alpha * long_time_energy/zeta
  
  pass_low <- as.numeric(short_time_energy > low_threshold)
  pass_high <- as.numeric(short_time_energy > high_threshold)
  dt <- data.table(pass_low = pass_low, pass_high=pass_high, t =1:length(pass_low),ste = short_time_energy)
  
  
#  lines(low_threshold, col=2)
#  lines(high_threshold, col=4)
  
    
    
  dt
  
}
# st <- pulseSegmentation(wave)
# plot(st[]
NULL
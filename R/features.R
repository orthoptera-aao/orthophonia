#' @include
NULL

#' Locates pulses
#' 
#' test
#' 
#' @export
pulseSegmentation <- function(wave, 
                              alpha=1.4, 
                              gamma=.05,
                              u=2.7, 
                              v=10.8, 
                              beta=u/4, 
                              zeta=v/2 ){
  
  signal = standardiseWave(wave)
  u_n_points = round(u * 1e-3 * wave@samp.rate /2) * 2 + 1
  print(u_n_points)
  v_n_points = round(v * 1e-3 * wave@samp.rate /2) * 2 + 1
  short_time_energy = filter(abs(signal@left), rep(1,u_n_points))
  plot(short_time_energy)
  long_time_energy = filter(abs(signal@left), rep(1,v_n_points))
  lines(alpha * long_time_energy/zeta, col=2)
  uper_threshold =
    
    
  short_time_energy
  
}
# st <- pulseSegmentation(wave)
# plot(st[]
NULL
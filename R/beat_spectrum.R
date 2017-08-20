beatSpectrum <- function(wave, 
                         min_period = 5e-4,#s
                         max_period=30, #s,
                         dj=1/32, # 1/nvoices
                         ...
){
  
  wave_std <- orthophonia::standardiseWave(wave) 
  scaling_ratio <- wave_std@samp.rate / (1/min_period)
  runmed_k <- 2*(floor(scaling_ratio/2))+1
  signal <- runmed(abs(wave_std@left), runmed_k)
  n = length(signal)
  t0 <- 0:(n-1) / wave_std@samp.rate
  t1 <- seq(from=0, to=t0[length(t0)], by=min_period)
  signal_dsp <- approx(y=signal,t0, xout = t1)$y
  dt_tmp <- data.table(x=signal_dsp)
  upper_period = ceiling(1 + log2(max_period/ min_period))
  wt <- analyze.wavelet(dt_tmp,"x",
                        loess.span = 0, dj=dj,
                        lowerPeriod = 2 ^ 1,
                        upperPeriod = 2 ^ upper_period,
                        make.pval =F, 
                        verbose = F,...)
  data.table(power=wt$Power.avg, period = wt$Period * min_period)
}



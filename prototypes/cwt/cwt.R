#install.packages("WaveletComp")
rm(list=ls())
library("orthophonia")
library(WaveletComp)
library(data.table)
library(ggplot2)


wav1 <- readWave("~/Desktop/ortho_data/589_1_Tettigonia_cantans_768r1.wav",
                from = 25, to=85,units = "seconds")
wav2 <- readWave("~/Desktop/ortho_data/423-1_Tettigonia_cantans_596a_.wav",
                from = 120, to=170,units = "seconds")
wav1 <- orthophonia::standardiseWave(wav1)
wav2 <- orthophonia::standardiseWave(wav2)

signal1 <- abs(wav1@left)
#smooth and downsample
signal1 <- runmed(signal1, 21)
signal1 <- signal1[seq(from=0,to=length(signal1), by=20)]



signal2 <- abs(wav2@left)
#smooth and downsample
signal2 <- runmed(signal2, 21)
signal2 <- signal2[seq(from=0,to=length(signal2), by=20)]

pdf("/tmp/cwt.pdf")

dt <- data.table("x"=signal1[1:10e4])

w1 <- analyze.wavelet(dt,"x",
                 loess.span = 0,
                 dt = 1, dj = 1/32,
                 lowerPeriod = 2,
                 upperPeriod = 2^16,
                 make.pval =F)
dt <- data.table("x"=signal2[1:10e4])
w2 <- analyze.wavelet(dt,"x",
                     loess.span = 0,
                     dt = 1, dj = 1/32,
                     lowerPeriod = 2,
                     upperPeriod = 2^16,
                     make.pval =F)
wt.image(w1)
wt.image(w2)
dt <- rbind(
  data.table(wave="w1", period=w1$Period, power_avg = w1$Power.avg),
  data.table(wave="w2", period=w2$Period, power_avg = w2$Power.avg)
  )

ggplot(dt, aes(period, power_avg, colour=wave)) + geom_line() + scale_x_log10()

dev.off()



#' 
#' 
#' 
beatSpectrum <- function(wave, 
                         min_period = 5e-4,#s
                         max_period=10, #s,
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

wav1 <- readWave("~/Desktop/ortho_data/old/589_1_Tettigonia_cantans_768r1.wav",
                 from = 45, to=85,units = "seconds")
s <- summary(wav1)


wav1 <- readWave("~/Desktop/ortho_data/424-17_Platycleis_grisea_605r1_.wav",
                 from = 60, to=100,units = "seconds")
beat_sptrm <- beatSpectrum(wav1)

struct = c(400,35,1.25)
ggplot(beat_sptrm, aes(period*1000, power)) + 
    geom_point(size=.1) + scale_x_log10() +
    annotate("segment", 
           x = struct, xend = struct,
           y = -Inf, yend = +Inf,
           colour = "blue")



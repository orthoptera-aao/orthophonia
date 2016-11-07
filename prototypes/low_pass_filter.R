library(data.table)


lowPassMedian <- function(wave, f0){
  wave <- standardiseWave(wave)
  period_s <- 1/f0
  print(period_s)
  period = period_s * wave@samp.rate
  k <- (round(period/4) * 2) +1
  print(k)
  ts <- runmed(wave@left,k)
  wave@left <- wave@left - ts 
  wave
}
file <- "~/Desktop/ortho_data/annotation_294_898_1_Pholidoptera_griseoaptera_73r1.wav"
w0 <- standardiseWave(file)
r <- (2*44100 +1):(44100*2.4)
plot(w0[r])
w <- lowPassMedian(file, 1000)

plot(w[r])
bs <- beatSpectrum(w)
bs0 <- beatSpectrum(w0)
bs0[,pp := "no"]
bs[,pp := "med"]
bs <- rbind(bs0, bs)

ggplot(bs, aes(x=period, y =power, colour = pp)) + geom_line() + scale_x_log10()

meanspec(w, wl=1024, ovlp = 95)

acf(abs(w@left), lag.max=10000)

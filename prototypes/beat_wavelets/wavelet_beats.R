rm(list=ls())
library("orthophonia")
library(data.table)
library(wavelets)
library(ggplot2)
wav <- readWave("~/Desktop/ortho_data/423-1_Tettigonia_cantans_596a_.wav",
                from = 120, to=150,units = "seconds")
wav <- readWave("~/Desktop/ortho_data/589_1_Tettigonia_cantans_768r1.wav",
                from = 25, to=85,units = "seconds")
wav <- orthophonia::standardiseWave(wav)
wt_wave <- dwt(abs(wav@left),n.levels = 15)

LAG_MAX <- 1000
wavelets <- wt_wave@W

pdf("/tmp/test.pdf", w=16,h=9)
for(i in 1:length(wavelets)){
  w = runmed(abs(wavelets[[i]]),5)
  acf(w,lag.max=LAG_MAX, main=as.character(i))
  
}
dev.off()
wavelets <- lapply(wavelets, function(x)runmed(abs(x),11))
acfs <- lapply(wavelets, acf,lag.max=LAG_MAX,plot=F)

makeTimedACFValues <- function(scale, acfs){
  acf = acfs[[scale]]
  t <- seq(from = 1, 
           length.out =length(acf$acf), 
           by = 2 ^(scale-1))
  out <- data.frame(t=t, r = acf$acf,scale=scale, lag=1:length(t))
  
  out
}
o <- lapply(1:length(acfs),makeTimedACFValues, acfs)
dt <- rbindlist(o)
dt[, t := 2 * t * 1000 / wav@samp.rate ]

struct=c(1.7,30,1e4)
ggplot(dt[ lag >1], aes(t,r)) + 
                  geom_point(size=.5) +
                  geom_line() + 
                  scale_x_log10() + 
                  facet_grid( scale~ .) +
                  annotate("segment", 
                           x = struct, xend = struct,
                           y = -Inf, yend = +Inf,
                           colour = "blue")

setkeyv(dt,"t")
ggplot(dt[lag>20,.(r=mean(r)),by=t], aes(t,r)) +geom_line() +
  scale_x_log10() + 
  annotate("segment", 
           x = struct, xend = struct,
           y = -Inf, yend = +Inf,
           colour = "blue")


                  
 fillAcfAggregator <- function(i, acfs,acf_aggregator){
  acf_aggregator <- rep(0, LAG_MAX * 2^(length(wavelets)-1)/2)
  acf_res <- acfs[[i]]$acf
  step <- 2 ^ (i-1)
  last <-  2 ^(i-1) *LAG_MAX +1
  seq_ <- seq(from=1, to=last,by=step)[1:length(acf_res)]
  
  print(length(acf_res))
  print(step)
  print(last)
  print(length(seq_))
  print("---------")
  acf_aggregator[seq_] <- acf_res
  acf_aggregator
  #wavelets[[i]]
}

l <- sapply(1:length(acfs),fillAcfAggregator,acfs)

o <- apply(l, 1, sum)
p <- apply(l, 1, function(x){sum(x!=0)})
last <- last(which(p !=0))
p <- p[1:last]
o <- o[1:last]
o <- o/p

library(data.table)
dt <- data.table(x = 1:length(o), y =o)
ggplot(dt, aes(x,y)) + geom_point() + scale_x_log10() +geom_line()




#########################
# library(bspec)
#o <- bspec(abs(wavelets[[1]]),log="x")
# plot(o)



abs(wav@left)

scaledAcf <- function(signal,scale){
  v <- runmed(abs(signal),k=2*scale +1)
  v <- v[seq(from=1, to=length(v), by=2^(scale-1))]
  meanspec(v,44000/2^(scale-1),log="x")
}

scaledAcf(abs(wav@left),scale=13)


seewave::fund()

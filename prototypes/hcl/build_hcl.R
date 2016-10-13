#' prototype to build hierachical clustering representation of songs
#' this is the basis for semisupervised, distance learning, classification.
#' 
rm(list=ls())

library("orthophonia")
library(data.table)
library(ggplot2)

DATA_DIR <- "~/Desktop/ortho_data"
query <- fread("./query.csv")


energyAutoCorrelogram <- function(wave, 
                                  wl=0.3, #ms
                                  down_sample_factor=10,
                                  lag_max=2000 #todo, put in ms
                                  ){
  wl_n_points <- round(wl * 1e-3 * wave@samp.rate /2) * 2 + 1
  s <- runmed(abs(wave@left),wl_n_points)  
  sss <- s[seq(from=1, to=length(s), by=down_sample_factor)]
  acf_res <- acf(sss,lag.max = lag_max, plot=F);
  #resample on logspace:
  # 1 : lag.max, in lagmax point
  
  e_seq <- seq(from=0, to=log10(lag_max), length.out=(lag_max))
  approx_out <- approx(y=acf_res$acf, x = 1: length(acf_res$acf),xout=10 ^ e_seq)
  list(acf = approx_out$y)
}

  
makeFeatures <- function(file,start,end,title=""){
  wave <- readWave(paste(DATA_DIR,file,sep="/"),  from=start, to=end, units="seconds")
  wave_std <- standardiseWave(wave)
  w2 <- autoBandPassFilter(wave_std, bps = .5)
  acf_res <- energyAutoCorrelogram(w2)
  #print(acf_res)
 #  
 #  o <- ggplot(dt, aes(t,r)) + geom_line() + 
 #    scale_x_log10(limits=c(.1,2000)) + 
 #    scale_y_continuous(limits=c(-0.5,1)) + ggtitle(title)
 #  print (o)
 # return(NA) 
}


distance <- function(x, y, ...){
  out <- round(sum(abs(x[[1]] - y[[1]])))
  plot(x[[1]],type="l",...)
  lines(y[[1]],col="blue")
  out 
}

distanceWrapper <- function(data,V1,V2, FUN,...){
  FUN(data[full_id==V1,features], data[full_id==V2,features],...)
}

query[, full_id := paste(species, id, sep="_")]
dt <- query[,
            .(features = makeFeatures(file, start, end, title=species))
          ,by=c("full_id")]


map <- dt[,as.data.table(t(combn(full_id, 2)))]
setnames(map, c("V1", "V2"), c("x", "y"))
map[, comp_id := 1:.N]
pdf()
dist_dt <- map[,
               .(dist = distanceWrapper(dt,x,y, FUN=distance,log="x",main=paste(x,y))),
               by=c("comp_id","x","y")]
dev.off()
dist_dt_wide <- reshape(dist_dt, idvar = "x", timevar ="y", v.names= "dist", direction = "wide")

dist_mat <- as.matrix(dist_dt_wide[,3:ncol(dist_dt_wide),with=F])
dist_mat <- cbind(NA,dist_mat)
dist_mat <- rbind(dist_mat,NA)

dist_mat[lower.tri(dist_mat,diag=F)] <- dist_mat[upper.tri(dist_mat,diag=F)]
diag(dist_mat) <- 0
rownames(dist_mat) <- dt[,full_id]
colnames(dist_mat) <- dt[,full_id]

dist_mat <- as.dist(dist_mat)
hclust(dist_mat)

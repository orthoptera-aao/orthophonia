wave <- my_annotations[2,annotation_path]
spectrumWrapper <- function(wave,wl=2 ^ 10){
  wave_std <- orthophonia::standardiseWave(wave) 
  filt_wave <- bwfilter(wave_std, 
                        from = 500, 
                        to = 20000, output="Wave")
  spec <- meanspec(filt_wave, wl=wl,ovlp = 95, plot=T)
  data.table(frequency = spec[,"x"] * 1000, amplitude= spec[,"y"])
}

DATA_DIR <- "~/Desktop/ortho_data/"

all_annotations = getAllAnnotationData()
query = all_annotations[author == "qgeissmann"]
my_annotations <- dowloadFilesForAnnotations(query,
                                             dst_dir = DATA_DIR,
                                             verbose=T,
                                             force=F)
# keep annotation shorted than 30s, for speed
query <- query[ end - start < 30 ]

# This is what we would like to do to each annotation:
pipelineFunction <- function(file_name){
  print(file_name)
  wave <- standardiseWave(file_name)
  #wave <- autoBandPassFilter(wave)
  beatSpectrum(wave)
}


# we use data table `by` to do the job, this can take some time:
dt <- my_annotations[, pipelineFunction(annotation_path), by=id]
dt2 <- my_annotations[, spectrumWrapper(annotation_path), by=id]

print(dt)
dt <- my_annotations[dt]
ggplot(dt, aes(period, power, group=id)) + geom_line(alpha=.3) +
  scale_x_log10(name="period (s)", limits=c(1e-3,5e1)) + facet_wrap(~ taxon, nrow=2 )  

dt2 <- my_annotations[dt2]
ggplot(dt2, aes(frequency, amplitude, group=id)) + geom_line(alpha=.3) +
  scale_x_log10(name="freq") + facet_wrap(~ taxon, nrow=2 )  

library(dtwclust)
library(ggdendro)

ctrl <- new("dtwclustControl", window.size = 20L, trace = TRUE)
datalist <- split(dt$power, dt$id)
datalist2 <- split(dt2$amplitude, dt2$id)
hc.sbd <- dtwclust(datalist, type = "hierarchical",
                   k = 19:21, distance = "sbd",
                   method = "all",
                   control = ctrl, seed=1234)
cvis <- sapply(hc.sbd, cvi, b = names(datalist))

hc.sbd2 <- dtwclust(datalist2, type = "hierarchical",
                   k = 19:21, distance = "sbd",
                   method = "all",
                   control = ctrl, seed=1234)
cvis <- sapply(hc.sbd2, cvi, b = names(datalist))


#labels <- paste(unique(dt)$taxon_id, ,sep="_")
result <- hc.sbd2[[which.min(cvis["VI", ])]]
result$labels <- unique(dt2)$id

ddata <- dendro_data(result, type = "rectangle")

labs <- as.data.table(ddata$labels)
labs$id = labs$label
setkey(labs,id)
tmp_dt <- unique(dt2)[,.(id,taxon_id,taxon)]
tmp_dt[,id :=as.character(id)]

setkey(tmp_dt,id)

labs <- tmp_dt[labs]
labs[,label2 := paste(taxon_id,label,sep="_")]

labs[,taxon2 := gsub(" ", "\n", taxon)]
labs[,taxon2 := paste(taxon_id,taxon2)]



ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = labs, 
            aes(x = x, y = y, label = label2,
                colour=taxon2), 
            vjust = 0.5, hjust=0) + 
  coord_flip() + scale_y_reverse(expand = c(0.2, 0)) +
  theme_dendro()


all_annotations = getAllAnnotationData()
query = all_annotations[author == "qgeissmann"]
query[id %in% c(286, 290, 291),.(taxon, taxon_id)]

library(datasets)
library(ggplot2)

# masukkan datasetku
datasetku <- datasets::CO2
# fungsi cuberoot
cbrt <- function(x){return(x^(1/3))}
# fungsi struge rule buat methode menentukan angka binning
struge =function(df,kolomku){
  return(1 + ceiling(3.322*log10(length(df[[kolomku]]))))
}
# fungsi rice rule buat methode menentukan angka binning
rice =function(df,kolomku){
  return(ceiling(cbrt(length(df[[kolomku]]))))
}
# fungsi fredmann-diconis rule buat methode menentukan angka binning
fredik <- function(dfku, kolomku){
  seriesku <- dfku[[kolomku]]
  banyak_obs <- length(seriesku)
  widthku <- 2*IQR(seriesku) / cbrt(banyak_obs)
  binariku <- ceiling((max(seriesku) - min(seriesku)) / widthku)
  return(binariku)
}

# buat csv
# write.csv(datasetku,file="CO2.csv",quote=FALSE,row.names = FALSE)

# plotting di histogram kolom conc
hist_conc<- ggplot(datasetku,aes(conc,label=..count..)) + 
              geom_histogram(bins=fredik(datasetku,"conc"),
                             color="black",
                             fill="white") +
              ggtitle("histogram concentration (CO2)") +
              geom_text(stat="bin", size=5,vjust=-0.1,
                        bins=fredik(datasetku,"conc"))
              
hist_conc
# mengikuti weka
hist_conc_weka <- ggplot(datasetku,aes(conc,label=..count..)) + 
                    geom_histogram(breaks=c(95,321.25,547.5,773.75,1000),
                                   color="black",
                                   fill="white") +
                    ggtitle("histogram concentration (CO2) with weka")+
                    geom_text(stat="bin", size=5,vjust=-0.1,
                              breaks=c(95,321.25,547.5,773.75,1000))
hist_conc_weka
# plotting di histogram kolom uptake
hist_uptake <- ggplot(datasetku,aes(uptake,label=..count..)) + 
                geom_histogram(bins = fredik(datasetku,"uptake"),
                               color="black",
                               fill="white") +
                ggtitle("histogram uptake(CO2)") +
                  geom_text(stat="bin", size=5,vjust=-0.1,
                    bins=fredik(datasetku,"uptake"))

hist_uptake
# mengikuti weka
hist_uptake_weka <- ggplot(datasetku,aes(uptake,label=..count..)) + 
                      geom_histogram(breaks = c(7.7,17.15,26.6,36.05,45.5),
                                     color="black",
                                     fill="white")+
                      ggtitle("histogram uptake(CO2) with weka")+
                      geom_text(stat="bin", size=5,vjust=-0.1,
                                breaks = c(7.7,17.15,26.6,36.05,45.5))
hist_uptake_weka
# plotting di scatterplot conc vs uptake
scat_conc_uptake <- ggplot(datasetku,aes(conc,uptake)) + 
                    geom_point()+
                    ggtitle('Scatterplot concentration vs uptake (CO2) ')
scat_conc_uptake

ggsave('LKP2_hist_conc.jpg',plot=hist_conc)
ggsave('LKP2_hist_uptake.jpg',plot=hist_uptake)
ggsave('LKP2_hist_conc_weka.jpg',plot=hist_conc_weka)
ggsave('LKP2_hist_uptake_weka.jpg',plot=hist_uptake_weka)
ggsave('LKP2_scat_conc_uptake.jpg',plot=scat_conc_uptake)
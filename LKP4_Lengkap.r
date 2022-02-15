infotheokrasi <- system.file(package = "infotheo")
if(infotheokrasi != ""){
  library(infotheo)
}else{
  install.packages("infotheo")
  library(infotheo)
}

tikus <- system.file(package = "mice")
if(tikus != ""){
  library(mice)
}else{
  install.packages("mice")
  library(mice)
}

visvisdat <- system.file(package = "visdat")
if(visvisdat != ""){
  library(visdat)
}else{
  install.packages("visdat")
  library(visdat)
}

ggg <- system.file(package = "ggplot2")
if(ggg != ""){
  library(ggplot2)
}else{
  install.packages("ggplot2")
  library(ggplot2)
}


#NO 1
dataset1 <- read.csv('Product.csv')
md.pattern(dataset1)
dataset1$Weight[is.na(dataset1$Weight)] <- mean (dataset1$Weight,na.rm= TRUE)
dataset1$Color[is.na(dataset1$Color)] <- names(sort(-table(dataset1$Color))[1])
dataset1$Size[is.na(dataset1$Size)] <- names(sort(-table(dataset1$Size))[1])

jpeg("mdvis_1.jpeg")
md.pattern(dataset1)
dev.off()

jpeg("mdvis_1.jpeg")
vis_miss(dataset1) + theme(axis.text.x = element_text(angle=85))
dev.off()


#NO 2
#dibuat x menjadi variabel sementara terlebih dahulu

x <-discretize(dataset1$StandardCost,'equalwidth',4)
dataset1$StandardCost_discret <- x$X

x <-discretize(dataset1$ListPrice,'equalwidth', 4)
dataset1$ListPrice_discret <- x$X

x <-discretize(dataset1$Weight,'equalwidth', 3)
dataset1$Weight_discret <- x$X

#NO3
dataset1$PriceGap <- abs(dataset1$ListPrice - dataset1$StandardCost)

#NO4
dataset2 <- read.csv("ProductSubcategory.csv")
dataset12 <- merge(dataset1,dataset2)

# Perlu di Prepocessing ulang dikarenakan 
# adanya nilai "" pada suatu kolom di dataset2

kolom_dt12_kosong <- c()
for(ii in names(dataset12)){
  if("" %in% unique(dataset12[[ii]])  ){
    kolom_dt12_kosong <- c(kolom_dt12_kosong,ii)
    dataset12[[ii]] <- replace(dataset12[[ii]],dataset12[[ii]]=="",NA)
  }
}

# Berdasarkan Link ini
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701793/#:~:text=Proportion%20of%20missing%20data,-The%20proportion%20of&text=Yet%2C%20there%20is%20no%20established,5%25%20or%20less%20is%20inconsequential.
# dikarenakan semua data kosong tersebut lebih dari 10% maka 
# kolom yang ada di  variabel kolom_dt12_kosong akan dihapus

jpeg("vis_miss_NA.jpeg",quality=100)
vis_miss(dataset12) + theme(axis.text.x = element_text(angle=85))
dev.off()

dataset12 <- dataset12[,!(names(dataset12) %in% kolom_dt12_kosong)]

jpeg("vis_miss_final.jpeg",quality=100)
vis_miss(dataset12) + theme(axis.text.x = element_text(angle=85))
dev.off()

write.csv(dataset12,"Product_Final.csv", row.names = FALSE)
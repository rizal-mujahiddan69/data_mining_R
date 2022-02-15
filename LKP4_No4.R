library(visdat)
library(mice)
library(ggplot2)

dataset1 <- read.csv("Product.csv")
md.pattern(dataset1)
str(dataset1)

dataset1$Size <- as.numeric(dataset1$Size)

dataset1$Color[is.na(dataset1$Color)] <- mode(dataset1$Color)
dataset1$Size[is.na(dataset1$Size)]  <-  mean(dataset1$Size,na.rm=TRUE)
dataset1$Weight[is.na(dataset1$Weight)]  <-  mean(dataset1$Weight,na.rm=TRUE)




dataset2 <- read.csv("ProductSubcategory.csv")


dataset12 <- merge(dataset1,dataset2,by = "ProductID")




md.pattern(dataset2)
visdat::vis_miss(dataset2)
View(dataset2)

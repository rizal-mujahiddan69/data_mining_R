library(fpc)
library(datasets)
library(mlbench)


dataset <- data.frame(read.csv('Vehicles.csv'))

#Mengisi NA
for(i in 1:ncol(dataset)) {       # Replace NA in all columns
  dataset[ , i][is.na(dataset[ , i])] <- mean(dataset[ , i], na.rm = TRUE)
}

dataset2 <- dataset


#Clustering
dataset2$Class <- NULL
(kmeans.result <- kmeans (dataset2,4))

#perbandingan
table(dataset$Class, kmeans.result$cluster)

#Plot
plot(dataset2[c("Comp", "Circ")],col=kmeans.result$cluster)
points(kmeans.result$centers[,c("Comp","Circ")], col = 1:3,pch = 8, cex=2)


dataset2$Class <- NULL
pamk.result <- pamk(dataset2)

pamk.result$nc
table(pamk.result$pamobject$clustering, dataset$Class)
layout(matrix(c(1,2),1,2))
plot(pamk.result$pamobject)
layout(matrix(1))


data("Glass")
data_glass <- Glass

samp_idx <- sample(1:dim(data_glass)[1],40)
data_sample <- data_glass[samp_idx,]

data_sample$Type <- NULL

hc_ave <- hclust(dist(data_sample),method="average")
plot(hc_ave,hang = -1, labels = data_glass$Type[samp_idx],
     main="Average link")

hc_sing <- hclust(dist(data_sample),method="single")
plot(hc_sing,hang = -1, labels = data_glass$Type[samp_idx],
     main="Single link")

hc_comp <- hclust(dist(data_sample),method="complete")
plot(hc_comp,hang = -1, labels = data_glass$Type[samp_idx],
     main="Complete link")
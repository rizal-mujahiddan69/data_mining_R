library(datasets)
library(mlbench)
library(dendextend)

data("Glass")
datasetku <- Glass

data_sample_idx <- sample(1:dim(datasetku)[1],40)
data_sample <- datasetku[data_sample_idx,]

data_sample$Type <- NULL

hc_ave <- hclust(dist(data_sample),method="average")
plot(hc_ave,hang = -1, labels = datasetku$Type[data_sample_idx],
     main="Average link")

hc_sing <- hclust(dist(data_sample),method="single")
plot(hc_sing,hang = -1, labels = datasetku$Type[data_sample_idx],
     main="Single link")

hc_comp <- hclust(dist(data_sample),method="complete")
plot(hc_comp,hang = -1, labels = datasetku$Type[data_sample_idx],
     main="Complete link")

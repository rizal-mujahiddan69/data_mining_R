library(ggplot2)
library(tidyverse)
library(ggExtra)
dt_mobil <-read.csv('usedcars.csv')
print(dt_mobil)

head(dt_mobil)


ggplot(gather(dt_mobil[,c(-2,-5,-6)]),aes(value)) +
  geom_histogram(bins=10) + 
  facet_wrap(~key,scale='free_x')

ggplot(gather(dt_mobil[,c(2,5,6)]),aes(value)) +
  geom_bar()+facet_wrap(~key,scale='free_x')

g1_no5 <- ggplot(data = dt_mobil,aes(year,price)) + geom_point()
g2_no5 <- ggplot(data = dt_mobil,aes(mileage,price)) + geom_point()
g3_no5 <- ggplot(data = dt_mobil,aes(x=model,y=price)) + 
          geom_point()
g4_no5 <- ggplot(data = dt_mobil,aes(x=color,y=price)) + 
          geom_point()
g5_no5 <- ggplot(data = dt_mobil,aes(x=transmission,y=price)) + 
          geom_point()

gridExtra::grid.arrange(g1_no5,g2_no5,g3_no5,g4_no5,g5_no5,nrow=3,ncol=2)


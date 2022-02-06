library(ggplot2)
library(tidyverse)
library(ggExtra)
library(gridExtra)
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

g1_no5 <- ggplot(data = dt_mobil,aes(year,price)) + geom_point()
g2_no5 <- ggplot(data = dt_mobil,aes(mileage,price)) + geom_point()
g3_no5 <- ggplot(data = dt_mobil,aes(x=model,y=price)) + 
          geom_point()
g4_no5 <- ggplot(data = dt_mobil,aes(x=color,y=price)) + 
          geom_point()
g5_no5 <- ggplot(data = dt_mobil,aes(x=transmission,y=price)) + 
          geom_point()

grid.arrange(g1_no5,g2_no5,g3_no5,g4_no5,g5_no5,nrow=3,ncol=2)

head(dt_mobil)

bxplt_color_price <- ggplot(data = dt_mobil,aes(x=as.factor(color),
                                                y=price,
                                                fill=as.factor(color))) + 
                      geom_boxplot() + theme(legend.position="none") 
bxplt_model_price <- ggplot(data = dt_mobil,aes(x=model,y=price,
                                                fill=model)) + 
                      geom_boxplot() + theme(legend.position="none")
bxplt_year_price <- ggplot(data = dt_mobil,aes(x=as.factor(year),y=price,
                                               fill=as.factor(year))) + 
                    geom_boxplot()+theme(legend.position="none")
bxplt_trans_price <- ggplot(data = dt_mobil,aes(x=as.factor(transmission),
                                                y=price,
                                                fill=as.factor(transmission))) + 
                      geom_boxplot()+theme(legend.position="none")
bxplt_millage <- ggplot(data = dt_mobil,aes(y=mileage)) + 
                geom_boxplot() +theme(legend.position="none")


gridExtra::grid.arrange(bxplt_model_price,bxplt_year_price,
             bxplt_color_price,bxplt_trans_price,bxplt_millage,nrow=3,ncol=2)
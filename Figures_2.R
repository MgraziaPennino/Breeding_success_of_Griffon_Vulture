

###################################################################################3
#Read library
library(ggplot2)
library(cowplot)

#Read data
data <- read.csv("~/Downloads/Figure_2/data_nests.csv", sep=";")
colnames(data)=c("Year","Occupied","Successful")

#Plot temporal series

ggplot() + geom_line(data=data,aes(x=Year,y=Occupied,color='Occupied')) +  
  geom_line(data=data,aes(x=Year,y=Successful,color='Successful')) +  
   ylab('Number of nests')+xlab('Year')+theme_bw()

ggsave("Figure_2.jpeg")


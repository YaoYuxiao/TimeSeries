library(tidyverse)
library(ggplot2)
library(dplyr)

raw_data=read_csv("CO2.csv")

str(raw_data)

plot=ggplot(raw_data,aes(Year.Deci,Interpolated))+
  geom_line(size=1,color='darkgreen')+
  scale_x_continuous(breaks = seq(1958,2019,5))

plot

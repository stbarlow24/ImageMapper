### Playing with transient FEEM data 9.5.19 ###

library(plyr) ###File manipulation
library(dplyr) ###File manipulation
library(ggplot2) ###plotting
library(gridExtra) ###Tabulating data
library(zoo)  ###not used in this, but mathematical functions
library(ggthemes) ###other ggplot2 graph themes
library(DescTools) ###Calculate Area under curve (AUC)

####Set the directory manually for extracting CV data####
setwd("C:\\Users\\stbar\\Desktop\\Diffusion limited FEEM PeterTodd\\Test imageJ file path #3\\testing rename")


filenames<- list.files(pattern="ROI", full.names=TRUE)


read_csv_filename <- function(filename){
  ret <- read.csv(filename)
	colnames(ret)[1]<- "Slice_Number"
  colnames(ret)[2]<- "Intensity"
	ret$Time<- ret$Slice_Number*0.03 #30 ms exposure time
	ret$Source <- filename
	ret$dIntensity<- c(0, diff(ret$Intensity, lag=1))
	ret$dTime<- c(0, diff(ret$Time, lag=1))
	ret
}

#File structure= ROI000.csv rename files here


my.data_test<- ldply(filenames, read_csv_filename)
my.data_test$ElectrodeID<-gsub(pattern=("[a-z \\. \\/]"), "", my.data_test$Source)
head(my.data_test)

  
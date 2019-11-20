###Streamlined peak detection and assignment; generalized
my.theme<- theme(plot.title=element_text(size=30, family="Serif"), 
              plot.subtitle=element_text(size=24, family="Serif"),
              axis.line = element_line(colour = "black",size=1.0),
              axis.text.x=element_text(colour="black", size=8, family="Serif",angle=60),
              axis.text.y=element_text(colour="black", size=8, family="Serif"),
              axis.title=element_text(colour="black", size=26, family="Serif"), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position="right",
              legend.text=element_text(colour="black", size=14, family="Serif"),
              strip.text.x= element_text(colour="black", size=12, family="Serif"),
              axis.ticks.x=element_line()
)
  
mainDir <- "C:\\Users\\stbar\\Desktop\\Diffusion limited FEEM PeterTodd\\Figures"
subDir <- "Data Analysis of FEEM Data 9.10.19"

##Setting the Working directory for the day's graphs

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}

###Implementing thresholding algorithm in my dataset

ThresholdingAlgo <- function(y,lag,threshold,influence) {
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag])
  stdFilter[lag] <- sd(y[0:lag])
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i])
    stdFilter[i] <- sd(filteredY[(i-lag):i])
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}

##Assign dataframe
y <- my.data_test$Intensity  
lag       <- 100  ##How many points to roll-mean to determine threshold
threshold <- 7   ##factor by which to multiply Standard deviation; e.g. 5 = 5*sigma
influence <- 0   ##How the algorithm should react to signals.  between 0 and 1 where 0 = no influence on threshold, 1 = maximum influence on threshold


##Visualize results
result <- ThresholdingAlgo(y,lag,threshold,influence)
result$signals[1000:1800] #<1000 points can be displayed at a time. signals stored in results as result$signal
# Plot result
par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
plot(1:length(y),y,type="l",ylab="",xlab="") 
lines(1:length(y),result$avgFilter,type="l",col="cyan",lwd=2)
lines(1:length(y),result$avgFilter+threshold*result$stdFilter,type="l",col="green",lwd=2)
lines(1:length(y),result$avgFilter-threshold*result$stdFilter,type="l",col="green",lwd=2)
plot(result$signals,type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)


##re-unite signals identified (0 or 1 where 1 = signal hit) with data.frame of results

my.data_peaksFound<- bind_cols(my.data_test, result)
head(my.data_peaksFound)
my.data_peaksFound$signals

my.data_check<- my.data_test %>%
  group_by(ElectrodeID)
##split recombine method

##Use a split-recombine approach - separate out only the signal hits;
my.data_peaksOnly<- my.data_peaksFound %>%
  filter(signals == 1) %>%
  group_by(ElectrodeID) %>%
  mutate(PeakIncrement = cumsum(c(1, as.numeric(diff(Slice_Number)>1))))

my.data_peaksOnly
##label all the hits as an incrementing vector (i.e. 1,1,1,0,0,1,1 would indicate 2 separate signals.  Peak number vector would read: 1,1,1,NA,NA, 2, 2)
  y.data_peaksChange$PeakIncrement<- cumsum(c(1, as.numeric(diff(my.data_peaksOnly$Slice_Number)>1)))

##Recombine incrementing vector with "signals only dataset" 
##my.data_peaksOnly<- bind_cols(my.data_peaksOnly, my.data_peaksChange)

##Recombine with original data set, remove NA
my.data_allData<- full_join(my.data_peaksFound, my.data_peaksOnly)
head(my.data_allData)
my.data_allData[is.na(my.data_allData)]<- 0
head(my.data_allData)

##Label peak numbers using a string variable for later sorting/plotting
my.data_allData$PeakNumber<- ifelse(my.data_allData$PeakIncrement>0, 
                                              paste("Peak",my.data_allData$PeakIncrement), 
                                              paste("No Peak"))
#write.csv( my.data_allData, "AaronTestFile.csv")
##Plot signals and Raw intensities to assess quality of data

Intensity.plot<-ggplot(my.data_allData, aes(x=Time, y=Intensity))
Intensity.plot<- Intensity.plot+
  geom_line(size=1, colour="black")+
  labs(	x="Time(s)",
        y="Intensity (a.u)",
        title="",
        subtitle="")+
  my.theme
Intensity.plot

ggsave(filename="AllPeaks.Intensity.plots.tiff",plot=Intensity.plot, device="tiff",dpi=600, units="in",width=5,height=5)

Signal.plot<-ggplot(my.data_allData, aes(x=Time, y=signals))
Signal.plot<- Signal.plot+
  geom_line(size=1, colour="red")+
  labs(	x="Time(s)",
        y="Binary (on=1, off=0)",
        title="",
        subtitle="")+
  my.theme
Signal.plot

ggsave(filename="AllSignals.Binary.plots.tiff",plot=Signal.plot, device="tiff",dpi=600, units="in",width=5,height=5)


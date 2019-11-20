##A script to develop a faithful representation of imagemap where pixels are regions of interest color coded according to external data analysis


library(plyr) ###File manipulation
library(dplyr) ###File manipulation
library(ggplot2) ###plotting
library(gridExtra) ###Tabulating data
library(zoo)  ###not used in this, but mathematical functions
library(ggthemes) ###other ggplot2 graph themes
library(DescTools) ###Calculate Area under curve 
library(reshape2)


setwd("C:\\Users\\stbar\\Desktop\\Diffusion limited FEEM PeterTodd\\ROI map test")


filenames<- list.files(pattern="Roi", full.names=TRUE)

ROImap<- as.matrix(read.table("Roi Map.txt")) ###Read in Image Map.txt
ROImap

ROImap.melted<- melt(ROImap, value.name = "ROI.value", varnames = c("y","x")) ##Convert data to long format
head(ROImap.melted)


ROImap.relabeled<- ROImap.melted %>%
  mutate(ElectrodeID= paste0("ROI", sprintf("%03d",ROI.value)))## add string identifier

head(ROImap.relabeled$ROIlabel)

##Merge datasets

head(my.data_allData) ##Requires running File Manipulator and Peak Detection prior

foundPeaks.summary<- my.data_allData %>%
  filter(PeakNumber != "No Peak") %>%
  group_by(ElectrodeID, PeakNumber) %>%
  summarise(Imax = max(Intensity),
            Duration=(length(Time)*0.03),
            AUC = AUC(Time,Intensity))
foundPeaks.summary

ROI.characteristics<- foundPeaks.summary %>%
  group_by(ElectrodeID) %>%
  summarise(PeakFrequency = n_distinct(PeakNumber),
            CumulativeIntensity = max(cumsum(Imax)),
            CumulativeTime = max(cumsum(Duration)),
            CumulativeAUC = max(cumsum(AUC)))
ROI.characteristics

ROImap.merged<- left_join(ROImap.relabeled,ROI.characteristics, by = "ElectrodeID")
ROImap.merged[is.na(ROImap.merged)]<-0
    

head(ROImap.merged)

        
##fuckin did it

###Streamlined peak detection and assignment; generalized
my.theme<- theme(plot.title=element_text(size=30, family="Serif"), 
                 plot.subtitle=element_text(size=24, family="Serif"),
                 axis.line = element_line(colour = "black",size=1.0),
                 axis.text.x=element_blank(),# element_text(colour="black", size=8, family="Serif",angle=60),
                 axis.text.y=element_blank(),#element_text(colour="black", size=8, family="Serif"),
                 axis.title=element_text(colour="black", size=26, family="Serif"), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.position="right",
                 legend.text=element_text(colour="black", size=14, family="Serif"),
                 strip.text.x= element_text(colour="black", size=12, family="Serif"),
                 axis.ticks.x=element_line()
)

mainDir <- "C:\\Users\\stbar\\Desktop\\Diffusion limited FEEM PeterTodd\\Figures"
subDir <- "Data Analysis of FEEM Data 9.11.19"

##Setting the Working directory for the day's graphs

if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}
##plotting various maps

Frequency.map<- ggplot(ROImap.merged,aes(x=x, y=y))+ 
  geom_raster(aes(fill=PeakFrequency), interpolate=TRUE)+
  scale_fill_viridis_c(option = "inferno")+
  labs(x="", 
       y="")+
  scale_y_reverse()+
  theme_tufte()+
  my.theme
Frequency.map


ggsave(filename="Frequency_vs_ElectrodeIDmap.tiff",plot=Frequency.map, device="tiff",dpi=600, units="in",width=6.58,height=4.96)

Intensity.map<- ggplot(ROImap.merged,aes(x=x, y=y))+ 
  geom_raster(aes(fill=CumulativeIntensity), interpolate=TRUE)+
  scale_fill_viridis_c(option = "inferno")+
  labs(x="", 
       y="")+
  scale_y_reverse()+
  theme_tufte()+
  my.theme
Intensity.map


ggsave(filename="Intensity_vs_ElectrodeIDmap.tiff",plot=Intensity.map, device="tiff",dpi=600, units="in",width=6.58,height=4.96)


Duration.map<- ggplot(ROImap.merged,aes(x=x, y=y))+ 
  geom_raster(aes(fill=CumulativeTime), interpolate=TRUE)+
  scale_fill_viridis_c(option = "inferno")+
  labs(x="", 
       y="")+
  scale_y_reverse()+
  theme_tufte()+
  my.theme
Duration.map


ggsave(filename="Duration_vs_ElectrodeIDmap.tiff",plot=Duration.map, device="tiff",dpi=600, units="in",width=6.58,height=4.96)



AUC.map<- ggplot(ROImap.merged,aes(x=x, y=y))+ 
  geom_raster(aes(fill=CumulativeAUC), interpolate=TRUE)+
  scale_fill_viridis_c(option = "inferno")+
  labs(x="", 
       y="")+
  scale_y_reverse()+
  theme_tufte()+
  my.theme
AUC.map


ggsave(filename="AUC_vs_ElectrodeIDmap.tiff",plot=AUC.map, device="tiff",dpi=600, units="in",width=6.58,height=4.96)




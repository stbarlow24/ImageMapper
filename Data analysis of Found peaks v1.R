##Have all peaks identified.  Now need to quantify Imax, duration, area


head(my.data_allData)

foundPeaks.summary<- my.data_allData %>%
    filter(PeakNumber != "No Peak") %>%
    group_by(ElectrodeID, PeakNumber) %>%
    summarise(Imax = max(Intensity),
              Duration=(max(Time)-min(Time)),
              AUC = AUC(Time,Intensity)) %>%
    filter(Duration > 0)#a.u. 
foundPeaks.summary

###Imax vs Duration
foundPeaks.plot<-ggplot(foundPeaks.summary, aes(x=Duration, y=Imax))
foundPeaks.plot<- foundPeaks.plot+
  geom_point(size=1, colour="red")+
  labs(	x="Duration (ms)",
        y="Maximum Intensity (a.u)",
        title="",
        subtitle="")+
  my.theme
foundPeaks.plot

ggsave(filename="Duration_vs_MaxIntens.plots.tiff",plot=foundPeaks.plot, device="tiff",dpi=600, units="in",width=5,height=5)

##Auc vs Duration
foundPeaks.plot<-ggplot(foundPeaks.summary, aes(x=Duration, y=AUC))
foundPeaks.plot<- foundPeaks.plot+
  geom_point(size=1, colour="red")+
  labs(	x="Duration (ms)",
        y="Area of Peaks (a.u)",
        title="",
        subtitle="")+
  my.theme
foundPeaks.plot

ggsave(filename="Duration_vs_AUC.plots.tiff",plot=foundPeaks.plot, device="tiff",dpi=600, units="in",width=5,height=5)

##AUC vs IMax
foundPeaks.plot<-ggplot(foundPeaks.summary, aes(x=Imax, y=AUC))
foundPeaks.plot<- foundPeaks.plot+
  geom_point(size=1, colour="red")+
  labs(	x="Maximum Intensity (a.u.)",
        y="Area of Peaks (a.u)",
        title="",
        subtitle="")+
  my.theme
foundPeaks.plot

ggsave(filename="Imax_vs_AUC.plots.tiff",plot=foundPeaks.plot, device="tiff",dpi=600, units="in",width=5,height=5)




###Now, determine event frequency and tabulate data

foundPeaks.frequency<- foundPeaks.summary %>%
      group_by(ElectrodeID) %>%
      summarise(PeakFrequency = n_distinct(PeakNumber))
foundPeaks.frequency

##Visualize peak frequency
foundPeaks.frequency.plot<-ggplot(foundPeaks.frequency, aes(x=ElectrodeID, y=PeakFrequency))
foundPeaks.frequency.plot<- foundPeaks.frequency.plot+
  geom_point(size=1, colour="red")+
  labs(	x="Electrode ID",
        y="Peak Frequency",
        title="",
        subtitle="")+
  my.theme
foundPeaks.frequency.plot

ggsave(filename="Frequency_vs_ElectrodeID.tiff",plot=foundPeaks.frequency.plot, device="tiff",dpi=600, units="in",width=5,height=5)

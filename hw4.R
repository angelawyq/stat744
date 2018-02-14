library(tidyverse)
library(directlabels)
library(readxl)
#This package is used to read 
#library(ggplot2)
#library(dplyr)
#library(readr)
#From last assignment, I learned that tidyverse will automatically load the above three
theme_set(theme_bw())
concentration<-read_excel("Concentrations.xlsx")

(concentration %>% mutate(conc.avg=rowMeans(data.frame(Concentration1,Concentration2,Concentration3)))
)-> concentration_avg
#Here I have calculated the average of the three measures for each compound in one site
#View(concentration_avg)
gg1 <- (ggplot(concentration_avg,aes(x=Site, y=conc.avg+1, colour=Compound)) + 
          geom_point(aes(color=Compound),size=1) +
          geom_line(aes(group=Compound))+
          ylab("Concentrations (ng/L)") +
          xlab("Site") + 
          ggtitle("Estimated time-weighted concentrations of 24 Compounds from different sites") +
          theme(plot.title = element_text(hjust=0.5))+
          geom_dl(aes(label=Compound),method=list("top.bumptwice",cex=0.7))+
          scale_x_discrete()+
          scale_y_log10(breaks=c(1,10,100,1000),labels=c(1,10,100,1000))+
          facet_wrap(~Class)
)
print(gg1+theme(legend.position="none"))

#This graph is trying to compare the concentrations of each compound from four different sites.
#We can certainly observe the trend that the concentration is much higher from wasterwater treatment
#plants(WWTP) compared to the control. The other two wastewater-exposed sites, Outfall and Downstream
#both have higher levels of concentrations of these compounds.
#The graph compared to the table did lose specific information of the concentration at each site.
#But I think the graph does a better job of getting the message across to reader that there is a general
#trend that the concentrations of the compounds are much higher at the wastewater-exposed sites.
                        
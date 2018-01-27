## library(ggplot2)
library(tidyverse)
## BMB: tidyverse loads dplyr, reader, ggplot2 by default
## library(dplyr)
## library(readr)
library(directlabels)
library(faraway)  ## what for? helpful to comment
theme_set(theme_bw())
dat<-read_csv("https://bbolker.github.io/stat744/data/vaccine_data_online.csv")

#In my opinion, the graph is trying to explore the effect of vaccines. 
#It shows that after the introduction of vaccine of one disease, 
#the number of cases of that disease will decrease dramatically.
#This shows that vaccines can do more good than harm.


## BMB: +1 for reordering
(dat %>% filter(cases>0) %>%
 mutate(disease=fct_reorder(f=disease,x=year,fun=min))) -> dat
## BMB: please comment these out
##View(dat)
options(scipen = 100000)
## BMB: how did you pick this palette?
cbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
gg1 <- (ggplot(dat,aes(x=year, y=cases, colour=disease))+ 
          geom_line() + 
          geom_point(data=dat[dat$vaccine != "FALSE",],shape=18,size=3) +
          ylab("Number of Reported Cases in U.S. \n (Log transformed)") +
          xlab("Year") + 
          ggtitle("The Power of Vaccines") +
          theme(plot.title = element_text(hjust = 0.5))+
          scale_y_log10()+
          scale_x_continuous(breaks=seq(1945,2015,by=10))+
          facet_wrap(~disease)
)


print(gg1+theme(legend.position="none")+
      ## BMB: good.  I would (1) squeeze panels together
      ## (although this also causes problems with tick label overlaps)
      ## theme(panel.spacing=grid::unit(0,"lines"))+
      scale_color_manual(values=cbPalette))

## BMB: colour is redundant

#In my opinion, my graph does not provide the same amount of information
#as the original graph, in fact, much less. However, my graph is somewhat
#straight forward, and readers can see the decrease clearly for every 
#disease, which fulfills the goal of informing the readers that vaccines
#have been helping with eliminating diseases.

## BMB: why less information? other than dynamic graphics, and labels for events,
##  isn't this the *same* information?
## note that you say "(log transformed)", I think you mean "(log scale)", since
## you haven't actually transformed the data ...
## colour is redundant (but pretty)
## note that mumps vaccine indicator 

#I read the comments from HW1, and I took a lot of useful advice.
#First, I suppress the legend in this graph since it doesn't provide information.
#Secondly, I chose a color palette that is friendly for color-blinded people.

#Also, I really want to add the text saying that the dots on each line
#representing the year for vaccine licence. Unfortunately, 
#I did not have enough time to figure out the code to do that. 

## BMB: how about ...
print(gg1+theme(legend.position="none")+
      labs(caption="dots on each line represent the year of vaccine licensing")+
      scale_x_continuous(breaks=seq(1950,2010,by=10))+
      theme(panel.spacing=grid::unit(0,"lines"))+
      scale_color_manual(values=cbPalette))

log.cases<-log(dat$cases)
gg2 <- (ggplot(dat,aes(y=year,x=disease,fill=disease,width=log.cases))+
    geom_violin(stat="identity",aes(x=log.cases)) +
    scale_y_continuous(breaks=seq(1945,2015,by=5)) +
    coord_flip()+
    ylab("Year") +
    xlab("Disease")+
    ggtitle("The Power of Vaccines") +
    theme(plot.title = element_text(hjust = 0.5))
)
print(gg2+
      theme(legend.position="none")+
      scale_fill_manual(values=cbPalette))

#I know this is not an informative graph at all. 
#I think that the violin graph here is probably not a good way 
#to show that vaccines have a huge impact on the reduction of the 
#number of reported cases of these diseases.
#The violin does not do a good job in reflecting the reduced cases(no matter
#it is the original cases or log-transformed cases).
#I could not fit the point for vaccine license in there, so the graph
#lost the purpose of showing the effect of vaccines.

## BMB: it's a little weird, but it is at least interesting/visually attractive
## there might be a way to get the "violin" shape with raw values, but it's
## not super-easy.  I'm not sure what this transformation actually does ...

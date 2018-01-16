library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(faraway)
theme_set(theme_bw())

#To be honeset, I don't know whether the following step is necessary, because
#I am not sure whether the data set built in R is already in the data frame setting.
worldcup<-as_data_frame(worldcup)

#This step is to create new variables, so I can plot things that will provide sensible
#information. I want to learn about the average number of passes per minute for each team.
#The total number of passes for each team does not provide useful information since
#the more games that one team played would have resulted in a greater total number of passes.
#I also manually added the continent information for each team, so I can compare the average
#number of passes for the teams from different continents.
(worldcup %>% group_by(Team) %>% summarise(Total_passes=sum(Passes),Total_time=sum(Time))
) -> worldcup_total
worldcup_average<-mutate(worldcup_total, Passes_per_minute=Total_passes/Total_time)
continent<-c("Africa","SouthAmerica","AsiaPacific","SouthAmerica",
             "Africa","SouthAmerica","Europe","Europe",
             "Europe","Europe","Africa","Europe",
             "NorthAmerica","Europe","Africa","AsiaPacific",
             "NorthAmerica","Europe","AsiaPacific","Africa",
             "AsiaPacific","SouthAmerica","Europe","Europe",
             "Europe","Europe","Africa","AsiaPacific",
             "Europe","Europe","NorthAmerica","SouthAmerica")
Continent<-factor(continent)
worldcup_average<-mutate(worldcup_average, Continent=Continent)
(worldcup_average %>% arrange(Continent,Passes_per_minute)) -> worldcup_average

#First graph:
worldcup_average$Team <- factor(worldcup_average$Team,
                                levels = worldcup_average$Team[order(worldcup_average$Continent)])
gg1<-(ggplot(worldcup_average,aes(x=Team,y=Passes_per_minute,colour=Continent))
      +geom_point(size=3)
      +ggtitle("2010 World Cup Average Number of Passes")
      +theme(plot.title = element_text(hjust = 0.5))
      +xlab("Country")
      +ylab("Passes Per Minute")
      +theme(axis.text.x=element_text(size=rel(1.1), angle=90))
      )
print(gg1)
#Things that I want to say about this graph:
#The average number of passes is graphed for each country, but grouped by each continent
#also ordered from lower to higher average number of passes. 
#I think that this is a clean graph, and different colors of the points easily show the
#different continents. Moreover, we can also see that European and South American teams
#have higher average number of passes than other continents.


#Second graph:
worldcup_average$Team <- factor(worldcup_average$Team, 
                                levels = worldcup_average$Team[order(worldcup_average$Passes_per_minute)])
gg2<-(ggplot(worldcup_average,aes(x=Team,y=Passes_per_minute,colour=Continent))
      +geom_point(size=3)
      +coord_flip()
      +ggtitle("2010 World Cup Average Number of Passes")
      +theme(plot.title = element_text(hjust = 0.5))
      +xlab("Country")
      +ylab("Passes Per Minute")
      +theme(axis.text.x=element_text(size=rel(1.1)))
)
print(gg2)
#Things that I want to say about this graph:
#After watching the Rauser on YouTube, where he showed the fuel consumption for 
#different models of cars, he showed a similar graph as this one here.
#The axis is flipped, so the name of the country can be read easier.
#The data point is also ordered nicely, and it is very easy to identify 
#Spain, Argentina, and Brazil as the Top 3 teams. 
#We can also see that with higher number of passes, there are more European teams, which
#suggests that the style of teams from different continents is different. 
#From Rauser's keynote, I also noted that position along a common scale is 
#the highest ranked encoding method, which is what I used to plot this graph here.
#I don't know whether you can say this is an improvement from the first graph.


#Third graph:
gg3<-(ggplot(worldcup,aes(Position,Passes,fill=Position))
      +geom_boxplot(outlier.size=0.8, outlier.colour = "cyan4")
      +ggtitle("2010 World Cup Passes for Different Positions")
      +theme(plot.title = element_text(hjust = 0.5))
)
print(gg3+scale_fill_brewer(palette="Greens"))
#Things that I want to say about this graph:
#I created this bar graph using green colors since soccer reminds
#me of green colors. I also matched the color of the outliers to the 
#green color theme. I am aiming at creating a clean graph showing 
#the difference in the number of passes for players with different positions.
#We can see that goalkeeper have more passes than forward, which is a bit surprising.


#Forth graph
gg4<-(ggplot(worldcup,aes(x=Passes,y=Shots,colour=Position)) + 
        geom_point(aes(shape=Position),size=0.8)
      +ggtitle("2010 World Cup Shots VS. Passes")
      +theme(plot.title = element_text(hjust = 0.5))
)
print(gg4+geom_smooth(method="lm")+facet_wrap(~Position))
#Things that I want to say about this graph:
#This graph is created for these four different positions' shots vs. passes.
#Using a position on a common scale, it is rather obvious that the slope for each
#position is highly different. 



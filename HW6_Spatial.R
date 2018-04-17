library(ggplot2)
library(leaflet)
library(rgdal)

# Download shape file using the code provided in the presentation
tmp <- tempdir()
url <-"http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmp)

# Read the file with the rgdal library in R
world_pop2015=readOGR(dsn=tmp, layer="TM_WORLD_BORDERS_SIMPL-0.3")

#Clean up the population data and transform the population in Million unit
world_pop2015$POP2005[which(world_pop2015$POP2005 == 0)] = NA
world_pop2015$POP2005=as.numeric(as.character(world_pop2015$POP2005))/1000000

#Create a color palette for the map
mybins=c(0,10,20,50,100,200,500,1000,Inf)
mypal=colorBin(palette="PuBuGn", world_pop2015$POP2005,na.color="transparent", bins=mybins)

#Use leaflet to customize the chloropleth map
leaflet(data=world_pop2015) %>% 
  addTiles()  %>% 
  addPolygons(fillColor=~mypal(POP2005),
              fillOpacity=0.8,
              color="grey",
              weight=0.9) %>% 
  addLegend(pal=mypal, values=~POP2005, opacity=0.8,
            title = "Population (M)", position = "bottomleft" )

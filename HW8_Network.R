library(igraph)
library(RColorBrewer)

#create data:
links=data.frame(
  source=c("China","China","Mongolia", "China", "Kazakhstan", "Russia","China", "Malaysia", "Indonesia", "Sri Lanka","Poland"),
  target=c("Pakistan","Mongolia","Russia", "Kazakhstan", "Poland", "Kazakhstan","Malaysia","Indonesia", "Sri Lanka", "Kenya","Greece"),
  importance=(sample(1:3,11,replace=T))
)

nodes=data.frame(Country=c("China","Mongolia","Kazakhstan","Pakistan","Russia","Poland","Greece","Malaysia","Indonesia","Sri Lanka","Kenya"),
                 Continent=c(rep("China",1),rep("Central Asia",3), rep("Europe",3),rep("South Asia",3),rep("Africa",1))
)

#plot:
network=graph_from_data_frame(d=links, vertices=nodes, directed=F)
deg=degree(network, mode="all")

coul = brewer.pal(5, "Set2")
my_color=coul[as.numeric(as.factor(V(network)$Continent))]
plot(network,
     vertex.shape="circle",
     vertex.size=9*deg,
     vertex.color=my_color,
     vertex.frame.color="transparent",
     vertex.label.color="black"
)
text(-1.5,-1.5,"China One Belt One Road Initiative",col="black")

## JD: What is this meant to be a network of? I'm pretty sure the initiative doesn't contain a road from Sri Lanka to Kenya!
## http://www.scmp.com/infographics/article/1874865/infographic-one-belt-one-road
## I think that it is a maritime silk road of 21st century, but I did not include every single country that is involved. 

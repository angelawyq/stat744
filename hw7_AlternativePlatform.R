library(networkD3)
#create data:
links=data.frame(
  source=c("China","Mongolia", "China", "Kazakhstan", "Russia","China", "Malaysia", "Indonesia", "Sri Lanka","Mongolia"),
  target=c("Mongolia","Russia", "Kazakhstan", "Poland", "Kazakhstan","Malaysia","Indonesia", "Sri Lanka", "Kenya","China"),
  my_width=rep(1,length(source))
)

nodes=data.frame(Country=c("China","Mongolia","Kazakhstan","Russia","Poland","Malaysia","Indonesia","Sri Lanka","Kenya"),
  Continent=c(rep("China",1),rep("Central Asia",2), rep("Europe",2),rep("South Asia",3),rep("Africa",1)),
  size=c(80,rep(1,8))
)
links$source=as.numeric(as.factor(links$source))-1
links$target=as.numeric(as.factor(links$target))-1

#plot
forceNetwork(
  Links=links,
  Nodes=nodes,
  NodeID = "Country",
  Group = "Continent",
  Nodesize="size",
  fontSize = 19,
  #arrow=TRUE,
  zoom=TRUE,
  legend=TRUE,
  opacity=0.9
)





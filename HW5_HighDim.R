library(tourr)
library(tidyverse)
library(TeachingDemos)
library(colorspace)
library(skimr)
library(geozoo) #high dim shapes, and codes to generate them
library(randomForest)
library(rlang)
library(devtools)
library(reshape)
library(ash)
library(mlbench)

data(wine,package="gclus")
animate(wine[, 2:10], tour_path=grand_tour(), display=display_xy())
animate_faces(wine[2:10, 2:10], grand_tour(9))
## BMB: not much to see here ?

## BMB: can you say a little bit more about what you did here and why?
## seems clear that points fall near edges/1 face of simplex, but
##  what does that mean?
data(Glass)
glass_sub <- filter(Glass, Type %in% c("1","2","3","5")) %>% 
  mutate(Type=factor(Type))
glass_rf <- randomForest(Type~., data=glass_sub)
votes <- f_composition(glass_rf$votes)
colnames(votes) <- paste0("V", 1:3)
votes1 <- cbind(votes, Type=glass_sub$Type)
sp3 <- simplex(p=3)
colnames(sp3$points) <- paste0("V", 1:3)
sp3$points <- cbind(sp3$points, area=c(5,5,5,5))
sp3$edges <- as.matrix(sp3$edges)
votes2 <- rbind(sp3$points, votes1)
animate(votes[,-4], grand_tour(),
        display_xy(axes = "bottomleft", edges=sp3$edges))

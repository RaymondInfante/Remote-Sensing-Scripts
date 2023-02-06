library(tidyverse)


#Analyze data from SolarLight and AC9

#SolarLight Formula: 
# Kd = (1/z2-z1) (ln ed(z1)/ed(z2))

SolarLight <- function(x1,x2,ed1,ed2){
  depth <- 1/(x2-x1)
  ED <- log(ed1/ed2)
  final <- depth*ED
  
  return(final)
}

#Solar Light Data

SolarLight(1,2,651,555)






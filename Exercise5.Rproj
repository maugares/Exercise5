# Author: Mauro Garcia
# Date: 13/11/2013
# Description: Composit image of taravao and taravao2 to avoid clouds

library(rasta)
data(taravao)
data(taravao2)

composting <- function (object1, object2){
  
  # Get clouds
  cloud1 <- calc(x=object1[[9]], fun=QA2cloud) 
  cloud2 <- calc(x=object2[[9]], fun=QA2cloud)
  
  # NA for clouds
  cloud1 [cloud1==0] <- NA
  cloud2 [cloud2==0] <- NA
  
  # Drop QA layer
  object1_8 <- dropLayer(x=object1, i=9)
  object2_8 <- dropLayer(x=object2, i=9)
  
  # Remove clouds
  
  object1_8 [cloud1 == 1] <- NA
  object2_8 [cloud2 == 1] <- NA
  
  # Calculate mean
  
  # Define the function to calculate the mean
  
  meanRaster <- function(x, y) {
    out <- mean(c(x,y), na.rm=TRUE)
    return(out)
  }
  
  # Define the function to vectorize a function
  
  VRandomFunction <- function(a, b) {
    out <- mapply(FUN=meanRaster, a, b)
    return(out)
  }
  
  # Calculate mean
  
  pixelavg <- overlay (x=object1_8, y=object2_8, fun=VRandomFunction)
  
  plotRGB (pixelavg, 4,5,3)
}

composting (object1=taravao, object2=taravao2)

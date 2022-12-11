### Packages & Libraries
library(dplyr)

### Calling the other scripts to call data
source("/Users/User/Desktop/UPV_DataAcquisition/Project/R/00_download-from-drive.R")
source("/Users/User/Desktop/UPV_DataAcquisition/Project/R/01_check-data.R")

###Creating Data Frame with required variables

data0 <- data.frame(species = ws0$Species)

# Name columns
ws0

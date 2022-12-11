#### Install packages --------------
#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)

#### Packages ------------
library(dplyr)
library(tidyr)
library(tidyverse)

#### List files and source each ---------
list.files("/Users/User/Desktop/UPV_DataAcquisition/Project/R/", full.names = TRUE) %>% map(source)

### Calling the other scripts to call data
#source("/Users/User/Desktop/UPV_DataAcquisition/Project/R/00_download-from-drive.R")
#source("/Users/User/Desktop/UPV_DataAcquisition/Project/R/01_check-data.R")

###Creating Data Frame with required variables

data0 <- data.frame(species = ws0$Species)

# Name columns
ws0

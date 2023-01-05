#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("car")

#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)

#### Calling the other R scripts -------
#Comment this after getting the data
source("R/00_download-from-drive.R")
source("R/01_check-data.R")
rm(i, p, w, ws.legend, ws.observ)

#### Creating duplicate of the original data ------

dt1 <- ws0

#### Adding columns to calculate water content for Aerial material and Root 

dt1 <- mutate(dt1, 
              Aerial_water_content = 
                (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

dt1 <- mutate(dt1, 
              Root_water_content = 
                (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)

### Neat data to work on

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_area","Roots_dry_weight",
                "Root_water_content", "Chlorophyll_content")

num.var <- c("Leaf_area","Roots_dry_weight",
             "Root_water_content", "Chlorophyll_content")

w <- "W6"

dt2 <- dt1 %>% filter(Week == w) %>% select(all_of(parameters))

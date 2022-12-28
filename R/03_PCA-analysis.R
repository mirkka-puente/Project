#### Install packages --------------
#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)

#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(agricolae)

#### Calling the other R scripts ---
#Comment this after getting the data
source("R/00_download-from-drive.R")
source("R/01_check-data.R")
rm(i, p, w)

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

num.var <- c("Soil_humidity","Electrical_conductivity",
                   "Plant_height", "Leaf_number", "Leaf_length",
                   "Leaf_width", "Leaf_area","Chlorophyll_content",
                   "Aerial_fresh_weight", "Aerial_dry_weight",     
                   "Root_length", "Roots_fresh_weight", 
                   "Roots_dry_weight", "Aerial_water_content",
                   "Root_water_content")

dt2 <- select(dt1, all_of(num.var)) %>% drop_na()

#### PCA analysis (princomp) -----
pca.dt2 <- princomp(dt2, cor = TRUE)

# information output
names(pca.dt2)

# summary
summary(pca.dt2)

# correlation between components and data 
round(cor(dt2, pca.dt2$scores), 3)

#COMMENTS
# determine if there is relevance, the closer to 1 then 
# closer related to the data set 

### Screen plot -----------
screeplot(pca.dt2, type = "l", main = "Screenplot for Water stress experiment")
abline(1, 0, col = "red", lty = 2)

plot()



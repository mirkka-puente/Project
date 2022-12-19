#### Install packages --------------
#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)

#### Packages ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

#### Calling the other scripts to call data ---
#source("R/00_download-from-drive.R")
#source("R/01_check-data.R")

#### Creating duplicates of the original data ------

#### 1. Adding a column for aerial water content 
data1 <- ws0  
data1 <- mutate(data1, 
       Aerial_water_content = 
         ((Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight) * 100) 

data1 <- mutate(data1, 
       Root_water_content = 
         ((Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight) * 100) 

data1 <- mutate(data1, 
       Root_water_content = 
        ((Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight) * 100)

#### Plant parameters to measure 

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_number", "Chlorophyll_content",
                'Root_length', 'Aerial_fresh_weight', 
                'Aerial_dry_weight', "Aerial_water_content",
                "Roots_fresh_weight", "Roots_dry_weight",
                "Root_water_content")

data2 <- select(data1, all_of(parameters))

####################### Function to not get division by zero ###################
check_zero_div <- function(n) {
   if (n != 0){
     n
   }else{"NA"}
}

# 1. Should we get ride of the NA?
# 2. Replace the data
# 3. Should we used just water content from both aerial + root
###############################################################################



#### GGPLOTS ----------

for (c in data2[7:14]){
  ggplot(data1, aes(x = Week, y = c, group = PlantId, col = Treatment)) +
    geom_smooth()+
    geom_point()+
    facet_wrap(~Species)
}

#### ANOVA -------
a_leaf_num <- c()
for(c in data2[7:14]){
  a_leaf_num <- c(a_leaf_num, aov(c ~ Treatment+Date, data=data2))
}

for (a in a_leaf_num) {
  summary(a)
}


plot(a_leaf_num, 2)

#### Shaphiro test for normality -------
s_pheight <- shapiro.test(a1_pheight$residuals)
s_pheight

#### Log Data 
data0 <- data0 %>% mutate(Log_pheight = log(Plant_height))

#### ANOVA -------
a2_pheight <- aov(Log_pheight ~ Treatment, data=data0)
summary(a2_pheight)
plot(a2_pheight, 2)
#### Shaphiro test for normality -------
s2_pheight <- shapiro.test(a2_pheight$residuals)
s2_pheight

#### Install packages --------------
#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)

#### Packages ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(agricolae)

#### Calling the other scripts to call data ---
source("R/00_download-from-drive.R")
source("R/01_check-data.R")

#### Creating duplicates of the original data ------

#### 1. Adding a column for aerial water content 
data1 <- ws0  

check_real <- function(dt) {
  for (i in dt){
    if (!is.nan(i)){
      i
    }else{NA}
  }
}


data1 <- mutate(data1, 
            Aerial_water_content = 
              (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

data1 <- mutate(data1, 
       Root_water_content = 
         (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)



#### Plant parameters to measure 

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_number", "Chlorophyll_content",
                'Root_length', 'Aerial_fresh_weight', 
                'Aerial_dry_weight', "Aerial_water_content",
                "Roots_fresh_weight", "Roots_dry_weight",
                "Root_water_content")

numerical_var <- c("Leaf_number", "Chlorophyll_content",
                   'Root_length', 'Aerial_fresh_weight', 
                   'Aerial_dry_weight', "Aerial_water_content",
                   "Roots_fresh_weight", "Roots_dry_weight",
                   "Root_water_content")


table <- as.data.frame(matrix(nrow = 9, ncol = 9))
names(table) <- numerical_var
table$Species <- levels(data1$Species)

for (i in 1:9){
  for(nv in numerical_var){
    k <- 1
    #tf <- data1$Species == c
    #num <- filter(data1, Species == c) %>% select(data1, one_of(nv)) 
    #num2 <- num[[1]]
    table[k, i] <- aov(nv ~ Treatment+Date+Species, data=data1)
    k<- k+1
  }
}


data2 <- select(data1, all_of(parameters))

####################### Function to not get division by zero ###################


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
  a_leaf_num <- c(a_leaf_num, aov(c ~ Treatment+Date+Species, data=data2))
}

for (a in a_leaf_num) {
  summary(a)
}

a_leaf_num <- aov(Leaf_number ~ Treatment+Week, data=data1)
plot(a_leaf_num, 2)


agricol <- agricolae::HSD.test(a_leaf_num, 'Treatment')
plot(agricol)

comparison <- with(data2, agricolae::kruskal(Leaf_number,
                                          Treatment, group=TRUE, 
                                          main="Treatment"))
comparison

#### Shaphiro test for normality -------
s <- shapiro.test(a_leaf_num$residuals)
s

#### Log Data 
data0 <- data0 %>% mutate(Log_pheight = log(Plant_height))

#### ANOVA -------
a2_pheight <- aov(Log_pheight ~ Treatment, data=data0)
summary(a2_pheight)
plot(a2_pheight, 2)
#### Shaphiro test for normality -------
s2_pheight <- shapiro.test(a2_pheight$residuals)
s2_pheight

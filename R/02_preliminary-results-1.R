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

#### Creating duplicate of the original data ------

data1 <- ws0

#### Adding columns to calculate water content 

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


#### variables to be used
numerical_var <- c("Leaf_number", "Chlorophyll_content",
                   'Root_length', 'Aerial_fresh_weight', 
                   'Aerial_dry_weight', "Aerial_water_content",
                   "Roots_fresh_weight", "Roots_dry_weight",
                   "Root_water_content")

#### Week we want to focus on

w <- "W6"

data2 <- data1 %>% filter(Week == w) %>% select(all_of(parameters))

#### Creating a data frame for  final results

col_nam <- c("Species", "Variables", levels(data1$Treatment))
num_col <- length(col_nam)
num_row <- length(levels(data1$Species)) * length(numerical_var)
final_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(final_table) <- col_nam

sp <- levels(data2$Species)[1]
nv <- numerical_var[1]
i <- 1

function_lm <- function(var, dt) {
  r <- lm(dt[[var]] ~ dt$Treatment+Week, data = dt)
}


for(sp in levels(data2$Species)){
  for(nv in numerical_var){
    
    data3 <- filter(data2, Species == sp)
    linear_model <- lm(data3[[nv]] ~ data3$Treatment+Week, data = data3)
    hsd1 <- HSD.test(linear_model, "Treatment")
    letter <- hsd1$groups$groups[order(rownames(hsd1$groups))]
    final_table$Species[i] <- sp
    final_table$Variables[i] <- nv
    final_table[i, levels(data1$Treatment)] <- letter
    
    i <- i + 1
  }
}




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

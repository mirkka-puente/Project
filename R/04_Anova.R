#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("car")

#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(agricolae)
library(car)

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

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_number","Aerial_dry_weight",
                "Roots_fresh_weight", "Aerial_water_content")

num.var <- c("Leaf_number","Aerial_dry_weight",
             "Roots_fresh_weight", "Aerial_water_content")


w <- "W6"

dt2 <- dt1 %>% filter(Week == w) %>% select(all_of(parameters))


### ANOVA with p-values to decide significant week 

#Creating the table to keep the information 
col_nam <- c("Species","Variables", "P_value", "Significant")
num_col <- length(col_nam)
num_row <- length(levels(dt2$Species))  * length(num.var)
anova_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(final_table) <- col_nam

#Variables for the loop
sp <- levels(dt2$Species)[1]
nv <- num.var[1]
i <- 1


for(sp in levels(dt2$Species)){
  for(nv in num.var){
    #Data filtered by species
    dt3 <- dt2 %>% filter(Species == sp)
    dt3 <- na.omit(dt3)
    
    #Anova test
        
    model <- aov(dt3[[nv]] ~ dt3$Treatment, data = dt3)
        
    #Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
      
      
    #Allocating the values in the table
    if (p < 0.05){
      anova_table$Species[i] <- sp
      anova_table$Variables[i] <- nv
      anova_table$P_value[i] <- p
      anova_table$Significant[i] <- "Yes"
    } else {
      anova_table$Species[i] <- sp
      anova_table$Variables[i] <- nv
      anova_table$P_value[i] <- p
      anova_table$Significant[i] <- "No"
    }
    
    i <- i + 1
    rm(dt3)
  }
}

anova_table <- anova_table %>% filter(Significant == "Yes")

#Remove variables that are not gonna be used
rm(sp, nv, i, p)



### Bonferroni test

b.test <- pairwise.t.test(dt2$Leaf_number, dt2$Treatment, p.adjust.method="bonferroni")




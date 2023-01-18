#### Install packages --------------
#remotes::install_github('vqv/ggbiplot')
#packs <- c("plyr","dplyr", "tidyr", "tidyverse","ggplot2",
#           "factoextra", "psych", "ggpubr", "car")
#install.packages(packs, dependencies = TRUE)

#install_github("vqv/ggbiplot")
#### Libraries ------------
library(devtools)
library(ggbiplot)
library(corrplot)
library(devtools)
library(ggbiplot)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(factoextra)
library(psych)
library(car)
library(ggpubr)

#### Calling the other R scripts ---
#Comment this after getting the data

source("R/00_download-from-drive.R")
source("R/01_check-data.R")
rm(i, p, w, ws.legend, ws.observ)

#### Creating duplicate of the original data ------

dt1 <- ws0

#### Adding columns to calculate water content for Aerial material and Root ----

dt1 <- mutate(dt1, 
              Aerial_water_content = 
                (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

dt1 <- mutate(dt1, 
              Root_water_content = 
                (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)

### Neat data to work on -------

num.var <- c("Plant_height", "Leaf_width", 
             "Leaf_area","Aerial_dry_weight")


### ANOVA with p-values to decide significant week ---------- 
num.var.n.aerial <- c("Plant_height", "Leaf_width", 
                        "Leaf_area")

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use',
                'Treatment', num.var.n.aerial)


dt2.w1 <- dt1 %>% filter(Week == "W1") %>% select(one_of(parameters))
dt2.w6 <- dt1 %>% filter(Week == "W6") %>% select(one_of(parameters))
dt2 <- bind_rows(dt2.w1, dt2.w6)
rm(dt2.w1, dt2.w6)

# Creating the table to keep the information 
week <- as.factor(c("W1", "W6"))

col_nam <- c("Species", levels(week))
num_col <- length(col_nam)
num_row <- length(levels(dt2$Species))

#Tables for p values
leaf.area_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
leaf.width_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
plant.height_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))

#Tables for results
leaf.area.res <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
leaf.width.res <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
plant.height.res <- as.data.frame(matrix(nrow = num_row, ncol = num_col))

#Names
names(leaf.area_table) <- col_nam
names(leaf.width_table) <- col_nam
names(plant.height_table) <- col_nam
names(leaf.area.res) <- col_nam
names(leaf.width.res) <- col_nam
names(plant.height.res) <- col_nam

# Variables for the loop
sp <- levels(dt2$Species)[1]
w <- levels(week)[1]
i <- 1


for(sp in levels(dt2$Species)){
  #p-values
  pvls1 <- c()
  pvls2 <- c()
  pvls3 <- c()
  #results
  res1 <- c()
  res2 <- c()
  res3 <- c()
  
  for(w in levels(week)){
    
    # Data filtered by species
    dt4 <- dt2 %>% filter(Week == w) %>% filter(Species == sp)
    
    # Anova test
    model1 <- aov(dt4$Leaf_area ~ dt4$Treatment, data = dt4)
    model2 <- aov(dt4$Leaf_width ~ dt4$Treatment, data = dt4)
    model3 <- aov(dt4$Plant_height ~ dt4$Treatment, data = dt4)
    
    # P values leaf_area
    p1 <- summary(model1)[[1]][["Pr(>F)"]][1]
    p1 <- round(p1, 4)
    pvls1 <- c(pvls1 ,p1)
    
    if(p1 < 0.05){
      r1 <- "significant"
      res1 <- c(res1 ,r1)
    } else {
      r1 <- "not significant"
      res1 <- c(res1 ,r1)
    }
    
    # P values leaf_width
    p2 <- summary(model2)[[1]][["Pr(>F)"]][1]
    p2 <- round(p2, 4)
    pvls2 <- c(pvls2 ,p2)
    
    if(p2 < 0.05){
      r2 <- "significant"
      res2 <- c(res2 ,r2)
    } else {
      r2 <- "not significant"
      res2 <- c(res2 ,r2)
    }
    
    # P values plant_height
    p3 <- summary(model3)[[1]][["Pr(>F)"]][1]
    p3 <- round(p3, 4)
    pvls3 <- c(pvls3 ,p3)
    
    if(p3 < 0.05){
      r3 <- "significant"
      res3 <- c(res3 ,r3)
    } else {
      r3 <- "not significant"
      res3 <- c(res3 ,r3)
    }
    
    # Remove data just in case
    rm(dt4, r1, r2, r3)
  }
  # Allocating the values in the table
  #Leaf_area
  leaf.area_table$Species[i] <- sp
  leaf.area_table[i, levels(week)] <- pvls1
  leaf.area.res$Species[i] <- sp
  leaf.area.res[i, levels(week)] <- res1
  
  #Leaf_width
  leaf.width_table$Species[i] <- sp
  leaf.width_table[i, levels(week)] <- pvls2
  leaf.width.res$Species[i] <- sp
  leaf.width.res[i, levels(week)] <- res2
  
  #Plant heigth
  plant.height_table$Species[i] <- sp
  plant.height_table[i, levels(week)] <- pvls3
  plant.height.res$Species[i] <- sp
  plant.height.res[i, levels(week)] <- res3
  
  #Counter
  i <- i + 1
  rm(pvls1, pvls2, pvls3, res1, res2, res3)
}

# Remove variables that are not gonna be used
rm(sp, i, p1, p2, p3, w, parameters, col_nam, 
   num_col,num_row, model1, model2, model3, num.var.n.aerial, week)

### Joining data

leaf.area.res$Variable <- rep("Leaf area", nrow(leaf.area.res))
leaf.width.res$Variable <- rep("Leaf width", nrow(leaf.width.res))
result.table <- bind_rows(leaf.area.res, leaf.width.res)

plant.height.res$Variable <- rep("Plant height", nrow(plant.height.res))
result.table <- bind_rows(result.table, plant.height.res)

not.correct <- result.table %>% filter(W1 == "significant")
significant <- result.table %>% filter(W6 == "significant")

rm(dt2, num.var.n.aerial, num.var)

final_species <- c("Amaranthus retroflexus","Beta vulgaris",
                        "Hordeum vulgare",
                        "Portulaca oleracea",
                        "Raphanus sativus",
                        "Sonchus oleraceus", 
                        "Spinacia oleracea")





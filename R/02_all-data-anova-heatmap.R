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

#### Adding columns to calculate water content for Aerial material and Root 

dt1 <- mutate(dt1, 
              Aerial_water_content = 
                (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

dt1 <- mutate(dt1, 
              Root_water_content = 
                (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)

### Neat data to work on

num.var <- c("Plant_height", "Leaf_number", "Leaf_length",
             "Leaf_width", "Leaf_area","Chlorophyll_content",
             "Aerial_fresh_weight", "Aerial_dry_weight",     
             "Root_length", "Roots_fresh_weight", 
             "Roots_dry_weight", "Aerial_water_content",
             "Root_water_content")

num.var2 <- c("Species","Treatment","Plant_height", "Leaf_number", 
              "Leaf_length","Leaf_width", "Leaf_area",
              "Chlorophyll_content",
              "Aerial_fresh_weight", "Aerial_dry_weight",     
              "Root_length", "Roots_fresh_weight", 
              "Roots_dry_weight", "Aerial_water_content",
              "Root_water_content")


final_species <- c("Amaranthus retroflexus", "Beta vulgaris",      
                   "Portulaca oleracea", "Raphanus sativus",      
                   "Solanum lycopersicum","Sonchus oleraceus",
                   "Spinacia oleracea")



### Choosing my species and variables
temp <- c(dt1$Species)

dt2 <- dt1 %>% filter(temp %in% final_species, Week == "W6") %>% 
  select(all_of(num.var)) %>% drop_na()

dt3 <- dt1 %>% filter(temp %in% final_species, Week == "W6") %>% 
  select(all_of(num.var2)) %>% drop_na()

rm(temp)

#### Creating the table
species <- as.factor(final_species)
col_nam <- c("Species", num.var)
num_col <- length(col_nam)
num_row <- length(levels(species))

#Tables for p values
table.p.values <- as.data.frame(matrix(nrow = num_row, ncol = num_col))


#Tables for results
TF.p.values <- as.data.frame(matrix(nrow = num_row, ncol = num_col))

#Names
names(table.p.values) <- col_nam
names(TF.p.values) <- col_nam

# Variables for the loop
sp <- levels(species)[1]
nv <- num.var[1]
i <- 1

for(sp in levels(species)){
  #p-values
  pvls <- c()

  #results
  res <- c()
  
  for(nv in num.var){
    
    # Data filtered by species
    dt4 <- dt3 %>% filter(Species == sp) %>% select(one_of(nv,"Treatment"))
    
    # Anova test
    model <- aov(dt4[[1]] ~ dt4$Treatment, data = dt4)
    
    # P values
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    p <- round(p, 4)
    pvls <- c(p, pvls)
    
    if(p < 0.05){
      r <- "T"
      res <- c(res ,r)
    } else {
      r <- "F"
      res <- c(res ,r)
    }
    
    # Remove data just in case
    rm(dt4, r, p)
  }
  # Allocating the values in the table
  #P values
  table.p.values$Species[i] <- sp
  table.p.values[i, num.var] <- pvls
  
  #Results 
  TF.p.values$Species[i] <- sp
  TF.p.values[i, num.var] <- res
  
  #Counter
  i <- i + 1
  rm(pvls, res)
}

# Remove variables that are not gonna be used
rm(sp, i, nv, col_nam, num_col,num_row, model, species)

##### Heatmap 

rownames(table.p.values) <- table.p.values$Species

# The mtcars dataset:
data <- as.matrix(table.p.values[,2:14])

# Default Heatmap
heatmap(data, Colv = NA, Rowv = NA, scale = "none",
        cexRow = 1, cexCol = 0.8, margins = c(6.8, 0), 
        col = heat.colors(200))

legend(x="bottomleft", legend=c("p < 0.05(*)",
      "0.05 < p < 0.5", "p > 0.5"), 
       fill=heat.colors(3))

# Remove variables
rm(final_species, num.var, num.var2)


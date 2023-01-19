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

final_species <- c("Raphanus sativus",
                   "Spinacia oleracea")

### Chosing my species and variables
temp <- c(dt1$Species)

dt2 <- dt1 %>% filter(temp %in% final_species, Week == "W6") %>% 
  select(all_of(num.var)) %>% drop_na()

dt3 <- dt1 %>% filter(temp %in% final_species, Week == "W6") %>% 
  select(all_of(num.var2)) %>% drop_na()

rm(temp)


#### PCA analysis (princomp) -----
pca.dt2 <- princomp(dt2, cor = TRUE)

# summary
summary(pca.dt2)


# correlation between components and data 
pl1 <- fviz_eig(pca.dt2, addlabels = TRUE, ylim = c(0, 50))
round(cor(dt2, pca.dt2$scores), 3)

# COMMENTS
# Correlation between given data and principal component
# Determine: if there is relevancy between component and data
# The closer to 1  the closer related to the data set 

### Scree plot -----------
screeplot(pca.dt2, type = "l", main = "Screenplot for Water stress experiment")
abline(1, 0, col = "red", lty = 3)


### Plots with variables and points ------
### Four Principal components
pn <- principal(dt2, nfactors = 4, rotate = "none")
dt4 <- as.data.frame(round(cor(dt2, pn$scores), 3))

rm(dt1)


#### 

# Extract the results for species and variables, respectively
var <- get_pca_var(pca.dt2)

# Quality of representation of the variables
# Color by cos2 values: quality
# The closer a variable is to the circle of correlations, 
# the better its representation on the factor map 
# (and the more important it is to interpret these components) 
fviz_pca_var(pca.dt2, col.var = "cos2",
             gradient.cols = c("midnightblue", 
                              "mediumseagreen", "red"), 
                                  repel = TRUE) # Avoid text overlapping


# Contributions of variables to PC1
fviz_contrib(pca.dt2, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(pca.dt2, choice = "var", axes = 2, top = 10)

# Total contribution
fviz_contrib(pca.dt2, choice = "var", axes = 1:2, top = 10)

# Interpret: The red dashed line on the graph above indicates 
# the expected average contribution. 

#The most important (or, contributing) variables can be highlighted
#on the correlation plot as follow:

fviz_pca_var(pca.dt2, col.var = "contrib",
             gradient.cols = c("midnightblue", 
                                             "mediumseagreen","red"))
                                             

#### Cluster of Species + Treatments
pl3 <- ggbiplot(pca.dt2, ellipse=TRUE, groups=dt3$Species) +
  geom_point(aes(shape = dt3$Treatment, 
                 colour = dt3$Species), size = 2.5)

pl3


# Color variables by groups according to PC1-2 total contribution
#Group according to the PC1-2, 3 not important
grp <- factor(c("1", "2", "2", "1", 
                "1", "3", "3", "1", 
                "2", "1", "1", "2",
                "3"))


fviz_pca_var(pca.dt2, col.var = grp, 
             palette = "Set2",
             legend.title = "Cluster")


fviz_pca_biplot(pca.dt2, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = dt3$Species,
                col.ind = "black",
                # Color variable by groups
                col.var = grp,
                
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE,
                addEllipses = TRUE,
                
)+
  ggpubr::fill_palette("Pastel1")+      # Indiviual fill color
  ggpubr::color_palette("Dark2")      # Variable colors




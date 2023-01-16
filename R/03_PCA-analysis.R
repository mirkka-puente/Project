remotes::install_github('vqv/ggbiplot')

#### Libraries ------------
library(devtools)
library(ggbiplot)
library("corrplot")

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


dt2 <- select(dt1, all_of(num.var)) %>% drop_na()
dt3 <- select(dt1, all_of(num.var2)) %>% drop_na()

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


################################### NEW STUFF

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
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

# Interpret: The red dashed line on the graph above indicates 
# the expected average contribution. 

#The most important (or, contributing) variables can be highlighted
#on the correlation plot as follow:

fviz_pca_var(pca.dt2, col.var = "contrib",
               gradient.cols = c("midnightblue", 
                                 "mediumseagreen","red"))

####################################################
#library("FactoMineR")
pca2 <- prcomp(dt3, graph = FALSE)
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)

# Color variables by groups
fviz_pca_var(pca2, col.var = grp, 
             palette = c("midnightblue", 
                          "mediumseagreen","red"),
             legend.title = "Cluster")

#####

fviz_pca_biplot(pca.dt2, 
                # Individuals
                geom.ind = "point",
                fill.ind = dt3$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "SEt3",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "npg",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)


fviz_pca_biplot(pca.dt2, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = dt3$Species,
                col.ind = "black",
                # Color variable by groups
                col.var =  factor(c("c", "c", "c",
                                       "c", "c","v",
                                       "v", "v",     
                                       "v", "y", 
                                       "y", "y",
                                       "y")),
                
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE,
                addEllipses = TRUE,
            
)+
  ggpubr::fill_palette("Set3")+      # Indiviual fill color
  ggpubr::color_palette("jco")      # Variable colors

#### Cluster of Species + Treatments
pl3 <- ggbiplot(pca.dt2, ellipse=TRUE, groups=dt3$Species) +
  geom_point(aes(shape = dt3$Treatment, 
                 colour = dt3$Species), size = 2.5)

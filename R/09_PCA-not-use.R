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

num.var <- c("Leaf_number", "Aerial_water_content",
             "Root_water_content")
num.var2 <- c("Species","Treatment","Leaf_number", 
              "Aerial_water_content",
              "Root_water_content")

dt2 <- dt1 %>% filter(Week == "W6") %>% select(all_of(num.var)) %>% 
  drop_na()

dt3 <- dt1 %>% filter(Week == "W6") %>% select(all_of(num.var2)) %>% 
  drop_na()

#### PCA 
pca.dt2 <- prcomp(dt2, center = TRUE, scale. = TRUE)

# correlation between components and data 
pl1 <- fviz_screeplot(pca.dt2)
round(cor(dt2, pca.dt2$scores), 3)

# COMMENTS
# Correlation between given data and principal component
# Determine: if there is relevancy between component and data
# The closer to 1  the closer related to the data set 

### Scree plot -----------
screeplot(pca.dt2, type = "l", main = "Screenplot for experiment")
abline(1, 0, col = "red", lty = 3)


### Plots with variables and points ------

# Correlated variables
pl2 <- fviz_pca_var(pca.dt2,
                    col.var = "contrib",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE)# Avoid text overlapping

### Four Principal components
pn <- principal(dt2, nfactors = 1, rotate = "none")
dt4 <- as.data.frame(round(cor(dt2, pn$scores), 3))

rm(dt1)

### GGBIPLOT############################################
ggbiplot(pca.dt2, ellipse=TRUE, groups=dt3$Species)
########################################################







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

num.var <- c("Chlorophyll_content", "Roots_fresh_weight")

num.var2 <- c("Species","Treatment",
              "Chlorophyll_content", "Roots_fresh_weight")



Species2 <- as.factor(c("Amaranthus retroflexus","Beta vulgaris","Portulaca oleracea",
                        "Raphanus sativus","Solanum lycopersicum",
                        "Sonchus oleraceus","Spinacia oleracea"))



### with Species -----
dt <- dt1 %>% filter(Week == "W6") %>% select(one_of(num.var2))
dt0 <- dt %>% filter(Species == "Amaranthus retroflexus") 

for (sp in Species2[2:7]){
  dt.t <- dt %>% filter(Species == sp) 
  dt0 <- bind_rows(dt0, dt.t)
  rm(dt.t)
}
rm(dt, sp)

dt2 <- dt0 %>% drop_na()
rm(dt0)

#### PCA 
pca.dt2 <- prcomp(dt2[,3:4], center = TRUE, scale. = TRUE)

# correlation between components and data 
pl1 <- fviz_screeplot(pca.dt2)
round(cor(dt2[,3:4], pca.dt2$scores), 3)

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
pn <- principal(dt2, nfactors = 2, rotate = "none")
dt4 <- as.data.frame(round(cor(dt2, pn$scores), 3))

### GGBIPLOT
ggbiplot(pca.dt2, ellipse=TRUE, groups=dt0$Species)
pl2

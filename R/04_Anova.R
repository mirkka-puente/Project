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

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_area","Roots_dry_weight",
                "Root_water_content", "Chlorophyll_content")

num.var <- c("Leaf_area","Roots_dry_weight",
             "Root_water_content", "Chlorophyll_content")

Species2 <- as.factor(c("Amaranthus retroflexus","Beta vulgaris","Portulaca oleracea",
                        "Raphanus sativus","Solanum lycopersicum",
                        "Sonchus oleraceus","Spinacia oleracea"))


w <- "W6"

dt2 <- dt1 %>% filter(Week == w) %>% select(one_of(parameters))


### ANOVA with p-values to decide significant variables ---------- 

# Creating the table to keep the information 
col_nam <- c("Species","Variables", "P_value", "Significant")
num_col <- length(col_nam)
num_row <- length(Species2)  * length(num.var)
anova_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(anova_table) <- col_nam

# Variables for the loop

sp <- levels(Species2)[1]
nv <- num.var[1]
i <- 1


for(sp in levels(Species2)){
  for(nv in num.var){
    # Data filtered by species
    dt3 <- dt2 %>% filter(Species == sp) %>% select(one_of(nv,"Treatment"))
    dt3 <- na.omit(dt3)
    
    # Anova test
        
    model <- aov(dt3[[1]] ~ dt3$Treatment, data = dt3)
        
    # Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
      
      
    # Allocating the values in the table
    
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

# Significant variables
#anova_table <- anova_table %>% filter(Significant == "Yes")

# Remove variables that are not gonna be used
rm(sp, nv, i, p, w, parameters, col_nam, num_col,num_row, 
   num.var, model, dt2)


### ANOVA with p-values to decide significant week for Leaf_area ---------- 

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use',
                'Treatment',"Leaf_area", "Chlorophyll_content")


dt2.w1 <- dt1 %>% filter(Week == "W1") %>% select(one_of(parameters))
dt2.w6 <- dt1 %>% filter(Week == "W6") %>% select(one_of(parameters))
dt2 <- bind_rows(dt2.w1, dt2.w6)
rm(dt2.w1, dt2.w6)

# Creating the table to keep the information 
week <- as.factor(c("W1", "W6"))

col_nam <- c("Species", levels(week))
num_col <- length(col_nam)
num_row <- length(levels(dt2$Species))
leaf_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(leaf_table) <- col_nam

# Variables for the loop
sp <- levels(dt2$Species)[1]
w <- levels(week)[1]
i <- 1


for(sp in levels(dt2$Species)){
  
  pvls <- c()
  
  for(w in levels(week)){
    
    # Data filtered by species
    dt4 <- dt2 %>% filter(Week == w) %>% filter(Species == sp)
    
    # Anova test
    model <- aov(dt4$Leaf_area ~ dt4$Treatment, data = dt4)
    
    # Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    p <- round(p, 4)
    pvls <- c(pvls ,p)
    
    
    # Remove data just in case
    rm(dt4)
  }
  # Allocating the values in the table
  leaf_table$Species[i] <- sp
  leaf_table[i, levels(week)] <- pvls
  i <- i + 1
  rm(pvls)
}

# Remove variables that are not gonna be used
rm(sp, i, p, w, parameters, col_nam, num_col,num_row, model)

### Significant table for p-values ----------------------

# Creating the table to keep the information 
col_nam <- c("Species", levels(week))
num_col <- length(col_nam)
num_row <- length(levels(dt2$Species))
leaf_result <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(leaf_result) <- col_nam

# Variables for the loop
sp <- levels(dt2$Species)[1]
w <- levels(week)[1]
i <- 1


for(sp in levels(dt2$Species)){
  
  pvls <- c()
  
  for(w in levels(week)){
    
    # Data filtered by species
    dt4 <- dt2 %>% filter(Week == w) %>% filter(Species == sp)
    
    # Anova test
    model <- aov(dt4$Leaf_area ~ dt4$Treatment, data = dt4)
    
    # Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    res <- NA
    
    if(p < 0.05){
      res <- "significant"
      pvls <- c(pvls ,res)
    } else {
      res <- "not significant"
      pvls <- c(pvls ,res)
    }
    
    # Remove data just in case
    rm(dt4, res)
  }
  # Allocating the values in the table
  leaf_result$Species[i] <- sp
  leaf_result[i, levels(week)] <- pvls
  i <- i + 1
  rm(pvls)
}

# Remove variables that are not gonna be used
rm(sp, i, p, w, col_nam, num_col,num_row, model, Species2, dt2, week)

#### Tests for the remainder species -------

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_area","Roots_dry_weight",
                "Root_water_content")

num.var <- c("Leaf_area","Roots_dry_weight",
             "Root_water_content")

Species2 <- as.factor(c("Hordeum vulgare",
                        "Lolium perenne"))

w <- "W6"

dt2 <- dt1 %>% filter(Week == w) %>% select(one_of(parameters))


### ANOVA with p-values to decide significant variables ---------- 

# Creating the table to keep the information 
col_nam <- c("Species","Variables", "P_value", "Significant")
num_col <- length(col_nam)
num_row <- length(Species2)  * length(num.var)
anova_table2 <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(anova_table2) <- col_nam

# Variables for the loop

sp <- levels(Species2)[1]
nv <- num.var[1]
i <- 1


for(sp in levels(Species2)){
  for(nv in num.var){
    # Data filtered by species
    dt3 <- dt2 %>% filter(Species == sp) %>% select(one_of(nv,"Treatment"))
    dt3 <- na.omit(dt3)
    
    # Anova test
    
    model <- aov(dt3[[1]] ~ dt3$Treatment, data = dt3)
    
    # Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    
    
    # Allocating the values in the table
    
    if (p < 0.05){
      anova_table2$Species[i] <- sp
      anova_table2$Variables[i] <- nv
      anova_table2$P_value[i] <- p
      anova_table2$Significant[i] <- "Yes"
    } else {
      anova_table2$Species[i] <- sp
      anova_table2$Variables[i] <- nv
      anova_table2$P_value[i] <- p
      anova_table2$Significant[i] <- "No"
    }
    
    i <- i + 1
    rm(dt3)
  }
}

#### Significant variables
#anova_table <- anova_table %>% filter(Significant == "Yes")

# Remove variables that are not gonna be used
rm(sp, nv, i, p, w, parameters, col_nam, num_col,num_row, 
   num.var, model, dt2, Species2)





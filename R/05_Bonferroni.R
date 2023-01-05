#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("car")

#### Libraries ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)

#### Calling the other R scripts -------
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

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_area","Roots_dry_weight",
                "Root_water_content")

num.var <- c("Leaf_area","Roots_dry_weight",
             "Root_water_content")


dt2 <- dt1 %>% filter(Week == "W6") %>% select(one_of(parameters))

### Bonferroni test-----

# Creating the table to keep the information 
groups <- c("Control.Intermediate", "Control.Stress", "Intermediate.Stress")
col_nam <- c("Species","Variables", groups)
num_col <- length(col_nam)
num_row <- length(levels(dt2$Species))  * length(num.var)
b_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(b_table) <- col_nam

# Variables for the loop
sp <- levels(dt2$Species)[1]
nv <- num.var[1]
i <- 1


for(sp in levels(dt2$Species)){
  for(nv in num.var){
    # Data filtered by species
    dt3 <- dt2 %>% filter(Species == sp) %>% select(nv,"Treatment")
    
    # Bonferroni test
    
    bt <- pairwise.t.test(dt3[[1]], dt3$Treatment, p.adjust.method="bonferroni")
    
    # Allocating the values in the table
    b_table$Species[i] <- sp
    b_table$Variables[i] <- nv
    n1 <- round(bt$p.value[1], 3)
    n2 <- round(bt$p.value[2], 3)
    n3 <- round(bt$p.value[4], 3)
    vec <- c(n1, n2, n3)
    b_table[i, groups] <- vec
      
    i <- i + 1
    
    # Remove the data just in case
    rm(dt3, n1, n2, n3, bt, vec)
  }
}

# Remove variables for future
rm(i, nv, sp)

### Summary table Bonferroni --------------------------------------
 
res_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(res_table) <- col_nam

# Variables for the loop
sp <- levels(dt2$Species)[1]
nv <- num.var[1]
i <- 1


for(sp in levels(dt2$Species)){
  for(nv in num.var){
    # Data filtered by species
    dt3 <- dt2 %>% filter(Species == sp) %>% select(nv,"Treatment")
    
    # Bonferroni test
    
    bt <- pairwise.t.test(dt3[[1]], dt3$Treatment, p.adjust.method="bonferroni")
    
    # Allocating the values in the table
    res_table$Species[i] <- sp
    res_table$Variables[i] <- nv
    
    n1 <- round(bt$p.value[1], 3)
    n2 <- round(bt$p.value[2], 3)
    n3 <- round(bt$p.value[4], 3)
    vec <- c(n1, n2, n3)
    sig <- c()
    
    for(n in vec){
      if(n < 0.05){
        sig <- c(sig, "significant")
      }else{
        sig <- c(sig, "not significant")
      }
    }
    
    res_table[i, groups] <- sig
    
    i <- i + 1
    
    # Remove the data just in case
    rm(dt3, n1, n2, n3, bt, vec, sig)
  }
}

# Remove variables for future
rm(i, nv, sp, col_nam, n, num_col, num_row, parameters)

#### Chlorophyll content for some species ------------------------------

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Chlorophyll_content")

Species2 <- c("Beta vulgaris","Portulaca oleracea",
              "Raphanus sativus","Solanum lycopersicum",
              "Sonchus oleraceus","Spinacia oleracea")

dt <- dt1 %>% filter(Week == "W6") %>% select(one_of(parameters))
dt0 <- dt %>% filter(Species == "Beta vulgaris") 

for (sp in Species2[2:6]){
  dt.t <- dt %>% filter(Species == sp) 
  dt0 <- bind_rows(dt0, dt.t)
  rm(dt.t)
}
rm(dt, sp)

#### Bonferroni test para Chlorophyll content ----------

# Creating the table to keep the information 
col_nam <- c("Species","Variables", groups)
num_col <- length(col_nam)

Species2 <- as.factor(Species2)
num_row <- length(levels(Species2)) 
cc_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(cc_table) <- col_nam

# Variables for the loop
sp <- levels(Species2)[1]
i <- 1


for(sp in levels(Species2)){
  # Data filtered by species
  dt3 <- dt0 %>% filter(Species == sp) %>%
    select(one_of("Chlorophyll_content","Treatment"))
    
  # Bonferroni test
    
  bt <- pairwise.t.test(dt3$Chlorophyll_content, dt3$Treatment, p.adjust.method="bonferroni")
    
  # Allocating the values in the table
  cc_table$Species[i] <- sp
  cc_table$Variables[i] <- "Chlorophyll_content"
  n1 <- round(bt$p.value[1], 3)
  n2 <- round(bt$p.value[2], 3)
  n3 <- round(bt$p.value[4], 3)
  vec <- c(n1, n2, n3)
  cc_table[i, groups] <- vec
    
  i <- i + 1
    
    # Remove the data just in case
  rm(dt3, n1, n2, n3, bt, vec)
}


# Remove variables for future
rm(i, sp)

### Summary table Bonferroni for Chlorophyll -------------

chres_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(chres_table) <- col_nam

# Variables for the loop
sp <- levels(Species2)[1]
i <- 1


for(sp in levels(Species2)){
  # Data filtered by species
  dt3 <- dt0 %>% filter(Species == sp) %>%
    select(one_of("Chlorophyll_content","Treatment"))
    
  # Bonferroni test
  
  bt <- pairwise.t.test(dt3$Chlorophyll_content, dt3$Treatment, p.adjust.method="bonferroni")
    
  # Allocating the values in the table
  chres_table$Species[i] <- sp
  chres_table$Variables[i] <- "Chlorophyll_content"
    
  n1 <- round(bt$p.value[1], 3)
  n2 <- round(bt$p.value[2], 3)
  n3 <- round(bt$p.value[4], 3)
  vec <- c(n1, n2, n3)
  sig <- c()
    
  for(n in vec){
    if(n < 0.05){
      sig <- c(sig, "significant")
    }else{
      sig <- c(sig, "not significant")
    }
  }
    
  chres_table[i, groups] <- sig
    
  i <- i + 1
    
    # Remove the data just in case
  rm(dt3, n1, n2, n3, bt, vec, sig)
}


# Remove variables for future
rm(i, sp, col_nam, n, num_col, num_row, parameters, groups, Species2)



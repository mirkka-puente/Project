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
#Comment this after getting the data
source("R/00_download-from-drive.R")
source("R/01_check-data.R")

#### Creating duplicate of the original data ------

data1 <- ws0

#### Adding columns to calculate water content for Aerial material and Root 

data1 <- mutate(data1, 
            Aerial_water_content = 
              (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

data1 <- mutate(data1, 
       Root_water_content = 
         (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)


#### Plant parameters to measure 

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_number", "Chlorophyll_content",
                "Aerial_fresh_weight", "Aerial_dry_weight",     
                "Root_length", "Roots_fresh_weight", 
                "Roots_dry_weight", "Aerial_water_content",
                "Root_water_content")


#### variables to be used
numerical_var <- c("Leaf_number", "Chlorophyll_content",
                   "Aerial_fresh_weight", "Aerial_dry_weight",     
                   "Root_length", "Roots_fresh_weight", 
                   "Roots_dry_weight", "Aerial_water_content",
                   "Root_water_content")

#### GGPLOTS for Raw Data only ----------

#Leaf number
data1 %>% ggplot(aes(x = Week, y = Leaf_number, group = PlantId, 
  col = Treatment)) + geom_line() + geom_point()+
  facet_wrap(~Species) + 
  ggtitle("Raw data: Relationship between Week and Leaf number for each species")

#Leaf number in week 6
data1 %>% filter(Week == "W6") %>%
  ggplot(aes(x = Week, y = Leaf_number, group = PlantId, 
  col = Treatment)) + geom_point()+
  facet_wrap(~Species) + 
  ggtitle("Raw data: Relationship between Week 6 and Leaf number for each species")

#Chlorophyll content
data1 %>% ggplot(aes(x = Week, y = Chlorophyll_content, group = PlantId, 
  col = Treatment)) + geom_line() + geom_point() + 
  facet_wrap(~Species) + 
  ggtitle("Raw data: Relationship between Week and Chlorophyll content for each species")

#Chlorophyll content in week 6
data1 %>% filter(Week == "W6") %>%
  ggplot(aes(x = Week, y = Chlorophyll_content, group = PlantId, 
  col = Treatment)) + geom_point() + 
  facet_wrap(~Species) + 
  ggtitle("Raw data: Relationship between Week 6 and Chlorophyll content for each species")


#### Week we want to focus on

w <- "W6"

data2 <- data1 %>% filter(Week == w) %>% select(all_of(parameters))


## ANOVAs and Tukey tests for the plant parameters

Sqrt_leaf_number <- sqrt(data2$Leaf_number)   # ANOVA of leaf number with square root values
a1_leafnumb1 <- aov(Sqrt_leaf_number ~ Treatment+Date+Species, data = data2)
summary.aov(a1_leafnumb1)

TUKEY_leafnumb1 <- agricolae::HSD.test(a1_leafnumb1, "Treatment", group = TRUE)
plot(TUKEY_leafnumb1, main = "Relationship between water stress treatments and squared values of leaf number", xlab = "Treatement", ylab = "Leaf number")
shap_leafnumb1<- shapiro.test(a1_leafnumb1$residuals)
plot(a1_leafnumb1,2, main = "QQplot residuals")

a1_chlorcont <- aov(Chlorophyll_content ~ Treatment+Date+Species, data = data2)
TUKEY_chlorcont <- agricolae::HSD.test(a1_chlorcont, "Treatment", group = TRUE)
plot(TUKEY_chlorcont, main = "Relationship between water stress treatments and chlorophyll content", xlab = "Treatement", ylab = "Chlorophyll content")
summary.aov(a1_chlorcont)

a1_rlength <- aov(Root_length ~ Treatment+Date+Species, data = data2)
TUKEY_rlength <- agricolae::HSD.test(a1_rlength, "Treatment", group = TRUE)
plot(TUKEY_rlength, main = "Relationship between water stress treatments and root length", xlab = "Treatement", ylab = "Root length") 
summary.aov(a1_rlength)

a1_aerfw <- aov(Aerial_fresh_weight ~ Treatment+Date+Species, data = data2)
TUKEY_aerfw <- agricolae::HSD.test(a1_aerfw, "Treatment", group = TRUE, )
plot(TUKEY_aerfw, main = "Relationship between water stress treatments and aerial fresh weight", xlab = "Treatement", ylab = "Aerial fresh weight") 
summary.aov(a1_aerfw)

a1_aerdw <- aov(Aerial_dry_weight ~ Treatment+Date+Species, data = data2)
TUKEY_aerdw <- agricolae::HSD.test(a1_aerdw, "Treatment", group = TRUE)
plot(TUKEY_aerdw, main = "Relationship between water stress treatments and aerial dry weight", xlab = "Treatement", ylab = "Aerial dry weight")
summary.aov(a1_aerdw)

a1_rootfw <- aov(Roots_fresh_weight ~ Treatment+Date+Species, data = data2)
TUKEY_rootfw <- agricolae::HSD.test(a1_rootfw, "Treatment", group = TRUE)
plot(TUKEY_rootfw, main = "Relationship between water stress treatments and root fresh weight", xlab = "Treatement", ylab = "Root fresh weight") 
summary.aov(a1_rootfw)

a1_rootdw <- aov(Roots_dry_weight ~ Treatment+Date+Species, data = data2)
TUKEY_rootdw <- agricolae::HSD.test(a1_rootdw, "Treatment", group = TRUE)
plot(TUKEY_rootdw, main = "Relationship between water stress treatments and root dry weight", xlab = "Treatement", ylab = "Root dry weight") 
summary.aov(a1_rootdw)

a1_aerwc <- aov(Aerial_water_content ~ Treatment+Date+Species, data = data2)
TUKEY_aerwc <- agricolae::HSD.test(a1_aerwc, "Treatment", group = TRUE)
plot(TUKEY_aerwc, main = "Relationship between water stress treatments and aerial water content", xlab = "Treatement", ylab = "Aerial water content", ylim = c(0,1))
shap_aerwc <-shapiro.test(a1_aerwc$residuals) 
summary.aov(a1_aerwc)

# Kruskal Wallis test for water content 
library(agricolae)
KWtest_aerwc <- with(data2, kruskal(Aerial_water_content, Treatment, group=TRUE))       # Kruskal-Wallis rank sum test
plot(KWtest_aerwc, main = "Relationship between water stress treatments and aerial water content", ylim = c(0,1),xlab = "Treatement", ylab = "Aerial water content")

KWtest_rootwc <- with(data2, kruskal(Root_water_content, Treatment, group=TRUE))       # Kruskal-Wallis rank sum test
plot(KWtest_rootwc, main = "Relationship between water stress treatments and root water content", ylim = c(0,1),xlab = "Treatement", ylab = "Aerial root content")


### Creating a table by "hand" of p_values

v_names <- c("Leaf_number_sqrt","Chlorophyll_content", "Root_length",
             "Aerial_fresh_weight","Aerial_dry_weight",
             "Roots_fresh_weight", "Roots_dry_weight", "Aerial_water_content")

p_value <- c(4.18e-09, 0.555, 0.24805, 2e-16, 2.52e-07, 4.79e-05, 0.504, 0.434)
sd_nsd <- c("Significantly different", "Not significantly different",
            "Not significantly different","Significantly different", 
            "Significantly different", "Significantly different", 
            "Not significantly different", "Not significantly different")
cn <- c("Variables", "P_values", "Significance")

p_table <- as.data.frame(matrix(nrow = length(v_names), ncol = length(cn)))

names(p_table) <- cn
p_table$Variables <- v_names
p_table$P_values <- p_value
p_table$Significance <- sd_nsd


#### Creating a data frame for  final results from linear model

# Making sure the treatments and species are factors
treatments <- levels(as.factor(data2$Treatment))
species <- levels(as.factor(data2$Species))

#Creating the table to keep the information 
col_nam <- c("Species", "Variables", treatments)
num_col <- length(col_nam)
num_row <- length(species) * length(numerical_var)
final_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
names(final_table) <- col_nam

#Variables for the loop
sp <- species[1]
nv <- numerical_var[1]
i <- 1

################# TABLE 1 WITH ERROR ########################
#Loop
for(sp in species){
  for(nv in numerical_var){
    
    #Data filtered by species
    data3 <- filter(data2, Species == sp)
    
    #Linear model on variables
    model <- lm(data3[[nv]] ~ data3$Treatment, data = data3)
    
    #Tukey Test
    out <- agricolae::HSD.test(model, 'Treatment', group = TRUE)
    
    #Retrieving the group names from the test
    letters <- out$groups$groups[order(rownames(out$groups))]
    
    #Allocating the values in the table
    final_table$Species[i] <- sp
    final_table$Variables[i] <- nv
    final_table[i, treatments] <- letters
  
    i <- i + 1
  }
}

#Remove variables that are not gonna be used
rm(sp, nv, i, data3, model, out)

#### Creating a data frame to show the p-values from ANOVA test
col_nam2 <- c("Species", "Variables", "p_value")
num_col2 <- length(col_nam2)
result_table <- as.data.frame(matrix(nrow = num_row, ncol = num_col2))
names(result_table) <- col_nam2

# Variables for loop
s <- species[1]
n <- numerical_var[1]
i <- 1

################# TABLE 2 WITH ERROR ########################
#Loop
for(s in levels(data2$Species)){
  for(n in numerical_var){
    
    #Data filtered by species
    data3 <- filter(data2, Species == s)
    
    #ANOVA
    model <- aov(data3[[n]] ~ data3$Treatment+Week, data = data3)
    
    #Summary
    p <- summary(model)[[1]][["Pr(>F)"]][1]
    
    #Allocating values in the data frame
    result_table$Species[i] <- s
    result_table$Variables[i] <- n
    result_table$p_value[i] <- p
    i <- i + 1
  }
}

#Remove unused variables
rm(s, n, i, model)



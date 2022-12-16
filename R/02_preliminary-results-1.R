#### Install packages --------------
#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)

#### Packages ------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

#### Calling the other scripts to call data ---
#source("R/00_download-from-drive.R")
#source("R/01_check-data.R")

#### Creating Data Frame with required variables----
data0 <- select(filter(ws0, Week == "W6"), 
                -Too_dry, -Soil_humidity, -Electrical_conductivity)

#after the coma comes the columns
data1 <- ws0[ws0$Week == "W6", ]


ggplot(ws0, aes(x = Week, y = Chlorophyll_content, group = PlantId, 
                 col = Treatment)) +
         geom_line()+
        geom_point()+
        facet_grid(~Species)
 
ggplot(ws0, aes(x = Treatment, y = Leaf_number, col = Treatment))+
  geom_boxplot()+
  facet_grid()

ggplot(data1, aes(x=Species, y=Chlorophyll_content, fill=Treatment)) + 
  geom_boxplot()


#### Displaying data ----

boxplot(data0$Plant_height ~ data0$Treatment,
        main="Box plots for plant height",
        xlab="-----",
        ylab="Plant height",
        col= "white",
        border="black")

#### ANOVA -------
a1_pheight <- aov(Plant_height ~ Treatment+Date, data=data0)
summary(a1_pheight)
plot(a1_pheight, 2)
#### Shaphiro test for normality -------
s_pheight <- shapiro.test(a1_pheight$residuals)
s_pheight

#### Log Data 
data0 <- data0 %>% mutate(Log_pheight = log(Plant_height))

#### ANOVA -------
a2_pheight <- aov(Log_pheight ~ Treatment, data=data0)
summary(a2_pheight)
plot(a2_pheight, 2)
#### Shaphiro test for normality -------
s2_pheight <- shapiro.test(a2_pheight$residuals)
s2_pheight

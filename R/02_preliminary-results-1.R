#### Install packages --------------
#install.packages(c("dplyr", "tidyr"), dependencies = TRUE)
#install.packages(c("tidyverse"), dependencies = TRUE)

#### Packages ------------
library(dplyr)
library(tidyr)
library(tidyverse)

#### Calling the other scripts to call data ---
source("R/00_download-from-drive.R")
source("R/01_check-data.R")

#### Creating Data Frame with required variables----
data0 <- select(filter(ws0, Group = 'G4'), 
                Species:Chlorophyll_content,
                        -Too_dry, -Soil_humidity, -Electrical_conductivity)
  

#### Displaying data ----
boxplot(data0$Plant_height ~ data0$Treatment,
     main="Box plots for plant height",
     xlab="Species name",
     ylab="Plant height",
     col= "white",
     border="black")

#### ANOVA -------
a1_pheight <- aov(Plant_height ~ Treatment+Species+Date, data=ws0)
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

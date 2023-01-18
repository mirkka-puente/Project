#### Creating duplicate of the original data ------

dt1 <- ws0

#### Adding columns to calculate water content for Aerial material and Root 

dt1 <- mutate(dt1, 
              Aerial_water_content = 
                (Aerial_fresh_weight - Aerial_dry_weight)/Aerial_fresh_weight)

dt1 <- mutate(dt1, 
              Root_water_content = 
                (Roots_fresh_weight - Roots_dry_weight)/Roots_fresh_weight)

### Neat data to work on ---------

parameters <- c("Week", 'Date','Species', 'PlantId', 'Use', 'Treatment',
                "Leaf_area","Roots_dry_weight",
                "Root_water_content", "Chlorophyll_content")

num.var <- c("Leaf_area","Roots_dry_weight",
             "Root_water_content", "Chlorophyll_content")



w <- "W6"

dt3 <- dt1 %>% filter(Week == w) %>% select(one_of(parameters))

#### Function to calculate the mean and the standard deviation 
#   to calculate error bars

# varname : the name of a column containing the variable to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#### Summary of data for plot 1: Leaf area --------------
dt2 <- data_summary(dt3, varname="Leaf_area", 
                    groupnames=c("Species", "Treatment"))

# Significant groups
p.la <- c(" ", " ", " ",
          " ", "*", " ", 
          " "," ", " ", 
          " "," ", " ",
          " ", " "," ", 
          " ", "*","*", 
          " ","*","*",
          " ","*"," ",
          " ","*","*")


# Standard deviation of the mean as error bar
plot1 <- ggplot(dt2, aes(x=Treatment, y=Leaf_area, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Leaf_area-sd, ymax=Leaf_area+sd), width=0.3,
                position=position_dodge(0.9)) +
  geom_text(aes(y = Leaf_area/2 + 1,
                label = c(p.la)), vjust = 0.5, 
            colour = "black", size = 5,
            position = position_dodge(1))

# Finished bar plot
plot1+labs(title="Leaf area per species in week 6", 
           x="Treatments", y = "Leaf Area (cm^3)")

rm(dt2, w)

#### Summary of data for plot 2: Roots dry weight --------------
dt2 <- data_summary(dt3, varname="Roots_dry_weight", 
                    groupnames=c("Species", "Treatment"))

# Significant groups
p.rdw <- c(" ", " ", " ",
           " ", " ", " ", 
           " ","*", " ", 
           " "," ", " ",
           " ", " "," ", 
           " ", "*"," ", 
           " "," "," ",
           " "," "," ",
           " ","*","*")


# Standard deviation of the mean as error bar
plot2 <- ggplot(dt2, aes(x=Treatment, y=Roots_dry_weight, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Roots_dry_weight-sd, 
                    ymax=Roots_dry_weight+sd), width=0.3,
                position=position_dodge(0.9)) +
  geom_text(aes(y = Roots_dry_weight/2,
                label = c(p.rdw)), vjust = 0.8, 
            colour = "black", size = 5,
            position = position_dodge(1))
# Finished bar plot
plot2+labs(title="Roots dry weight in grams per species in week 6", 
           x="Treatments", y = "Roots dry weight (g)")

rm(dt2)
#### Summary of data for plot 3: Root water content --------------
dt2 <- data_summary(dt3, varname="Root_water_content", 
                    groupnames=c("Species", "Treatment"))

# Significant groups
p.rwc <- c(" ", " ", " ",
           " ", " ", " ", 
           " ","*", "*", 
           " ","*", "*",
           " ", " "," ", 
           " ", "*","*", 
           " "," "," ",
           " ","*  "," ",
           " "," "," ")

# Standard deviation of the mean as error bar
plot3 <- ggplot(dt2, aes(x=Treatment, y=Root_water_content, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Root_water_content-sd, 
                    ymax=Root_water_content+sd), width=0.3,
                position=position_dodge(0.9))+
  geom_text(aes(y = Root_water_content/2,
                label = c(p.rwc)), vjust = 0.7, 
            colour = "black", size = 5,
            position = position_dodge(1))

# Finished bar plot
plot3+labs(title="Average Root water content per species in week 6", 
           x="Treatments", y = "Root water content")

rm(dt2)

#### Summary of data for plot 4: Chlorophyll content --------------

dt3 <- na.omit(dt3)

dt2 <- data_summary(dt3, varname="Chlorophyll_content", 
                    groupnames=c("Species", "Treatment"))

# Significant groups
p.cc <- c(" ", " ", " ",
          " ", " ", " ", 
          " "," ", " ", 
          " ","*", "*",
          " ", "*","*", 
          " ", " "," ", 
          " "," "," ")


# Standard deviation of the mean as error bar
plot4 <- ggplot(dt2, aes(x=Treatment, y=Chlorophyll_content, fill=Species)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=Chlorophyll_content-sd, 
                    ymax=Chlorophyll_content+sd), width=0.3,
                position=position_dodge(0.9)) +
  geom_text(aes(y = Chlorophyll_content/2 + 1,
                label = c(p.cc)), vjust = 0.5, 
            colour = "black", size = 5,
            position = position_dodge(1))

# Finished bar plot
plot4+labs(title="Chlorophyll content per species in week 6", 
           x="Treatments", y = "Chlorophyll content (SPAD)")


rm(dt2)



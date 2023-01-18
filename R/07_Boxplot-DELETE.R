#### Boxplots for Chlorophyll 

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
                "Leaf_area","Roots_dry_weight","Chlorophyll_content",
                "Root_water_content")


w <- "W6"

dt2 <- dt1 %>% filter(Week == w) %>% select(one_of(parameters))

#### Boxplots for Leaf area
# One box per Species
p1 <- ggplot(dt2, aes(x=Treatment, y=Leaf_area, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Species)
# one box per Treatment
p2 <- ggplot(dt2, aes(x=Treatment, y=Leaf_area, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Treatment, scale="free")

#### Boxplots for Roots_dry_weight
# One box per Species
p3 <- ggplot(dt2, aes(x=Treatment, y=Roots_dry_weight, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Species)
# one box per Treatment
p4 <- ggplot(dt2, aes(x=Treatment, y=Roots_dry_weight, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Treatment, scale="free")

#### Boxplots for Root_water_content
# One box per Species
p5 <- ggplot(dt2, aes(x=Treatment, y=Root_water_content, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Species)
# one box per Treatment
p6 <- ggplot(dt2, aes(x=Treatment, y=Root_water_content, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Treatment, scale="free")

#### Boxplots for Chlorophyll_content
# One box per Species
na.omit(dt2)
p7 <- ggplot(dt2, aes(x=Treatment, y=Chlorophyll_content, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Species)
# one box per Treatment
p8 <- ggplot(dt2, aes(x=Treatment, y=Chlorophyll_content, fill=Species)) + 
  geom_boxplot() +
  facet_wrap(~Treatment, scale="free")

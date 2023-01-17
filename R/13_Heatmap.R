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

num.var <- c()

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

final_species <- c("Amaranthus retroflexus","Beta vulgaris",
                   "Hordeum vulgare",
                   "Portulaca oleracea",
                   "Raphanus sativus",
                   "Sonchus oleraceus", 
                   "Spinacia oleracea")

### Chosing my species and variables
temp <- c(dt1$Species)

dt2 <- dt1 %>% filter(temp %in% final_species, Week == "W6") %>% 
  select(all_of(num.var)) %>% drop_na()

dt3 <- dt1 %>% filter(temp %in% final_species, Week == "W6") %>% 
  select(all_of(num.var2)) %>% drop_na()

rm(temp)
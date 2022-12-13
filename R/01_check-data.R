############ Download and check water stress data ##########
### 2022-11-21 algarsal
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(readxl)

p <- # path to the data
  'data/wstress.xlsx' 

### Import data -----------------------------------------
# readxl::excel_sheets(p)
ws0 <- read_xlsx(path = p, sheet = "Data")
ws.legend <- read_xlsx(path = p, sheet = "Legend")
ws.observ <- read_xlsx(path = p, sheet = "Observations")

### Fix data -------------------------------------------
### Factors
# names(ws0)
var.factors <- c("Group", "Week", "Date", "Species", 
                 "Treatment", "Too_dry") 
for (i in var.factors) ws0[[i]] <- as.factor(ws0[[i]])

### Numeric
var.numeric <- c(
  "Soil_humidity", "Electrical_conductivity", "Plant_height", 
  "Leaf_number", "Leaf_length", "Leaf_width", "Leaf_area",
  "Chlorophyll_content","Aerial_fresh_weight", "Aerial_dry_weight",
  "Root_length", "Roots_fresh_weight", "Roots_dry_weight")
for (i in var.numeric) ws0[[i]] <- as.numeric(ws0[[i]])

### Check data -----------------------------------------

### Levels of factors
levels(ws0$Group)
levels(as.factor(ws0$Week))
levels(as.factor(ws0$Date))
levels(as.factor(ws0$Species))
levels(as.factor(ws0$Use))
levels(as.factor(ws0$Treatment))
levels(as.factor(ws0$Too_dry))

### Repeated plant id codes
ws0$PlantId[which(duplicated(ws0$PlantId))]

### Numeric variables
max(as.numeric(ws0$Soil_humidity), na.rm = TRUE)
min(as.numeric(ws0$Soil_humidity), na.rm = TRUE)

max(as.numeric(ws0$Soil_humidity), na.rm = TRUE)
min(as.numeric(ws0$Soil_humidity), na.rm = TRUE)

### The same check for the rest of numeric data


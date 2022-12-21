ws0 <- readxl::read_xlsx(path="water_strees.xlsx")
library(tidyverse)
library(agricolae)
# Loop
variables <- c("Plant_height" ,  "Leaf_number")
week <- "W6"
Tr <- levels(as.factor(ws0$Treatment))
column.names<-c("Species", "variables", Tr)
sp <- levels(as.factor(ws0$Species))

rows <- length(sp) * length(variables) 

results <- as.data.frame(matrix(nrow=rows,  ncol=length(column.names)) )

names(results) <- column.names
ss0 <- ws0[ws0$Week==week, ]




i <- sp[1]
j <- variables[1]
k <- 1

ws0 <- 
  
  for(i in sp ){
    for(j in variables){
      
      
      ss1 <- ss0[ss0$Species==i, ]
      lm.1 <- lm(ss1[[j]]~Treatment, data=ss1 )
      # summary(lm.1)
      hsd <- HSD.test(lm.1, "Treatment")
      letter <- hsd$groups$groups[order(rownames(hsd$groups))]
      results$Species[k] <- i
      results$variables[k] <- j
      results[k, Tr] <-  letter
      k <- k+1
    }
    
  }

ws1 <- ws0%>%filter(Week=="W6")%>%
  group_by(Species)%>%
  mutate(meanh=mean(Plant_height))

ggplot(ws1, aes(x=Treatment, y=meanh))+geom_bar()
library(tidyverse)
library(rFIA)
library(vegan)
library(ggrepel)

# 1. add species common and scientific names to spcd 

tree <- read.csv("data/raw/FIA_TreeSummary.csv")
sp_lookup <- read.csv("data/raw/SPCD_lookup_table.csv") %>% select(SPCD, COMMON_NAME, GENUS, SPECIES)

tree <- left_join(tree, sp_lookup, by="SPCD") %>% 
  select(Treatment, PLOT.NUMBER, SPCD, STATUSCD, "DBH_in" = `DIAMETER..inches.`, everything())
tree$Basal_area <- pi*(tree$DBH_in/2)^2
head(tree)
# write.csv(tree, "data/clean/FIA_tree_data.csv", row.names=F)

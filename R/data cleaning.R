library(tidyverse)
library(rFIA)
library(vegan)
library(ggrepel)

# 1. add species common and scientific names to spcd ####

tree <- read.csv("data/raw/FIA_TreeSummary.csv")
sp_lookup <- read.csv("data/raw/SPCD_lookup_table.csv") %>% select(SPCD, COMMON_NAME, GENUS, SPECIES)

tree <- left_join(tree, sp_lookup, by="SPCD") %>% 
  select(Treatment, PLOT.NUMBER, SPCD, STATUSCD, "DBH_in" = `DIAMETER..inches.`, everything())
tree$Basal_area <- pi*(tree$DBH_in/2)^2
head(tree)

#QA/QC
# any sp missing names
tree %>% filter(is.na(GENUS))

# export
# write.csv(tree, "data/clean/FIA_tree_data.csv", row.names=F)


# 2. add Rebecca's data ####
reb <- read.csv("data/raw/Rebeccas_tree_data.csv", fileEncoding="UTF-8-BOM") %>%
  filter(Year!=2019) %>% 
  select("PLOT.NUMBER" = Site.name,
         "GENUS" = Genus,
         "SPECIES" = Specific.Epithet,
         DBH..cm.) %>% 
  mutate(DBH_in =  DBH..cm./2.5)
reb$Basal_area <- pi*(reb$DBH_in/2)^2
reb$Treatment = "Mined"

# fix some type-os in rebs tree sp
reb <- reb %>% 
  mutate(SPECIES = str_trim(SPECIES, side = c("both")), # remove extra whitespace
         GENUS = str_trim(GENUS, side=c("both"))) %>% 
  mutate(SPECIES = case_when(SPECIES == "syraciflua" ~ "styraciflua",
                             SPECIES == "albidium" ~ "albidum",
                             SPECIES == "sp"  ~ "spp.",
                             SPECIES == "montana" ~ "prinus", # this is just to match with FIA
                             SPECIES == ""  ~ "spp.",
                             TRUE ~ as.character(SPECIES)),
         GENUS = case_when(GENUS == "Faxinus" ~ "Fraxinus",
                           TRUE ~ as.character(GENUS)))

# add in SPCD and common name from sp_lookup
reb <- left_join(reb, sp_lookup, 
          by=c("GENUS", "SPECIES")) 
reb <- select(reb, Treatment, PLOT.NUMBER, SPCD, DBH_in, COMMON_NAME, GENUS, SPECIES, Basal_area)

# QA/QC
# any trees missing names?
reb %>% 
  filter(is.na(SPECIES))

# any trees in rebs data that aren't in FIA data?
anti_join(distinct(reb, COMMON_NAME, GENUS, SPECIES),
          distinct(tree, COMMON_NAME, GENUS, SPECIES))

# combine reb's and FIA data
str(tree)
str(reb)
tree$PLOT.NUMBER <- as.factor(tree$PLOT.NUMBER) # convert to factor to match rebecca's naming system

# add variable noting which data were rebecca's vs FIA
tree$Data_Source <- "FIA"
reb$Data_Source <- "Rebecca"

# combine into one data frame
df <- bind_rows(reb, tree) %>% 
  select(Data_Source, Treatment, everything()) # re-order columns for clarity

# export full data set
# write.csv(df, "data/clean/full_tree_data.csv", row.names=F)

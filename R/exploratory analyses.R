# this file performs exploratory data analysis for FIA project

library(tidyverse)
library(rFIA)

# mature trees ####
{
  df <- read.csv("data/clean/Tree.csv")
  
  df <- df %>% 
    mutate(Species=paste(Genus, species, sep="_")) %>% 
    select(-Genus, -species)
  
  
  df <- df %>% 
    group_by(Year, Habitat, Site, SubplotLoc, Species) %>%
    summarize(TotalBiomass = sum(Biomass)) %>% 
    group_by(Year, Habitat, Site, SubplotLoc) %>%
    mutate(PlotBiomass = sum(TotalBiomass),
           RelAbund = TotalBiomass/PlotBiomass) 
  
  # richness
  p1 <- df %>% 
    group_by(Year, Site, Habitat, SubplotLoc) %>% 
    distinct(Species) %>% 
    tally() %>% # num of species per subplot
    ggplot(., aes(x=fct_reorder(Site, n, mean), y=n, fill=Habitat)) +
    stat_summary(fun.data="mean_se", geom="bar") +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0.5) +
    labs(x="Site ID", y="Tree Sp Richness (mean +- se)") +
    theme_classic() +
    theme(legend.position = "top")
  
  # average biomass
  p2 <- df %>% 
    distinct(Year, Site, Habitat, SubplotLoc, PlotBiomass) %>% 
    ggplot(., aes(x=fct_reorder(Site, PlotBiomass , mean), y=PlotBiomass , fill=Habitat)) +
    stat_summary(fun.data="mean_se", geom="bar") +
    stat_summary(fun.data="mean_se", geom="errorbar", width=0.5) +
    labs(x="Site ID", y="Subplot-level biomass (mean +- se)") +
    theme_classic() +
    theme(legend.position = "top")
  
  
  library(cowplot)
  myleg <- get_legend(p1)
  
  tmpp <- plot_grid(p1 + theme(legend.position = "none"),
            p2 + theme(legend.position = "none"), 
            ncol=2, nrow=1)
  
  plot_grid(myleg, tmpp,ncol=1, nrow=2,
            rel_heights = c(0.1,1))
  
  # stats
  library(nlme)
  # richness
  rich <- df %>% 
    group_by(Year, Site, Habitat, SubplotLoc) %>% 
    distinct(Species) %>% 
    tally()
  summary(lm(n ~ Site, data=rich))
  anova(lme(n ~ Habitat, random = ~1|Site, data=rich))
  
  # biomass
  bio <-  df %>% 
    distinct(Year, Site, Habitat, SubplotLoc, PlotBiomass)
  summary(lm(PlotBiomass ~ Site, data=bio))
  anova(lme(PlotBiomass ~ Habitat, random = ~1|Site, data=bio))
  
  
  
  library(vegan)
  # ords
  head(df)
  
  spmat <- df %>% 
    select(Year, Site, Habitat, SubplotLoc, Species, RelAbund) %>% 
    spread(., key=Species, value = RelAbund) %>% 
    gather(., key=Sp, value=relabund, 5:ncol(.)) 
  spmat$relabund <- replace_na(spmat$relabund, 0)
  spmat <- spmat %>% spread(., Sp, relabund)
  
  
  ord1 <- metaMDS(comm = spmat[, 5:ncol(spmat)],
                  distance="bray",
                  sratmax=0.9999999,
                  maxit=800,
                  try=100,
                  k=3)
  
  # extract sites scores and convert to df
  # species (rows) are "sites" and traits (columns) are "species"
  ord.scores.sp <- data.frame(scores(ord1, "sites"))
  
  # add in id info
  ord.scores.sp$Year <- spmat$Year
  ord.scores.sp$Site <- spmat$Site
  ord.scores.sp$Habitat <- spmat$Habitat
  ord.scores.sp$SubplotLoc <- spmat$SubplotLoc
  
  
  head(ord.scores.sp)
  
  library(ggrepel)
  # colored by habitat
  ggplot(filter(ord.scores.sp, Habitat!="PPF"), aes(x=NMDS1, y=NMDS2, color=Habitat)) +
    geom_point(size=5) + 
    stat_ellipse(aes(group=Habitat, fill=Habitat), geom="polygon", alpha=0.3, level=0.9 ) +
    theme_classic() +
    theme(legend.position = "top")
  
  # colored by site
  ggplot(filter(ord.scores.sp, Habitat!="PPF"), aes(x=NMDS1, y=NMDS2, color=Site)) +
    geom_point(size=5) + 
    stat_ellipse(aes(group=Site, fill=Site), geom="polygon", alpha=0.3, level=0.9 ) +
    theme_classic() +
    theme(legend.position = "top")
  
  adonis2(spmat[, 5:ncol(spmat)] ~ spmat$Habitat, strata=spmat$Site, by="margin") # blocking factor
  adonis2(spmat[, 5:ncol(spmat)] ~ spmat$Site,  by="margin") 
  adonis2(spmat[, 5:ncol(spmat)] ~ spmat$Site + spmat$Habitat, by="marginal") # both
}

# check out fia data ####
{
  oh <- getFIA(states="OH")
  
  oh$TREE %>% select(SPCD, STATUSCD, DIA, HT, ACTUALHT, TOTAGE) %>% head()
  
  oh$TREE %>% names()
  oh$TREE %>% 
    distinct(STATECD) %>% 
    head() # all in OH
  
  oh$TREE %>% 
    distinct(UNITCD) %>% 
    head() # survey units; usually groups of counties
  
  oh$TREE %>% 
    distinct(COUNTYCD) %>% 
    head() # county codes
  
  oh$TREE %>% 
    distinct(CN) %>% 
    head() # unique sequence num to id plot record; almost 200k records

  oh$TREE %>% 
    distinct(PLOT) %>% 
    head() # plot nums; 2040 plots
  
  oh$TREE %>% 
    distinct(INVYR) %>% 
    head() # inventory yr
  
  oh$TREE %>% 
    distinct(CN) %>% 
    nrow() # 199731
}
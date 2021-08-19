library(tidyverse)
library(rFIA)
library(vegan)
library(ggrepel)

tree <- read.csv("data/clean/full_tree_data.csv")

head(tree)

# calculate basal area for each sp in each plot
tree_summary <-  tree %>% 
  filter(STATUSCD==1) %>% 
  group_by(Treatment, PLOT.NUMBER, COMMON_NAME) %>% 
  summarize(Total_BA = sum(Basal_area)) %>% 
  select(Treatment, Plot=PLOT.NUMBER, Species = COMMON_NAME, BA = Total_BA)

head(tree_summary)

# plot richness 
tree_summary %>% 
  group_by(Treatment, Plot) %>% 
  summarize(tree_richness = length(BA)) %>% 
  ggplot(., aes(x=fct_reorder(.f = as.factor(Plot), tree_richness, max), y=tree_richness, fill=Treatment)) +
  geom_bar(stat="identity", position=position_dodge(), color=1) +
  labs(x="Plot_ID", y="Tree Species Richness (num.)") +
  theme(legend.position = "top") # no obvious difference in tree richness

tree_summary %>% 
  group_by(Treatment, Plot) %>% 
  summarize(tree_richness = length(BA)) -> tmp
summary(lm(tree_richness ~ Treatment, data=tmp))


# calculate diversity (based on basal area; not relative basal area)
# first convert to wide format
tree_summary_wide <- tree_summary %>%
  spread(., key=Species, value=BA) %>% 
  gather(., key=Species, value=BA, 3:ncol(.)) %>% 
  replace_na(list(BA=0)) %>% 
  mutate(., Species=str_replace_all(Species, " ", replacement="_")) %>% 
  spread(., key=Species, value=BA)

# caclualte shannon div
shannon_div <- data.frame(shannon_div = diversity(tree_summary_wide[,3:ncol(tree_summary_wide)]))
# add Treatment and Plot ID to shannon diversity indices
shannon_div <- bind_cols(select(tree_summary_wide, Treatment, Plot),
          shannon_div)  
# does diversity differ between treatments?
summary(lm(shannon_div ~ Treatment, data=shannon_div)) # no

# plot shannon diversity
ggplot(shannon_div, aes(x=Treatment, y=shannon_div, fill=Treatment)) +
  geom_boxplot(aes(fill=Treatment), alpha=0.5) +
  geom_jitter(aes(color=Treatment)) +
  theme(legend.position = "top") # no obvious differnce in diversity


# compare composition
{
  tree_nmds <- metaMDS(tree_summary_wide[,3:ncol(tree_summary_wide)],
                       distance="bray",
                       k=2)
  
  # extract site scores and convert to df
  tree_site_scores <- data.frame(scores(tree_nmds, "sites"))
  # add in treatment and plot 
  tree_site_scores$Treatment <- tree_summary_wide$Treatment
  tree_site_scores$Plot <- as.factor(tree_summary_wide$Plot)
  
  tree_sp_scores <- data.frame(scores(tree_nmds, display="species"))
  tree_sp_scores$Species <- rownames(tree_sp_scores)
  
  # add 5 sp most strongly post & neg cor with axis 1
  impsp_5 <- bind_rows(top_n(tree_sp_scores, 5, wt=NMDS1),
                       top_n(tree_sp_scores, -5, wt=NMDS1))
  impsp_5 %>% arrange(desc(NMDS1)) # high NMDS1 values = high HD; low NMDS1 vals = low HD 
  
  # plot
  ggplot(data=impsp_5) +
    geom_point(data=tree_site_scores, 
               aes(x=NMDS1, y=NMDS2, 
                   color=Treatment,
                   fill=Treatment),
               size=2) +
    stat_ellipse(data=tree_site_scores, 
                 aes(x=NMDS1, y=NMDS2, 
                     color=Treatment)) +
    geom_segment(aes(x=0,y=0,xend=NMDS1,yend=NMDS2),
                 color=1,
                 arrow=arrow(length=unit(0.3, "cm"))) +
    geom_label_repel(aes(x=NMDS1,y=NMDS2,label=Species),
                     box.padding=.5, size=3) +
    theme_classic() + 
    theme(legend.position="top") 
  
  adonis2(tree_summary_wide[,3:ncol(tree_summary_wide)] ~ Treatment, 
          data=tree_summary_wide)
  mrpp(dat=tree_summary_wide[,3:ncol(tree_summary_wide)], grouping=tree_summary_wide$Treatment)
  
  }

# compare size class distributions between mined and unmined
tree %>% 
  filter(STATUSCD!=2) %>% # live trees only (Reb's have NAs)
  ggplot(., aes(x=DBH_in, fill=Treatment, color=Treatment)) +
  geom_histogram(stat="count", alpha=0.5, position = position_dodge())

# look at size class distributions at each plot
tree %>% 
  filter(STATUSCD!=2) %>% # live trees only
  ggplot(., aes(x=DBH_in, fill=Treatment, color=Treatment)) +
  geom_histogram(stat="count", alpha=0.5) +
  facet_wrap(~as.factor(PLOT.NUMBER))

# avg number of trees in mined and unmined plots
tree %>% 
  filter(STATUSCD==1) %>% # live trees only
  group_by(Treatment, PLOT.NUMBER) %>% 
  summarize(Num_trees = length(STATUSCD),
            Tot_BA = sum(Basal_area)) %>% 
  group_by(Treatment) %>% 
  summarize(mean_num_trees = mean(Num_trees),
            se_num_trees = sd(Num_trees)/sqrt(length(Num_trees)),
            mean_tot_BA = mean(Tot_BA),
            se_tot_BA = sd(Tot_BA)/sqrt(length(Tot_BA)),
  )

# plot avg num of trees
tree %>% 
  filter(STATUSCD!=2) %>% # live trees only
  group_by(Treatment, PLOT.NUMBER) %>% 
  summarize(Num_trees = length(STATUSCD),
            Tot_BA = sum(Basal_area)) %>% 
  ggplot(., aes(x=Treatment, y=Num_trees, fill=Treatment)) +
  geom_boxplot()

tmp <- tree %>% 
  filter(STATUSCD!=2) %>% # live trees only
  group_by(Treatment, PLOT.NUMBER) %>% 
  summarize(Num_trees = length(STATUSCD),
            Tot_BA = sum(Basal_area))
summary(lm(Num_trees ~ Treatment, data=tmp))

# plot avg total basal area
tree %>% 
  filter(STATUSCD!=2) %>% # live trees only
  # filter(DBH_in < 25) %>% # remove outlier? doesn't change things much
  group_by(Treatment, PLOT.NUMBER) %>% 
  summarize(Num_trees = length(STATUSCD),
            Tot_BA = sum(Basal_area)) %>% 
  ggplot(., aes(x=Treatment, y=Tot_BA, fill=Treatment)) +
  geom_boxplot()

# Test Run of Torgegram for Delaware River Basin
# https://usepa.github.io/SSN2/articles/introduction.html
# https://pet221.github.io/SSNbler/articles/introduction.html

rm(list=ls(all.names=TRUE)) ; cat('\014'); gc()

library(tidyverse)
library(sf)
library(SSN2)
library(SSNbler)
library(gridExtra)
library(rcartocolor)
library(latex2exp)
library(MLmetrics)
library(elevatr)
library(data.table)

################################################################################

path.root = "C:/Users/sm6432/Projects/River-Network-Spatial-Interpolation/"

# get DE sites
DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                     predpts="pred")
sites = unique(DE_ssn$obs$Site.ID)

# get elevation point data
obs.elev = read_sf(paste0(path.root, "data/obs.elevation.gpkg"))
preds.elev = read_sf(paste0(path.root, "data/preds.elevation.gpkg"))

################################################################################

# loop over the study years
flow.df <- fread(paste0(path.root, 'data/Output01_Centiles.txt'),  header=T)
flow.df <- flow.df %>% filter(Site.ID %in% sites)

# annual maxima averaged over a 10 year period
flow.df.wy = flow.df %>% filter(Water.year %in% 1993:2023)
flow.df.seas = flow.df.wy %>%
  # filter(Season == "AnnualMaxima") %>%
  group_by(Site.ID, Season) %>%
  summarise(Cent.99.ave = mean(Cent.99))

# load ssn object
DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                     predpts="pred")
DE_ssn.v2 <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                     predpts="pred")

# join flood magnitude
DE.obs = DE_ssn$obs %>%
  left_join(flow.df.seas, by="Site.ID")

DE.obs.v2 = DE_ssn$obs %>%
  left_join(flow.df.wy %>% filter(Season == "AnnualMaxima"), by="Site.ID")

DE.obs = DE.obs %>%
  left_join(obs.elev[,c("rid", "elevation")] %>% st_drop_geometry(), by="rid")

DE_ssn$obs = DE.obs
DE_ssn.v2$obs = DE.obs.v2
################################################################################
# plot of modeled annual 40 year flood
gg.colors = carto_pal(10, "Earth")

DE_ssn$obs$Season = factor(DE_ssn$obs$Season,
                           levels=c("AnnualMaxima", "DJF", "MAM", "JJA", "SON"))

gg1 = ggplot() +
  geom_sf(data=DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="darkgray") +
  scale_linewidth(range=c(0.2,4), guide = NULL) +
  geom_sf(data=DE_ssn$obs, aes(color=Cent.99.ave, size=afvArea)) +
  scale_size(guide=NULL) +
  scale_color_stepsn(n.breaks=10, colors=gg.colors, name="0.01-AEP",
                     transform="log10") +
  facet_wrap(~Season, nrow=1) +
  theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        # legend.position = c(0.9,0.7),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box.background = element_rect(color="black")) +
  labs(title="1993-2023 Average 100 yr Flood")

gg2 = ggplot() +
  geom_sf(data=DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="darkgray") +
  scale_linewidth(range=c(0.2,4), guide = NULL) +
  geom_sf(data=DE_ssn.v2$obs, aes(color=Cent.99, size=afvArea)) +
  scale_size(guide=NULL) +
  scale_color_stepsn(n.breaks=10, colors=gg.colors, name="0.01-AEP",
                     transform="log10") +
  facet_wrap(~Water.year, nrow=3) +
  theme_bw() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        # legend.position = c(0.9,0.15),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box.background = element_rect(color="black")) +
  labs(title="1993-2023 100 yr Flood")

ggsave(paste0(path.root,'results/DE-flood-magnitude/1993-2023-0.01AEP-ave-season-log.png'),
       gg1, device='png', width=6*2.4, height=2*2.4, unit='in', dpi=300)
ggsave(paste0(path.root,'results/DE-flood-magnitude/1993-2023-0.01AEP-annual-log.png'),
       gg2, device='png', width=7*2.4, height=4*2.4, unit='in', dpi=300)
################################################################################
# Torgegram

# average over 30 years
flow.df.seas = flow.df.wy %>%
  # filter(Season == "AnnualMaxima") %>%
  group_by(Site.ID, Season) %>%
  summarise(Cent.99.ave = mean(Cent.99))

tg.orig = seas.names = NULL
for (seas.name in c("DJF", "MAM", "JJA", "SON")) {
  # load ssn object
  DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                       predpts="pred")
  
  # join flood magnitude
  flow.df.seas.i = flow.df.seas %>% filter(Season== seas.name)
  DE.obs = DE_ssn$obs %>%
    left_join(flow.df.seas.i, by="Site.ID")
  
  DE.obs = DE.obs %>%
    left_join(obs.elev[,c("rid", "elevation")] %>% st_drop_geometry(), by="rid")
  
  DE_ssn$obs = DE.obs
  
  tg.orig.seas <- Torgegram(
    formula = Cent.99.ave~1,
    ssn.object = DE_ssn,
    bins=10, cutoff=max(DE_ssn$obs$upDist)/2,
    cloud=F,
    type = c("flowcon", "flowuncon", "euclid")
  )
  tg.orig = c(tg.orig, tg.orig.seas)
  seas.names = c(seas.names, rep(seas.name, 10))
} # end season loop

tg.flowcon = rbind(tg.orig[[1]], tg.orig[[4]], tg.orig[[7]], tg.orig[[10]])
tg.flowcon$Season = factor(seas.names, levels=c("DJF", "MAM", "JJA", "SON"))
tg.flowuncon = rbind(tg.orig[[2]], tg.orig[[5]], tg.orig[[8]], tg.orig[[11]])
tg.flowuncon$Season = factor(seas.names, levels=c("DJF", "MAM", "JJA", "SON"))
tg.euclid = rbind(tg.orig[[3]], tg.orig[[6]], tg.orig[[9]], tg.orig[[12]])
tg.euclid$Season = factor(seas.names, levels=c("DJF", "MAM", "JJA", "SON"))

gg = ggplot() +
  geom_point(data=tg.flowcon,
             mapping=aes(dist, gamma, size=np, color="Flow connected"), alpha=0.7) +
  geom_point(data=tg.flowuncon,
             mapping=aes(dist, gamma, size=np, color="Flow unconnected"), alpha=0.7) +
  geom_point(data=tg.euclid,
             mapping=aes(dist, gamma, size=np, color="Euclidean"), alpha=0.7) +
  scale_color_manual(values=c("Flow connected"="turquoise",
                              "Flow unconnected"="orange",
                              "Euclidean"="gray")) +
  facet_wrap(~Season) +
  xlab("Distance") + labs(title=paste0("0.01-AEP Average")) +
  ylab("Semivariance") +
  theme_bw() + scale_size(guide = 'none') +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="black"))

ggsave(paste0(path.root,'results/DE-flood-magnitude/0.01AEP-seas-torgegram.png'),
       gg, device='png', width=7, height=4, unit='in', dpi=300)
################################################################################

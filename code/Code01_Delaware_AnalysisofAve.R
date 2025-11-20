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

################################################################################

path.root = "C:/Users/sm6432/Projects/River-Network-Spatial-Interpolation/"

# load DE stations
DE.stations <- sf::st_read(paste0(path.root,"data/DE_stations.gpkg"))
crs.usa = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
DE.stations = st_transform(DE.stations, crs.usa)

# get DE sites
DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                     predpts="pred")
sites = unique(DE_ssn$obs$Site.ID)
################################################################################

# loop over the study years
flow.df <- read.csv(paste0(path.root, 'data/Output01_Centiles.txt'))
flow.df <- flow.df %>% filter(Site.ID %in% sites)
water.years <- sort(unique(flow.df$Water.year))
df.AIC = NULL

tg.flowcon.dist = tg.flowcon.gamma = tg.flowcon.np = vector("list", 5*75)
tg.flowuncon.dist = tg.flowuncon.gamma = tg.flowuncon.np = vector("list", 5*75)
tg.euclid.dist = tg.euclid.gamma = tg.euclid.np = vector("list", 5*75)
tg.year = tg.season = vector("list", 5*75)
counter = 1

for (water.year in water.years[2:length(water.years)]) {
  flow.df.wy = flow.df %>% filter(Water.year == water.year)
  
  # track plots
  seas.resids = seas.torgegram.orig = seas.torgegram.resid = vector("list", 5)
  
  # loop over seasons
  seas = c("DJF", "MAM", "JJA", "SON", "AnnualMaxima")
  i = 1
  
  for (i in 1:length(seas)) {
    DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                         predpts="pred")
    
    # join flood magnitude
    seas.name = seas[i]
    print(seas.name)
    seas.flow.df <- flow.df.wy %>% filter(Season == seas.name)
    
    DE.obs = DE_ssn$obs %>%
      left_join(seas.flow.df, by="Site.ID")
    
    # load results to ssn object
    DE_ssn$obs = DE.obs
    
    # check for response variability
    no.variability = length(unique(DE.obs$Cent.97.5)) == 1
    
    if (no.variability) {
      # cannot model for parameters with no variability
      gg.colors = carto_pal(15, "Sunset")

      tg.flowcon.dist[[counter]] <- rep(NA, 15)
      tg.flowcon.gamma[[counter]] <- rep(NA, 15)
      tg.flowcon.np[[counter]] <- rep(NA, 15)
      tg.flowuncon.dist[[counter]] <- rep(NA, 15)
      tg.flowuncon.gamma[[counter]] <- rep(NA, 15)
      tg.flowuncon.np[[counter]] <- rep(NA, 15)
      tg.euclid.dist[[counter]] <- rep(NA, 15)
      tg.euclid.gamma[[counter]] <- rep(NA, 15)
      tg.euclid.np[[counter]] <- rep(NA, 15)
      tg.year[[counter]] <- rep(water.year, 15)
      tg.season[[counter]] <- rep(seas.name, 15)
      
    } else {
      # # ssn model specification
      # tailupparam = taildownparam = euclidparam = NULL
      # ssn_mods <- list()
      # counter = 1
      # # spatial interpolation function to compare models
      # for (tailup_type in c("none", "exponential", "spherical", "linear")) {
      #   for (taildown_type in c("none", "exponential", "spherical", "linear")) {
      #     for (euclid_type in c("none", "exponential", "spherical", "cubic")) {
      # # for (tailup_type in c("none", "exponential")) {
      # #   for (taildown_type in c("none", "exponential")) {
      # #     for (euclid_type in c("none", "exponential")) {
      #       print(paste0("Tailup: ", tailup_type, ", Taildown: ", taildown_type,
      #                    ", Euclid: ", euclid_type))
      #       ssn_mod <- ssn_lm(
      #         formula = Cent.97.5~1,
      #         ssn.object = DE_ssn,
      #         tailup_type = tailup_type,
      #         taildown_type = taildown_type,
      #         euclid_type = euclid_type,
      #         additive = "afvArea",
      #         estmethod = "reml")
      #       tailupparam = c(tailupparam, tailup_type)
      #       taildownparam = c(taildownparam, taildown_type)
      #       euclidparam = c(euclidparam, euclid_type)
      #       ssn_mods[[counter]] <- ssn_mod
      #       counter = counter + 1
      #     } # end euclidean loop
      #   } # end tail down loop
      # } # end tail up loop
      # 
      # AICs <- NULL
      # CV.rmspes <- NULL
      # for (s in ssn_mods) {
      #   g = glance(s)
      #   # print(g)
      #   AICs <- c(AICs, g$AIC)
      #   # loocv_mod <- loocv(s)
      #   # CV.rmspes <- c(CV.rmspes, loocv_mod$RMSPE)
      # }
      # 
      # # get the smallest AIC or RMSPE
      # # df <- data.frame(i=1:length(AICs), AIC=AICs, CV=CV.rmspes,
      # #                  euclid=euclidparam, tailup=tailupparam, taildown=taildownparam,
      # #                  season=seas.name)
      # df <- data.frame(i=1:length(AICs), AIC=AICs,
      #                  euclid=euclidparam, tailup=tailupparam, taildown=taildownparam,
      #                  season=seas.name)
      # df <- df %>% filter(AIC == min(AIC))
      # # df <- df %>% filter(CV == min(CV))
      # df.AIC = rbind(df.AIC, df)
      # min.idx <- df$i[1]
      # 
      # # leave-one-out cross validation
      # ssn_mod = ssn_mods[[min.idx]]
      # loocv_mod <- loocv(ssn_mod, cv_predict=T, se.fit=T)
      # 
      # # cross validation results
      # mod_train <- augment(ssn_mod)
      # mod_train$cv_pred = loocv_mod$cv_predict
      # mod_train$cv_se = loocv_mod$se.fit
      # mod_train$cv_resid = mod_train$cv_pred - mod_train$Cent.97.5
      # 
      # # combine data
      # mod_train$value.type = "Residual"
      # mod_train$value = mod_train$cv_resid
      # mod_train$se.fit = loocv_mod$se.fit
      # mod_train$season = seas.name
      # 
      # ################################################################################
      # gg.colors = carto_pal(15, "Sunset")
      # 
      # if (seas.name == "AnnualMaxima") {
      #   gg1 = ggplot() +
      #     geom_sf(data = DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="gray") +
      #     geom_sf(data = mod_train, aes(fill = value, size=-log10(se.fit)/5),
      #             color="black", shape=21) +
      #     scale_fill_stepsn(name="CV Resid", colors=carto_pal(15, "Geyser"),
      #                       n.breaks=15, limits=c(-10000,10000)) +
      #     coord_sf(datum = 4326) +
      #     facet_grid(season ~ value.type) +
      #     theme_bw() + scale_linewidth(guide = 'none') +
      #     scale_size(guide='none') +
      #     # guides(fill="none") +
      #     theme(plot.margin = unit(c(0,0,0,0), "cm"),
      #           # legend.position = c(0.9,0.7),
      #           legend.position = "right",
      #           legend.direction = "vertical",
      #           legend.box.background = element_rect(color="black"))
      # } else {
      #   gg1 = ggplot() +
      #     geom_sf(data = DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="gray") +
      #     geom_sf(data = mod_train, aes(fill = value, size=-log10(se.fit)/5),
      #             color="black", shape=21) +
      #     scale_fill_stepsn(name="CV Resid", colors=carto_pal(15, "Geyser"),
      #                       n.breaks=15, limits=c(-10000,10000)) +
      #     coord_sf(datum = 4326) +
      #     facet_grid(season ~ value.type) +
      #     theme_bw() + scale_linewidth(guide = 'none') +
      #     scale_size(guide='none') +
      #     # guides(fill="none") +
      #     theme(plot.margin = unit(c(0,0,0,0), "cm"),
      #           # legend.position = c(0.9,0.7),
      #           legend.position = "none",
      #           legend.direction = "vertical",
      #           legend.box.background = element_rect(color="black"))
      # }
      # 
      # seas.resids[[i]] <- gg1
      # 
      # ################################################################################
      # # refit torgegram to residuals
      # DE_ssn$obs$resid = mod_train$cv_resid
      
      tg.orig <- Torgegram(
        formula = Cent.97.5~1,
        ssn.object = DE_ssn,
        bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
        type = c("flowcon", "flowuncon", "euclid")
      )
      
      # tg.mod <- Torgegram(
      #   formula = resid ~ 1,
      #   ssn.object = DE_ssn,
      #   bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
      #   type = c("flowcon", "flowuncon", "euclid")
      # )
      
      gg2 = ggplot() +
        geom_point(data=tg.orig$flowcon,
                   mapping=aes(dist, gamma, size=np, color="Flow connected"), alpha=0.7) +
        geom_point(data=tg.orig$flowuncon,
                   mapping=aes(dist, gamma, size=np, color="Flow unconnected"), alpha=0.7) +
        geom_point(data=tg.orig$euclid,
                   mapping=aes(dist, gamma, size=np, color="Euclidean"), alpha=0.7) +
        scale_color_manual(values=c("Flow connected"="turquoise",
                                    "Flow unconnected"="orange",
                                    "Euclidean"="gray")) +
        xlab("Distance") + labs(title=paste0(seas.name, " Original")) +
        ylab("Semivariance") +
        theme_bw() + scale_size(guide = 'none') +
        theme(legend.position="none",
              legend.title = element_blank(),
              legend.box.background = element_rect(color="black"))
      
      # gg3 = ggplot() +
      #   geom_point(data=tg.mod$flowcon, mapping=aes(dist, gamma, size=np),
      #              color="turquoise", alpha=0.7) +
      #   geom_point(data=tg.mod$flowuncon, mapping=aes(dist, gamma, size=np),
      #              color="orange", alpha=0.7) +
      #   geom_point(data=tg.mod$euclid, mapping=aes(dist, gamma, size=np),
      #              color="gray", alpha=0.7) +
      #   xlab("Distance") + labs(title=paste0(seas.name, " Model Residuals")) +
      #   ylab("Semivariance") +
      #   theme_bw() + scale_size(guide = 'none')
      
      seas.torgegram.orig[[i]] <- gg2
      tg.flowcon.dist[[counter]] <- tg.orig$flowcon$dist
      tg.flowcon.gamma[[counter]] <- tg.orig$flowcon$gamma
      tg.flowcon.np[[counter]] <- tg.orig$flowcon$np
      tg.flowuncon.dist[[counter]] <- tg.orig$flowuncon$dist
      tg.flowuncon.gamma[[counter]] <- tg.orig$flowuncon$gamma
      tg.flowuncon.np[[counter]] <- tg.orig$flowuncon$np
      tg.euclid.dist[[counter]] <- tg.orig$euclid$dist
      tg.euclid.gamma[[counter]] <- tg.orig$euclid$gamma
      tg.euclid.np[[counter]] <- tg.orig$euclid$np
      tg.year[[counter]] <- rep(water.year, 15)
      tg.season[[counter]] <- rep(seas.name, 15)

    } # end if else variability
    counter = counter + 1
  } # end season loop
  
  ################################################################################
  
  # figure of seasonal results
  # gg = do.call('grid.arrange', c(seas.resids, seas.torgegram.orig, seas.torgegram.resid, ncol=5))
  # ggsave(paste0(path.root,'results/DE-flood-magnitude/cv-results-', water.year,'-model-results-AIC.png'), gg, device='png', width=7*2,
  #        height=5*2, unit='in', dpi=300)
  
  # gg = do.call('grid.arrange', c(seas.torgegram.orig, ncol=5))
  # ggsave(paste0(path.root,'results/DE-flood-magnitude/cv-results-', water.year,'.png'), gg, device='png', width=7*2,
  #        height=2*2, unit='in', dpi=300)
  
  # best model
  # paste0("Water Year: ", water.year, ", Best model: ", df.AIC)
  
  
} # end model parameter loop

# write.csv(df.AIC, paste0(path.root, 'results/DE-ssn-structure/seasonal-covariance-structure-DE-flood-magnitude.csv'), row.names = F)

################################################################################
# spatial dependence change over time

# combine results
years = data.frame(do.call(rbind, tg.year)) %>% pivot_longer(cols=1:15, values_to="year")
seasons = data.frame(do.call(rbind, tg.season)) %>% pivot_longer(cols=1:15, values_to="season")

flowcon.dist = data.frame(do.call(rbind, tg.flowcon.dist)) %>% pivot_longer(cols=1:15, values_to="flowcon.dist")
flowcon.gamma = data.frame(do.call(rbind, tg.flowcon.gamma)) %>% pivot_longer(cols=1:15, values_to="flowcon.gamma")
flowcon.np = data.frame(do.call(rbind, tg.flowcon.np)) %>% pivot_longer(cols=1:15, values_to="flowcon.np")
flowcon.df = data.frame(dist=flowcon.dist$flowcon.dist, gamma=flowcon.gamma$flowcon.gamma,
                        np=flowcon.np$flowcon.np, year=years$year, season=seasons$season)

flowuncon.dist = data.frame(do.call(rbind, tg.flowuncon.dist)) %>% pivot_longer(cols=1:15, values_to="flowuncon.dist")
flowuncon.gamma = data.frame(do.call(rbind, tg.flowuncon.gamma)) %>% pivot_longer(cols=1:15, values_to="flowuncon.gamma")
flowuncon.np = data.frame(do.call(rbind, tg.flowuncon.np)) %>% pivot_longer(cols=1:15, values_to="flowuncon.np")
flowuncon.df = data.frame(dist=flowuncon.dist$flowuncon.dist, gamma=flowuncon.gamma$flowuncon.gamma,
                        np=flowuncon.np$flowuncon.np, year=years$year, season=seasons$season)

euclid.dist = data.frame(do.call(rbind, tg.euclid.dist)) %>% pivot_longer(cols=1:15, values_to="euclid.dist")
euclid.gamma = data.frame(do.call(rbind, tg.euclid.gamma)) %>% pivot_longer(cols=1:15, values_to="euclid.gamma")
euclid.np = data.frame(do.call(rbind, tg.euclid.np)) %>% pivot_longer(cols=1:15, values_to="euclid.np")
euclid.df = data.frame(dist=euclid.dist$euclid.dist, gamma=euclid.gamma$euclid.gamma,
                          np=euclid.np$euclid.np, year=years$year, season=seasons$season)

# filter by 10 years
flowcon.df.10 <- flowcon.df %>% filter(year %in% 1949:1959) %>% mutate(method = "Flowcon")
flowuncon.df.10 <- flowuncon.df %>% filter(year %in% 1949:1959) %>% mutate(method = "Flowuncon")
euclid.df.10 <- euclid.df %>% filter(year %in% 1949:1959) %>% mutate(method = "Euclid")
gg.df <- rbind(flowcon.df.10, flowuncon.df.10, euclid.df.10)

gg = ggplot(gg.df) +
  geom_point(mapping=aes(x=dist, y=gamma, size=np, color=year), alpha=0.5) +
  scale_color_viridis_c() +
  facet_grid(season~method) +
  theme_bw() + labs(title="Variograms for 1949 to 1959")

ggsave(paste0(path.root,'results/DE-flood-magnitude/variogram-1949-1959.png'), gg, device='png', width=7,
       height=5.5, unit='in', dpi=300)

gg = ggplot(gg.df) +
  geom_point(mapping=aes(x=dist, y=gamma, size=np, color=method), alpha=0.5) +
  scale_color_viridis_d() +
  facet_grid(season~year) +
  theme_bw() + labs(title="Variograms for 1949 to 1959")

ggsave(paste0(path.root,'results/DE-flood-magnitude/variogram-by-year-1949-1959.png'), gg, device='png', width=7*2,
       height=4*2, unit='in', dpi=300)

################################################################################
# create a torgegram using averaged data from 10 year period
flow.df.wy = flow.df %>% filter(Water.year %in% 1949:1959)

# track plots
seas.torgegram.orig = vector("list", 5)

# loop over seasons
seas = c("DJF", "MAM", "JJA", "SON", "AnnualMaxima")

# join flood magnitude
for (i in 1:length(seas)) {
  seas.name = seas[i]
  print(seas.name)
  
  DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                       predpts="pred")
  
  seas.flow.df <- flow.df.wy %>% filter(Season == seas.name)
  
  # take average flow over the 10 year period for a given site
  seas.flow.df = seas.flow.df %>% group_by(Site.ID) %>% summarise(Cent.97.5.ave = mean(Cent.97.5))
  
  # load results to ssn object
  DE.obs = DE_ssn$obs %>%
    left_join(seas.flow.df, by="Site.ID")
  
  DE_ssn$obs = DE.obs
  
  tg.orig <- Torgegram(
    formula = Cent.97.5.ave~1,
    ssn.object = DE_ssn,
    bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
    type = c("flowcon", "flowuncon", "euclid")
  )
  
  gg2 = ggplot() +
    geom_point(data=tg.orig$flowcon,
               mapping=aes(dist, gamma, size=np, color="Flow connected"), alpha=0.7) +
    geom_point(data=tg.orig$flowuncon,
               mapping=aes(dist, gamma, size=np, color="Flow unconnected"), alpha=0.7) +
    geom_point(data=tg.orig$euclid,
               mapping=aes(dist, gamma, size=np, color="Euclidean"), alpha=0.7) +
    scale_color_manual(values=c("Flow connected"="turquoise",
                                "Flow unconnected"="orange",
                                "Euclidean"="gray")) +
    xlab("Distance") + labs(title=paste0(seas.name, " Original")) +
    ylab("Semivariance") +
    theme_bw() + scale_size(guide = 'none') +
    theme(legend.position="none",
          legend.title = element_blank(),
          legend.box.background = element_rect(color="black"))
  seas.torgegram.orig[[i]] <- gg2
} # end season loop

gg = do.call('grid.arrange', c(seas.torgegram.orig, nrow=5))
ggsave(paste0(path.root,'results/DE-flood-magnitude/variogram-ave-10-yr-1949-1959', water.year,'.png'), gg, device='png', width=7*2,
       height=2*2, unit='in', dpi=300)

################################################################################
# workflow plot
gg.colors = carto_pal(10, "ag_GrnYl")
gg.df = NULL
for (i in 1:length(seas)) {
  seas.name = seas[i]
  DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                       predpts="pred")
  
  flow.df.seas = flow.df.wy %>% filter(Season == seas.name)
  
  DE.obs = DE_ssn$obs %>%
    left_join(flow.df.seas, by="Site.ID")
  
  gg = DE.obs %>%
    ggplot() +
    geom_sf(data = DE_ssn$edges, color="gray") +
    geom_sf(data=DE_ssn$edges, color="gray") +
    geom_sf(aes(color=Cent.97.5)) +
    facet_wrap(~Water.year, nrow=2) +
    scale_color_viridis_b(n.breaks=10, direction=-1)
  
  # ggsave(paste0(path.root,'results/DE-flood-magnitude/maps-10-yr-1949-1959.png'),
  #        gg, device='png', width=4*2.4, height=2*2.4, unit='in', dpi=300)
  
  # average map
  DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                       predpts="pred")
  
  flow.df.seas = flow.df.wy %>% filter(Season == seas.name) %>%
    group_by(Site.ID) %>% summarise(Cent.97.5.ave = mean(Cent.97.5))
  
  DE.obs = DE_ssn$obs %>%
    left_join(flow.df.seas, by="Site.ID")
  
  gg = DE.obs %>%
    ggplot() +
    geom_sf(data = DE_ssn$edges, color="gray") +
    geom_sf(data=DE_ssn$edges, color="gray") +
    geom_sf(aes(color=Cent.97.5.ave)) +
    # facet_wrap(~Water.year, nrow=2) +
    scale_color_viridis_b(n.breaks=10, direction=-1)
  
  # ggsave(paste0(path.root,'results/DE-flood-magnitude/maps-10-yr-ave.png'),
  #        gg, device='png', width=2*2.4, height=2*2.4, unit='in', dpi=300)
  
  # load results to ssn object
  DE.obs = DE_ssn$obs %>%
    left_join(flow.df.seas, by="Site.ID")
  
  DE_ssn$obs = DE.obs
  
  tg.orig <- Torgegram(
    formula = Cent.97.5.ave~1,
    ssn.object = DE_ssn,
    bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
    type = c("flowcon", "flowuncon", "euclid")
  )
  
  # gg = ggplot() +
  #   geom_point(data=tg.orig$flowcon,
  #              mapping=aes(dist, gamma, size=np, color="Flow connected"), alpha=0.7) +
  #   geom_point(data=tg.orig$flowuncon,
  #              mapping=aes(dist, gamma, size=np, color="Flow unconnected"), alpha=0.7) +
  #   geom_point(data=tg.orig$euclid,
  #              mapping=aes(dist, gamma, size=np, color="Euclidean"), alpha=0.7) +
  #   scale_color_manual(values=c("Flow connected"="turquoise",
  #                               "Flow unconnected"="orange",
  #                               "Euclidean"="gray")) +
  #   xlab("Distance") + labs(title=paste0(seas.name)) +
  #   ylab("Semivariance") +
  #   theme_bw() + scale_size(guide = 'none') +
  #   theme(legend.position="none",
  #         legend.title = element_blank(),
  #         legend.box.background = element_rect(color="black"))
  # 
  # ggsave(paste0(path.root,'results/DE-flood-magnitude/tg-of-10-yr-ave.png'),
  #        gg, device='png', width=4*2.4, height=2*2.4, unit='in', dpi=300)
  seas.gg.df = data.frame(dist = c(tg.orig$flowcon$dist, tg.orig$flowuncon$dist, tg.orig$euclid$dist),
                     gamma = c(tg.orig$flowcon$gamma, tg.orig$flowuncon$gamma, tg.orig$euclid$gamma),
                     np = c(tg.orig$flowcon$np, tg.orig$flowuncon$np, tg.orig$euclid$np),
                     method=c(rep("Flowcon", 15), rep("Flowuncon", 15), rep("Euclid", 15)),
                     season=seas.name)
  gg.df <- rbind(gg.df, seas.gg.df)
} # end season loop

gg = ggplot(gg.df) +
  geom_point(mapping=aes(x=dist, y=gamma, size=np), alpha=0.5) +
  scale_color_viridis_c() +
  facet_grid(season~method) +
  theme_bw() + labs(title="Variogram of averaged Cent.97.5 for 1949 to 1959")

ggsave(paste0(path.root,'results/DE-flood-magnitude/ave-cent-variogram-1949-1959.png'), gg, device='png', width=7,
       height=5.5, unit='in', dpi=300)

# alternative method
gg.group = gg.df %>%
  group_by(dist, np, season, method) %>%
  summarise(gamma.ave = mean(gamma, na.rm=T))

gg = ggplot(gg.group) +
  geom_point(mapping=aes(x=dist, y=gamma.ave, size=np), alpha=0.5) +
  scale_color_viridis_c() +
  facet_grid(season~method) +
  theme_bw() + labs(title="Averaged variogram for 1949 to 1959")

ggsave(paste0(path.root,'results/DE-flood-magnitude/ave-variogram-1949-1959.png'), gg, device='png', width=7,
       height=5.5, unit='in', dpi=300)

################################################################################
# model fit

DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                     predpts="pred")

flow.df.seas = flow.df.wy %>% filter(Season == "AnnualMaxima")

DE.obs = DE_ssn$obs %>%
  left_join(flow.df.seas, by="Site.ID")

gg = DE.obs %>%
  ggplot() +
  geom_sf(data = DE_ssn$edges, color="gray") +
  geom_sf(data=DE_ssn$edges, color="gray") +
  geom_sf(aes(color=Cent.97.5)) +
  facet_wrap(~Water.year, nrow=2) +
  scale_color_viridis_b(n.breaks=10, direction=-1)

# average map
DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                     predpts="pred")

flow.df.seas = flow.df.wy %>% filter(Season == seas.name) %>%
  group_by(Site.ID) %>% summarise(Cent.97.5.ave = mean(Cent.97.5))

# load results to ssn object
DE.obs = DE_ssn$obs %>%
  left_join(flow.df.seas, by="Site.ID")

DE_ssn$obs = DE.obs

tailupparam = taildownparam = euclidparam = NULL
ssn_mods <- list()
counter = 1
# spatial interpolation function to compare models
# for (tailup_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
#   for (taildown_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
#     for (euclid_type in c("none", "exponential", "spherical", "cubic", "gaussian", "cosine", "pentaspherical", "wave", "jbessel", "gravity", "rquad", "magnetic")) {
for (tailup_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
  for (taildown_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
    for (euclid_type in c("none", "exponential", "spherical", "cubic", "gaussian")) {
      print(paste0("Tailup: ", tailup_type, ", Taildown: ", taildown_type,
                   ", Euclid: ", euclid_type))
      # ssn_mod <- ssn_lm(
      #   formula = Cent.97.5.ave ~ upDist,
      #   ssn.object = DE_ssn,
      #   tailup_type = tailup_type,
      #   taildown_type = taildown_type,
      #   euclid_type = euclid_type,
      #   additive = "afvArea",
      #   estmethod = "reml")
      
      ssn_mod_glm <- ssn_glm(
        family="Gamma",
        formula = Cent.97.5.ave ~ upDist,
        ssn.object = DE_ssn,
        tailup_type = tailup_type,
        taildown_type = taildown_type,
        euclid_type = euclid_type,
        additive = "afvArea",
        estmethod = "reml")
      tailupparam = c(tailupparam, tailup_type)
      taildownparam = c(taildownparam, taildown_type)
      euclidparam = c(euclidparam, euclid_type)
      ssn_mods[[counter]] <- ssn_mod_glm
      counter = counter + 1
    } # end euclidean loop
  } # end tail down loop
} # end tail up loop

AICs <- NULL
for (i in ssn_mods) {
  g = glance(i)
  print(g)
  AICs <- c(AICs, g$AIC)
}

# get the 5 smallest AIC
df <- data.frame(i=1:length(AICs), AIC=AICs)
dfsort <- sort(df$AIC, index.return=T, decreasing=F)
min.idx <- lapply(dfsort, '[', dfsort$x %in% head(unique(dfsort$x), 5))$ix

tailupparam[min.idx[1]]
taildownparam[min.idx[1]]
euclidparam[min.idx[1]]

ssn_mod = ssn_mods[[min.idx[1]]]

plot(ssn_mod)

# ssn_mod <- ssn_glm(
#   family="Gamma",
#   formula = Cent.97.5.ave ~ upDist,
#   ssn.object = DE_ssn,
#   tailup_type = "exponential",
#   taildown_type = "spherical",
#   euclid_type = "cubic",
#   additive = "afvArea",
#   estmethod = "reml")

# plot(ssn_mod)

# check loocv residuals
mod_train <- augment(ssn_mod)
loocv_mod <- loocv(ssn_mod, cv_predict=T, se.fit=T, type="response")
mod_train$cv_pred = loocv_mod$cv_predict
mod_train$cv_se = loocv_mod$se.fit
mod_train$cv_resid = mod_train$cv_pred - mod_train$Cent.97.5.ave

DE_ssn$obs$resid = mod_train$cv_resid

tg.orig <- Torgegram(
  formula = Cent.97.5.ave ~ 1,
  ssn.object = DE_ssn,
  bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
  type = c("flowcon", "flowuncon", "euclid")
)

tg.mod <- Torgegram(
  formula = resid ~ 1,
  ssn.object = DE_ssn,
  bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
  type = c("flowcon", "flowuncon", "euclid")
)

gg1 = ggplot() +
  geom_point(data=tg.orig$flowcon,
             mapping=aes(dist, gamma, size=np, color="Flow connected"), alpha=0.7) +
  geom_point(data=tg.orig$flowuncon,
             mapping=aes(dist, gamma, size=np, color="Flow unconnected"), alpha=0.7) +
  geom_point(data=tg.orig$euclid,
             mapping=aes(dist, gamma, size=np, color="Euclidean"), alpha=0.7) +
  scale_color_manual(values=c("Flow connected"="turquoise",
                              "Flow unconnected"="orange",
                              "Euclidean"="gray")) +
  xlab("Distance") + labs(title=paste0(seas.name, " Original")) +
  ylab("Semivariance") + ylab("Flow Semivariance") +
  theme_bw() + scale_size(guide = 'none') +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.box.background = element_rect(color="black"))

gg2 = ggplot() +
  geom_point(data=tg.mod$flowcon, mapping=aes(dist, gamma, size=np),
             color="turquoise", alpha=0.7) +
  geom_point(data=tg.mod$flowuncon, mapping=aes(dist, gamma, size=np),
             color="orange", alpha=0.7) +
  geom_point(data=tg.mod$euclid, mapping=aes(dist, gamma, size=np),
             color="gray", alpha=0.7) +
  xlab("Distance") + labs(title=paste0(seas.name, " Flow Model Residuals")) +
  ylab("Semivariance") + ylab("Flow Semivariance") +
  theme_bw() + scale_size(guide = 'none')

gg.results = grid.arrange(gg1, gg2, ncol=1, heights=c(1.2,1))
ggsave(paste0(path.root,'results/DE-tg-residuals.png'), gg.results, device='png', width=8*1.5,
       height=5*1.5, unit='in', dpi=300)

################################################################################
################################################################################
# k-fold cross validation (spatial validation)

# annual maxima
flow.df.seas = flow.df.wy %>% filter(Season == "AnnualMaxima") %>%
  group_by(Site.ID) %>% summarise(Cent.97.5.ave = mean(Cent.97.5))

# random assign to k folds
k = 10
n = nrow(flow.df.seas)
nk = round(n/k)
k.iter = NULL
for (i in 1:k) {k.iter = c(k.iter, rep(i, nk))}
set.seed(42)
k.assignment = sample(k.iter) # randomly permute the groups
DE.obs$fold = k.assignment[1:nrow(flow.df.seas)]

# check k fold assignments
table(DE.obs$fold)

# filter to test on a single fold
df.results = NULL
for (fold.i in 1:k) {
  print(paste0("Fold: ", fold.i, " out of ", k))
  # reload ssn object
  DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                       predpts="pred")
  
  # assign observations and k fold groups
  DE.obs = DE_ssn$obs %>%
    left_join(flow.df.seas, by="Site.ID")
  DE.obs$fold = k.assignment[1:nrow(flow.df.seas)]
  
  # separate train results (leave out kth fold)
  DE.obs = DE.obs %>% mutate(train = fold != fold.i)
  DE.obs$Cent.97.5.ave.train = DE.obs$Cent.97.5.ave
  DE.obs[DE.obs$train == FALSE, ]$Cent.97.5.ave.train = NA
  DE_ssn$obs = DE.obs
  
  # ssn model on training data
  ssn_mod_glm <- ssn_glm(
    family="Gamma",
    formula = Cent.97.5.ave.train ~ upDist,
    ssn.object = DE_ssn,
    tailup_type = "none",
    taildown_type = "epa",
    euclid_type = "none",
    additive = "afvArea",
    estmethod = "reml")
  
  # predict on test data
  mod_train <- augment(ssn_mod_glm, newdata=".missing", type.predict="response")
  mod_train$resid <- mod_train$Cent.97.5.ave - mod_train$.fitted
  
  # track residuals across k folds
  df.resid = data.frame(k = fold.i, Site.ID = mod_train$Site.ID, resid = mod_train$resid,
                        Cent.97.5.ave = mod_train$Cent.97.5.ave, fitted = mod_train$.fitted)
  df.results = rbind(df.results, df.resid)

} # end loop over k folds

ggplot(df.results) +
  geom_boxplot(aes(x=as.factor(k), y=resid)) +
  geom_jitter(aes(x=as.factor(k), y=resid), width=0.1, height=0, alpha=0.4) +
  xlab("Kth fold") + ylab(TeX(r'($y - X\hat{\beta}$)')) +
  theme_bw()

ggplot(df.results) +
  geom_point(aes(x=Cent.97.5.ave, y=fitted), alpha=0.6) +
  # coord_fixed() + scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_abline(slope=1, intercept=0) +
  theme_bw()

DE.obs = DE_ssn$obs %>%
  left_join(df.results, by=c("Site.ID", "Cent.97.5.ave"))

gg.colors = carto_pal(10, "Geyser")
ggplot(DE.obs) +
  geom_sf(data = DE_ssn$edges, color="gray") +
  geom_sf(aes(color = resid)) +
  scale_color_gradientn(colors=gg.colors, limits=c(-1000, 1000), oob=scales::squish,
                       n.breaks=10, name=TeX(r'($y - X\hat{\beta}$)'),
                       labels=c("-1000"="")) +
  coord_sf(datum = 4326) +
  theme_bw()

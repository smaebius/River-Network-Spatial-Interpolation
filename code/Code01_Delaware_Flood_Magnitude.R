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

################################################################################

path.root = "C:/Users/sm6432/Projects/River-Network-Spatial-Interpolation/"

# load DE stations
DE.stations <- sf::st_read(paste0(path.root,"data/DE_stations.gpkg"))
crs.usa = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
DE.stations = st_transform(DE.stations, crs.usa)

# get DE sites
DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                     predpts="pred")
sites = unique(DE_ssn$obs$Site.ID)
################################################################################

# loop over the study years
flow.df <- read.csv(paste0(path.root, 'data/Output01_Centiles.txt'))
flow.df <- flow.df %>% filter(Site.ID %in% sites)
water.years <- sort(unique(flow.df$Water.year))
df.AIC = NULL

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
      
      # # create a placeholder dataframe
      # mod_train = DE.obs
      # mod_train$value = DE.obs$Cent.97.5
      # mod_train$se.fit = 0.001
      # mod_train$value.type = "Residual"
      # mod_train$season = seas.name
      # mod_train$cv_resid = 0
      # 
      # gg1 = ggplot() +
      #   geom_sf(data = DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="gray") +
      #   geom_sf(data = mod_train, aes(fill = value, size=-log10(se.fit)/5),
      #           color="black", shape=21) +
      #   scale_fill_stepsn(name="LOOCV Residual", colors=carto_pal(15, "Geyser"),
      #                     n.breaks=15, limits=c(-10000,10000)) +
      #   coord_sf(datum = 4326) +
      #   facet_grid(season ~ value.type) +
      #   theme_bw() + scale_linewidth(guide = 'none') +
      #   scale_size(guide='none') + guides(fill="none") +
      #   theme(plot.margin = unit(c(0,0,0,0), "cm"),
      #         # legend.position = c(0.9,0.7),
      #         legend.position = "right",
      #         legend.direction = "vertical",
      #         legend.box.background = element_rect(color="black"))
      # 
      # seas.resids[[i]] <- gg1
      # 
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
      # seas.torgegram.resid[[i]] <- gg3
      
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
      # seas.torgegram.resid[[i]] <- gg3
    } # end if else variability
  } # end season loop
  
  ################################################################################
  
  # figure of seasonal results
  # gg = do.call('grid.arrange', c(seas.resids, seas.torgegram.orig, seas.torgegram.resid, ncol=5))
  # ggsave(paste0(path.root,'results/DE-flood-magnitude/cv-results-', water.year,'-model-results-AIC.png'), gg, device='png', width=7*2,
  #        height=5*2, unit='in', dpi=300)
  
  gg = do.call('grid.arrange', c(seas.torgegram.orig, ncol=5))
  ggsave(paste0(path.root,'results/DE-flood-magnitude/cv-results-', water.year,'.png'), gg, device='png', width=7*2,
         height=2*2, unit='in', dpi=300)
  
  # best model
  paste0("Water Year: ", water.year, ", Best model: ", df.AIC)
  
  
} # end model parameter loop

write.csv(df.AIC, paste0(path.root, 'results/DE-ssn-structure/seasonal-covariance-structure-DE-flood-magnitude.csv'), row.names = F)

################################################################################

ggplot(df.AIC) +
  geom_boxplot(aes(x=as.factor(""), y=AIC)) +
  # scale_x_continuous(limits=c(0,2)) +
  facet_grid(parameter ~ season, scale="free_y") +
  theme_bw() + theme(axis.title.x = element_blank())

df.AIC %>%
  group_by(euclid, tailup, taildown)

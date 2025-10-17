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

path.root = "C:/Users/sm6432/Projects/Spatial-Interpolation/"

# load DE stations
DE.stations <- sf::st_read(paste0(path.root,"data/DE_stations.gpkg"))
crs.usa = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
DE.stations = st_transform(DE.stations, crs.usa)

################################################################################

# save torgegram plots
seas.torgegram.flowcon = seas.torgegram.flowuncon = seas.torgegram.euclid = vector("list", 4)
seas.torgegram = vector("list", 4)

# save parameter map plots
seas.map.mu.prec.con = vector("list", 4)

# loop over seasons
seas = c("DJF", "MAM", "JJA", "SON")
i = 1

for (i in 1:length(seas)) {
  # add flood magnitude parameters
  seas.name = seas[i]
  seas.params <- read.csv(paste0(path.root, "data/Output03_parameters_", seas.name, ".csv"))
  
  # load ssn
  DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn"),
                       predpts="pred")
  
  # # directed hydrological distance matrices
  # ssn_create_distmat(
  #   ssn.object = DE_ssn,
  #   predpts = "pred",
  #   among_predpts = TRUE,
  #   overwrite = TRUE
  # )
  
  # DE.obs = DE_ssn$obs
  DE.obs = DE_ssn$obs %>%
    left_join(seas.params, by="Site.ID")
  
  # check for zero inflated distribution
  nrow(DE.obs[DE.obs$Nu_intercept != 0,])
  
  # load results to ssn object
  DE_ssn$obs = DE.obs
  # DE_ssn$edges = DE_ssn$edges %>%
  #   left_join(DJF.param)
  
  colnames(DE_ssn$obs)
  
  param.cols <- c("Mu_intercept", "Mu_prec.conc", "Mu_temp.conc", "Mu_prec.lag",
                  "Mu_temp.lag", "Sig_intercept", "Sig_prec.conc", "Sig_temp.conc",
                  "Sig_prec.lag", "Sig_temp.lag")
  
  DE.obs.long = pivot_longer(DE.obs, cols=param.cols, names_to="Parameter", values_to="Value")
  
  # ggplot() +
  #   geom_sf(data = DE_ssn$edges, color="gray") +
  #   # geom_sf(data = DE_ssn$obs, aes(color = Mu_intercept)) +
  #   geom_sf(data=DE.obs.long, aes(color=Value)) +
  #   scale_color_viridis_c(name="DJF") +
  #   facet_wrap(~Parameter) +
  #   coord_sf(datum = 4326) +
  #   theme_bw()
  
  # # plots for the season
  # gg = DE.obs.long %>%
  #   split(.$Parameter) %>%
  #   map(~ ggplot(., aes(color = Value)) +
  #         geom_sf(data = DE_ssn$edges, color="gray") +
  #         geom_sf() +
  #         facet_wrap(~Parameter) +
  #         scale_color_viridis_c(name=seas.name) +
  #         coord_sf(datum = 4326) +
  #         theme_bw() +
  #         theme(plot.margin = unit(c(0,0,0,0), "cm"))) %>%
  #   cowplot::plot_grid(plotlist = .,
  #                      align="hv",
  #                      axis="tb")
  # 
  # cowplot::save_plot(paste0(path.root,'/results/parameters/DE-', seas.name, '.png'), gg, device='png',
  #        base_height=10, base_asp=1.3)
  
  # mu precip conc plot
  gg = ggplot() +
    geom_sf(data = DE_ssn$edges, color="gray") +
    geom_sf(data = DE_ssn$obs, aes(color = Mu_prec.conc)) +
    scale_color_viridis_c(name=seas.name) +
    coord_sf(datum = 4326) +
    theme_bw() +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))
  
  seas.map.mu.prec.con[[i]] <- gg
  
  ################################################################################
  
  # fit spatial model
  names(DE_ssn$obs)
  
  tg <- Torgegram(
    formula = Mu_prec.conc ~ Mu_prec.conc,
    ssn.object = DE_ssn,
    bins=12,
    cutoff=max(DE_ssn$obs$upDist)/2, # limited to half the maximum distance of flow-connected sites
    type = c("flowcon", "flowuncon", "euclid")
  )
  
  # plot(tg, type="flowcon")
  
  if (i == 1) {
    gg.flow = ggplot() +
      geom_point(data=tg$flowcon, mapping=aes(dist, gamma, size=np),
                 color="turquoise", alpha=0.7) +
      geom_point(data=tg$flowuncon, mapping=aes(dist, gamma, size=np),
                 color="orange", alpha=0.7) +
      geom_point(data=tg$euclid, mapping=aes(dist, gamma, size=np),
                 color="gray", alpha=0.7) +
      xlab("Distance") + labs(title=seas.name) +
      ylab("Semivariance") + ylab("Flow Semivariance") +
      theme_bw() + scale_size(guide = 'none')
    
    gg.flowcon = ggplot(tg$flowcon) +
      geom_point(aes(dist, gamma, size=np)) +
      labs(title=seas.name) +
      xlab("Distance") + ylab("Flow Connected") +
      theme_bw() + scale_size(guide = 'none')
    
    gg.flowuncon = ggplot(tg$flowuncon) +
      geom_point(aes(dist, gamma, size=np)) +
      xlab("Distance") + ylab("Flow Unconnected") +
      theme_bw() + scale_size(guide = 'none')
    
    gg.euclid = ggplot(tg$euclid) +
      geom_point(aes(dist, gamma, size=np)) +
      xlab("Distance") + ylab("Euclidean") +
      theme_bw() + scale_size(guide = 'none')
  } else {
    
    gg.flow = ggplot() +
      geom_point(data=tg$flowcon, mapping=aes(dist, gamma, size=np),
                 color="turquoise", alpha=0.7) +
      geom_point(data=tg$flowuncon, mapping=aes(dist, gamma, size=np),
                 color="orange", alpha=0.7) +
      geom_point(data=tg$euclid, mapping=aes(dist, gamma, size=np),
                 color="gray", alpha=0.7) +
      xlab("Distance") + labs(title=seas.name) +
      ylab("") +
      theme_bw() + scale_size(guide = 'none')
    
    gg.flowcon = ggplot(tg$flowcon) +
      geom_point(aes(dist, gamma, size=np)) +
      xlab("Distance") + labs(title=seas.name) +
      ylab("") +
      theme_bw() + scale_size(guide = 'none')
    
    gg.flowuncon = ggplot(tg$flowuncon) +
      geom_point(aes(dist, gamma, size=np)) +
      xlab("Distance") + ylab("") +
      theme_bw() + scale_size(guide = 'none')
    
    gg.euclid = ggplot(tg$euclid) +
      geom_point(aes(dist, gamma, size=np)) +
      xlab("Distance") + ylab("") +
      theme_bw() + scale_size(guide = 'none')
  }
  
  seas.torgegram.flowcon[[i]] <- gg.flowcon
  seas.torgegram.flowuncon[[i]] <- gg.flowuncon
  seas.torgegram.euclid[[i]] <- gg.euclid
  seas.torgegram[[i]] <- gg.flow

} # end season loop

# plot(tg$flowcon$dist, tg$flowcon$gamma)
# plot(tg$flowuncon$dist, tg$flowuncon$gamma)
# plot(tg$euclid$dist, tg$euclid$gamma)
# 
# par(mfrow=c(1,3))
# plot(tg, type="flowcon")
# plot(tg, type="flowuncon")
# plot(tg, type="euclid")
# par(mfrow=c(1,1))
# 
# summary(tg$flowcon)

################################################################################
# figure of seasonal torgegrams
gg = do.call('grid.arrange', c(seas.torgegram, seas.torgegram.flowcon, seas.torgegram.flowuncon,
                          seas.torgegram.euclid, seas.map.mu.prec.con, ncol=4))
ggsave(paste0(path.root,'results/seasonal-DE-torgegram.png'), gg, device='png', width=7*1.8,
       height=5*1.8, unit='in', dpi=300)

################################################################################

# example for SON
tg <- Torgegram(
  formula = Mu_prec.conc ~ 1,
  ssn.object = DE_ssn,
  bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
  type = c("flowcon", "flowuncon", "euclid")
)

par(mfrow=c(1,3))
plot(tg, type="flowcon")
plot(tg, type="flowuncon")
plot(tg, type="euclid")
par(mfrow=c(1,1))

# par(mfrow=c(1,3))
# plot(tg.mod, type="flowcon")
# plot(tg.mod, type="flowuncon")
# plot(tg.mod, type="euclid")
# par(mfrow=c(1,1))

plot(tg, type=c("flowcon", "flowuncon"))
plot(tg)

tailupparam = taildownparam = euclidparam = NULL
ssn_mods <- list()
counter = 1
# spatial interpolation function to compare models
for (tailup_type in c("none", "exponential", "spherical", "linear")) {
  for (taildown_type in c("none", "exponential", "spherical", "linear")) {
    for (euclid_type in c("none", "exponential", "spherical", "cubic")) {
      print(paste0("Tailup: ", tailup_type, ", Taildown: ", taildown_type,
                   ", Euclid: ", euclid_type))
      ssn_mod <- ssn_lm(
        formula = Mu_prec.conc ~ 1,
        ssn.object = DE_ssn,
        tailup_type = tailup_type,
        taildown_type = taildown_type,
        euclid_type = euclid_type,
        additive = "afvArea",
        estmethod = "reml")
      tailupparam = c(tailupparam, tailup_type)
      taildownparam = c(taildownparam, taildown_type)
      euclidparam = c(euclidparam, euclid_type)
      ssn_mods[[counter]] <- ssn_mod
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

ssn_mods[[min.idx[1]]]$residuals

tailupparam[min.idx[1]]
taildownparam[min.idx[1]]
euclidparam[min.idx[1]]

# get the smallest AIC for Euclidean only
df <- data.frame(i=1:length(AICs), AIC=AICs,
                 euclid=euclidparam, tailup=tailupparam, taildown=taildownparam)
df.euclid <- df %>% filter(tailup == "none" & taildown == "none")
df.euclid <- df.euclid %>% filter(AIC == min(AIC))
idx.euclid <- df.euclid$i[1]

# leave-one-out cross validation
ssn_mod = ssn_mods[[min.idx[1]]]

ssn_mod_2 = ssn_mods[[idx.euclid]]

glances(ssn_mod, ssn_mod_2)

loocv_mod <- loocv(ssn_mod, cv_predict=T, se.fit=T)
loocv_nonspatial <- loocv(ssn_mod_2, cv_predict=T, se.fit=T)


loocv(ssn_mod)
loocv(ssn_mod_2)

summary(ssn_mod)

varcomp(ssn_mod)

plot(ssn_mod, which=1)

predict(ssn_mod, newdata = "pred")

# results for test data
aug_preds <- augment(ssn_mod, newdata="pred")
aug_preds_nonspatial <- augment(ssn_mod_2, newdata="pred")

# # distinguish between train and test
# aug_preds$data = "test"
# train.sites = unique(DE.stations$Site.ID)
# aug_preds[aug_preds$Site.ID %in% train.sites,]$data = "train"

# results for training data
mod_train <- augment(ssn_mod)
mod_train$cv_pred = loocv_mod$cv_predict
mod_train$cv_se = loocv_mod$se.fit
mod_train_nonflow <- augment(ssn_mod_2)
mod_train_nonflow$cv_pred = loocv_nonspatial$cv_predict
mod_train_nonflow$cv_se = loocv_nonspatial$se.fit
mod_train$cv_resid = mod_train$cv_pred - mod_train$Mu_prec.conc
mod_train_nonflow$cv_resid = mod_train_nonflow$cv_pred - mod_train_nonflow$Mu_prec.conc

# combine data
aug_preds$type = "Flow"
aug_preds_nonspatial$type = "Nonflow"
aug_preds$value.type = "Prediction"
aug_preds_nonspatial$value.type = "Prediction"
aug_preds$value = aug_preds$.fitted
aug_preds_nonspatial$value = aug_preds_nonspatial$.fitted
mod_train$type = "Flow"
mod_train_nonflow$type = "Nonflow"
mod_train$value.type = "Residual"
mod_train_nonflow$value.type = "Residual"
mod_train$value = mod_train$cv_resid
mod_train_nonflow$value = mod_train_nonflow$cv_resid
mod_train$se.fit = loocv_mod$se.fit
mod_train_nonflow$se.fit = loocv_nonspatial$se.fit
pred.cols = c("type", "value", "value.type", "geometry")
resid.cols = c("type", "value", "value.type", "se.fit", "geometry")
model_results_preds = rbind(aug_preds %>% select(all_of(pred.cols)),
                            aug_preds_nonspatial %>% select(all_of(pred.cols)))
model_results_resid = rbind(mod_train %>% select(all_of(resid.cols)),
                            mod_train_nonflow %>% select(all_of(resid.cols)))
################################################################################
gg.colors = carto_pal(15, "Sunset")

# true values
gg = ggplot() +
  geom_sf(data = DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="gray") +
  geom_sf(data = DE_ssn$obs, aes(fill = Mu_prec.conc), color="black", shape=21, size=2) +
  # scale_fill_viridis_c(name=seas.name, direction=-1) +
  scale_fill_stepsn(name=seas.name, colors=gg.colors, n.breaks=15) +
  coord_sf(datum = 4326) +
  theme_bw() + scale_linewidth(guide = 'none') +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

# predictions
# model_results_preds = model_results %>% filter(value.type == "Prediction")
gg1 = ggplot() +
  geom_sf(data = DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="gray") +
  geom_sf(data = model_results_preds, aes(fill = value, size=se.fit), color="black", shape=21, size=2) +
  # scale_fill_viridis_c(name=seas.name, direction=-1, limits=c(0, 0.03)) +
  scale_fill_stepsn(name=seas.name, colors=gg.colors, n.breaks=15) +
  coord_sf(datum = 4326) +
  facet_grid(type ~ value.type) +
  theme_bw() + scale_linewidth(guide = 'none') +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

# model_results_resid = model_results %>% filter(value.type == "Residual")
gg2 = ggplot() +
  geom_sf(data = DE_ssn$edges, aes(linewidth=log10(afvArea+1)/10), color="gray") +
  geom_sf(data = model_results_resid,aes(fill = value, size=-log10(se.fit)-1),
          color="black", shape=21) +
  # scale_fill_distiller(palette="RdBu", name="LOOCV Residual",
  #                       direction=-1, limits=c(-0.02, 0.02)) +
  scale_fill_stepsn(name="LOOCV Residual", colors=carto_pal(15, "Geyser"), n.breaks=15,
                    limits=c(-0.02, 0.02)) +
  coord_sf(datum = 4326) +
  facet_grid(type ~ value.type) +
  theme_bw() + scale_linewidth(guide = 'none') + scale_size(guid='none') +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

gg.results.all = grid.arrange(gg, gg1, gg2, ncol=3)
ggsave(paste0(path.root,'results/seasonal-DE-model-prediction.png'), gg.results.all, device='png', width=10*1.5,
       height=5*1.5, unit='in', dpi=300)

################################################################################
# refit torgegram to residuals
DE_ssn$obs$resid = mod_train$cv_resid
DE_ssn$obs$residnonflow = mod_train_nonflow$cv_resid

tg.orig <- Torgegram(
  formula = Mu_prec.conc ~ 1,
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

tg.mod.nonflow <- Torgegram(
  formula = residnonflow ~ 1,
  ssn.object = DE_ssn,
  bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
  type = c("flowcon", "flowuncon", "euclid")
)

plot(tg.orig)
plot(tg.mod)

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

gg3 = ggplot() +
  geom_point(data=tg.mod.nonflow$flowcon, mapping=aes(dist, gamma, size=np),
             color="turquoise", alpha=0.7) +
  geom_point(data=tg.mod.nonflow$flowuncon, mapping=aes(dist, gamma, size=np),
             color="orange", alpha=0.7) +
  geom_point(data=tg.mod.nonflow$euclid, mapping=aes(dist, gamma, size=np),
             color="gray", alpha=0.7) +
  xlab("Distance") + labs(title=paste0(seas.name, " Nonflow Model Residuals")) +
  ylab("Semivariance") + ylab("Flow Semivariance") +
  theme_bw() + scale_size(guide = 'none')

gg.results = grid.arrange(gg1, gg2, gg3, ncol=1, heights=c(1.2,1,1))
ggsave(paste0(path.root,'results/DE-tg-residuals.png'), gg.results, device='png', width=8*1.5,
       height=5*1.5, unit='in', dpi=300)

################################################################################

# library(gamlss)
# newdata<-rGA(1000,mu=5000,sigma=0.45)
# hist(newdata)

# qGA(0.50, mu = exp(seas.param.values$Mu_intercept +
#                      seas.param.values$Mu_prec.conc *
#                      revert.NQ(P1, data.seas$P1) +
#                      seas.param.values$Mu_temp.conc * 
#                      revert.NQ(T1, data.seas$T1) +
#                      seas.param.values$Mu_prec.lag *
#                      revert.NQ(P2, data.seas$P2) +
#                      seas.param.values$Mu_temp.lag *
#                      revert.NQ(T2, data.seas$T2)),
    # sigma = exp(seas.param.values$Sig_intercept +
    #               seas.param.values$Sig_prec.conc *
    #               revert.NQ(P1, data.seas$P1) +
    #               seas.param.values$Sig_temp.conc *
    #               revert.NQ(T1, data.seas$T1) +
    #               seas.param.values$Sig_prec.lag *
    #               revert.NQ(P2, data.seas$P2) +
    #               seas.param.values$Sig_temp.lag *
    #               revert.NQ(T2, data.seas$T2)))




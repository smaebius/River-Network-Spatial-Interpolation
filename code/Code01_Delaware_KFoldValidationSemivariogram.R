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
flow.df <- read.csv(paste0(path.root, 'data/Output01_Centiles.txt'))
flow.df <- flow.df %>% filter(Site.ID %in% sites)

# annual maxima averaged over a 10 year period
flow.df.wy = flow.df %>% filter(Water.year %in% 1949:1959)
flow.df.seas = flow.df.wy %>% filter(Season == "AnnualMaxima") %>%
  group_by(Site.ID) %>% summarise(Cent.97.5.ave = mean(Cent.97.5))

# # check sample
# for (i in 1:10) {
#   DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
#                        predpts="pred")
#   
#   flow.df.seas.wy = flow.df.wy %>%
#     group_by(Site.ID, Season) %>% summarise(Cent.97.5.ave = mean(Cent.97.5))
#   
#   # join flood magnitude
#   DE.obs = DE_ssn$obs %>%
#     left_join(flow.df.seas.wy, by="Site.ID")
#   
# 
#   
#   # DE.obs = DE_ssn$obs %>%
#   #   left_join(flow.df.wy %>% filter(Season == "AnnualMaxima"), by="Site.ID")
#   # ggplot() +
#   #   geom_sf(data=DE_ssn$edges, alpha=0.5) +
#   #   geom_sf(data=DE.obs, aes(color=log(Obs))) +
#   #   scale_color_viridis_c(direction=-1) +
#   #   facet_wrap(~Water.year, nrow=2) +
#   #   theme_bw()
#   
#   # randomly remove 10 points and load results to ssn object
#   DE.obs.i = sample_n(DE.obs, 10)
#   
#   gg = ggplot(DE.obs.i) +
#     geom_sf()
#   print(gg)
# }

# ggplot(DE.obs) +
#   geom_sf(data=DE_ssn$edges) +
#   geom_sf(aes(color=log(Cent.97.5.ave))) +
#   facet_wrap(~Season) +
#   scale_color_viridis_c(direction=-1) +
#   theme_bw()

################################################################################
# compare model structures

# load ssn object
DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                     predpts="pred")

# join flood magnitude
DE.obs = DE_ssn$obs %>%
  left_join(flow.df.seas, by="Site.ID")

DE.obs = DE.obs %>%
  left_join(obs.elev[,c("rid", "elevation")] %>% st_drop_geometry(), by="rid")

DE_ssn$obs = DE.obs

tailupparam = taildownparam = euclidparam = NULL
ssn_mods <- list()
counter = 1
prm = proc.time()
# spatial interpolation function to compare models
for (tailup_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
  for (taildown_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
    for (euclid_type in c("none", "exponential", "spherical", "cubic", "gaussian", "cosine", "pentaspherical", "wave", "jbessel", "gravity", "rquad", "magnetic")) {
# for (tailup_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
#   for (taildown_type in c("none", "exponential", "spherical", "linear", "mariah", "epa", "gaussian")) {
#     for (euclid_type in c("none", "exponential", "spherical", "cubic", "gaussian")) {
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
      # result = tryCatch({
      #   ssn_mod_glm <- ssn_glm(
      #   family="Gamma",
      #   formula = Cent.97.5.ave ~ elevation,
      #   ssn.object = DE_ssn,
      #   tailup_type = tailup_type,
      #   taildown_type = taildown_type,
      #   euclid_type = euclid_type,
      #   additive = "afvArea",
      #   estmethod = "reml")
      # 
      #   ssn_mods[[counter]] <- ssn_mod_glm
      #   },
      #   error = function(msg){
      #     return(NA)
      #     })
      # 
      # tailupparam = c(tailupparam, tailup_type)
      # taildownparam = c(taildownparam, taildown_type)
      # euclidparam = c(euclidparam, euclid_type)
      # 
      # if (any(is.na(result))) {
      #   ssn_mods[[counter]] <- NA
      # }
      
      counter = counter + 1

    } # end euclidean loop
  } # end tail down loop
} # end tail up loop
t.elapsed <- round((proc.time()-prm)[3] / 60,2)
t.elapsed

AICs = RMSPEs <- NULL
for (i in ssn_mods) {
  if (any(is.na(i))) {
    AICs <- c(AICs, NA)
  } else {
    g = glance(i)
    print(g)
    AICs <- c(AICs, g$AIC)
    cv.rmspe = loocv(i)$RMSPE
    RMSPEs = c(RMSPEs, cv.rmspe)
  }
}

# get the 5 smallest AIC
# df <- data.frame(i=1:length(AICs), AIC=AICs)
# dfsort <- sort(df$AIC, index.return=T, decreasing=F)
# min.idx <- lapply(dfsort, '[', dfsort$x %in% head(unique(dfsort$x), 5))$ix
df <- data.frame(i=1:length(RMSPEs), RMSPE=RMSPEs)
dfsort <- sort(df$RMSPE, index.return=T, decreasing=F)
min.idx <- lapply(dfsort, '[', dfsort$x %in% head(unique(dfsort$x), 5))$ix

tailupparam[min.idx[1]]
taildownparam[min.idx[1]]
euclidparam[min.idx[1]]


################################################################################

set.seed(42)
n = 50 # iterations
df = df.obs = DE.sites = df.results = NULL
models1 = models2 = models3 = models4 = models5 = models6 = models7 = vector("list", n)
prm = proc.time()
for (i in 1:n) {
  print(i)
  # load ssn object
  DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                       predpts="pred")
  
  # join flood magnitude
  DE.obs = DE_ssn$obs %>%
    left_join(flow.df.seas, by="Site.ID")
  
  DE.obs = DE.obs %>%
    left_join(obs.elev[,c("rid", "elevation")] %>% st_drop_geometry(), by="rid")
  
  # randomly remove 10 points and load results to ssn object
  DE.obs.i = sample_n(DE.obs, 10)
  DE.sites.i = unique(DE.obs.i$Site.ID)
  DE.sites = rbind(DE.sites, data.frame(Site.ID=DE.sites.i, iter=i))
  DE.obs = DE.obs %>% mutate(train = (!Site.ID %in% DE.sites.i))
  # table(DE.obs$train)
  DE.obs$Cent.97.5.ave.train = DE.obs$Cent.97.5.ave
  DE.obs[DE.obs$train == FALSE, ]$Cent.97.5.ave.train = NA
  DE_ssn$obs = DE.obs
  
  # DE_ssn$obs = merge(DE_ssn$obs, DE_ssn$edges[,c("rid", "TotDASqKM")] %>% st_drop_geometry(),
  #                    by="rid", all.x=T)
  
  gg = ggplot(DE.obs.i) +
    geom_sf() + labs(title=i)
  print(gg)
  
  # df.obs = rbind(df.obs, DE.obs.i)
  
  # spatial model on training data
  m1 <- ssn_glm(
    family="Gamma",
    formula = Cent.97.5.ave.train ~ elevation,
    ssn.object = DE_ssn,
    tailup_type = "spherical",
    taildown_type = "none",
    euclid_type = "wave",
    additive = "afvArea",
    estmethod = "reml")
  
  # m2 <- ssn_glm(
  #   family="Gamma",
  #   formula = Cent.97.5.ave.train ~ upDist,
  #   ssn.object = DE_ssn,
  #   tailup_type = "exponential",
  #   taildown_type = "exponential",
  #   euclid_type = "none",
  #   additive = "afvArea",
  #   estmethod = "reml")
  # 
  # m3 <- ssn_glm(
  #   family="Gamma",
  #   formula = Cent.97.5.ave.train ~ upDist,
  #   ssn.object = DE_ssn,
  #   tailup_type = "exponential",
  #   taildown_type = "spherical",
  #   euclid_type = "none",
  #   additive = "afvArea",
  #   estmethod = "reml")
  # 
  # m4 <- ssn_glm(
  #   family="Gamma",
  #   formula = Cent.97.5.ave.train ~ upDist,
  #   ssn.object = DE_ssn,
  #   tailup_type = "exponential",
  #   taildown_type = "linear",
  #   euclid_type = "none",
  #   additive = "afvArea",
  #   estmethod = "reml")
  # 
  # m5 <- ssn_glm(
  #   family="Gamma",
  #   formula = Cent.97.5.ave.train ~ upDist,
  #   ssn.object = DE_ssn,
  #   tailup_type = "exponential",
  #   taildown_type = "mariah",
  #   euclid_type = "none",
  #   additive = "afvArea",
  #   estmethod = "reml")
  # 
  # m6 <- ssn_glm(
  #   family="Gamma",
  #   formula = Cent.97.5.ave.train ~ upDist,
  #   ssn.object = DE_ssn,
  #   tailup_type = "exponential",
  #   taildown_type = "epa",
  #   euclid_type = "none",
  #   additive = "afvArea",
  #   estmethod = "reml")
  # 
  # m7 <- ssn_glm(
  #   family="Gamma",
  #   formula = Cent.97.5.ave.train ~ upDist,
  #   ssn.object = DE_ssn,
  #   tailup_type = "exponential",
  #   taildown_type = "gaussian",
  #   euclid_type = "none",
  #   additive = "afvArea",
  #   estmethod = "reml")
  
  models1[[i]] = m1
  # models2[[i]] = m2
  # models3[[i]] = m3
  # models4[[i]] = m4
  # models5[[i]] = m5
  # models6[[i]] = m6
  # models7[[i]] = m7
  
  # m1_predict = augment(m1, type.residuals = "response", drop=F)
  # m2_predict = augment(m2, type.residuals = "response", drop=F)
  # m3_predict = augment(m3, type.residuals = "response", drop=F)
  m1_test <- augment(m1, newdata=".missing", type.residuals="response")
  # m2_test <- augment(m2, newdata=".missing", type.residuals="response")
  # m3_test <- augment(m3, newdata=".missing", type.residuals="response")
  # m4_test <- augment(m4, newdata=".missing", type.residuals="response")
  # m5_test <- augment(m5, newdata=".missing", type.residuals="response")
  # m6_test <- augment(m6, newdata=".missing", type.residuals="response")
  # m7_test <- augment(m7, newdata=".missing", type.residuals="response")
  m1_test$.resid = exp(m1_test$.fitted) - m1_test$Cent.97.5.ave
  # m2_test$.resid = exp(m2_test$.fitted) - m2_test$Cent.97.5.ave
  # m3_test$.resid = exp(m3_test$.fitted) - m3_test$Cent.97.5.ave
  # m4_test$.resid = exp(m4_test$.fitted) - m4_test$Cent.97.5.ave
  # m5_test$.resid = exp(m5_test$.fitted) - m5_test$Cent.97.5.ave
  # m6_test$.resid = exp(m6_test$.fitted) - m6_test$Cent.97.5.ave
  # m7_test$.resid = exp(m7_test$.fitted) - m7_test$Cent.97.5.ave
  # mod_all1 <- rbind(m1_predict %>% select(-c(".cooksd", ".std.resid", ".hat")),
  #                  m1_test)
  # mod_all2 <- rbind(m2_predict %>% select(-c(".cooksd", ".std.resid", ".hat")),
  #                  m2_test)
  # mod_all3 <- rbind(m3_predict %>% select(-c(".cooksd", ".std.resid", ".hat")),
  #                  m3_test)
  # DE_ssn$obs$fitted1 = mod_all1$.resid
  # DE_ssn$obs$fitted2 = mod_all2$.resid
  # DE_ssn$obs$fitted3 = mod_all3$.resid

  # tg.mod1 <- Torgegram(
  #   formula = fitted1 ~ 1,
  #   ssn.object = DE_ssn,
  #   cloud=T, cutoff=max(DE_ssn$obs$upDist)/2,
  #   type = c("flowcon", "flowuncon", "euclid")
  # )
  # 
  # tg.mod2 <- Torgegram(
  #   formula = fitted2 ~ 1,
  #   ssn.object = DE_ssn,
  #   bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
  #   type = c("flowcon", "flowuncon", "euclid")
  # )
  # 
  # tg.mod3 <- Torgegram(
  #   formula = fitted3 ~ 1,
  #   ssn.object = DE_ssn,
  #   bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
  #   type = c("flowcon", "flowuncon", "euclid")
  # )
  # 
  # tg.mod1$euclid$gamma
  # tg.mod2$euclid$gamma
  # tg.mod3$euclid$gamma
  # 
  # 
  # m = ssn_get_stream_distmat(DE_ssn, name="obs")$dist.net3
  # 
  # h = tg.mod1$flowcon$dist
  # theta.r = m1$coefficients$params_object$tailup[['range']]
  # theta.v = m1$coefficients$params_object$tailup[['de']]
  # gamma = theta.v - theta.v*exp(-h / theta.r)
  # 
  # h = tg.mod2$flowuncon$dist
  # theta.r = m2$coefficients$params_object$taildown[['range']]
  # theta.v = m2$coefficients$params_object$taildown[['de']]
  # gamma = theta.v - theta.v*exp(-h / theta.r)
  # 
  # b.mat <- pmin(m, t(m))
  # 
  # ggplot() +
  #   # geom_point(data=tg.orig$flowcon, aes(dist, gamma), color="black") +
  #   geom_point(data=tg.mod1$flowcon, aes(dist, gamma), color="orange") +
  #   # geom_point(data=tg.mod2$euclid, aes(dist+50, gamma), color="gray") +
  #   # geom_point(data=tg.mod3$euclid, aes(dist+100, gamma), color="turquoise") +
  #   theme_bw()
  # 
  # ggplot() +
  #   geom_point(mapping=aes(tg.mod1$flowcon$gamma, tg.orig$flowcon$gamma))
  # 
  # ggplot()+ geom_sf(data=DE_ssn$edges, aes(color=afvArea)) +
  #   scale_color_viridis_c(transform="log10") +
  #   geom_sf(data=DE_ssn$obs)
  # 
  # # predict on test data
  # mod_train <- augment(ssn_mod_glm, type.predict="response", drop=F)
  # mod_test <- augment(ssn_mod_glm, newdata=".missing", type.predict="response")
  # setdiff(colnames(mod_train), colnames(mod_test))
  # mod_all <- rbind(mod_train %>% select(-c(".cooksd", ".std.resid", ".resid", ".hat")),
  #                  mod_test)
  # DE_ssn$obs$fitted = mod_all$.fitted
  # 
  # tg.mod <- Torgegram(
  #   formula = fitted ~ 1,
  #   ssn.object = DE_ssn,
  #   bins=15, cutoff=max(DE_ssn$obs$upDist)/2,
  #   type = c("flowcon", "flowuncon", "euclid")
  # )
  # 
  # df.i = data.frame(dist=tg.orig$flowcon$dist,
  #                 gamma=c(tg.orig$flowcon$gamma,tg.mod$flowcon$gamma),
  #                 type=c(rep("original", length(tg.orig$flowuncon$dist)),
  #                        rep("model", length(tg.orig$flowuncon$dist))),
  #                 iter=i)
  df.i = data.frame(RMSPE=c(RMSPE(m1_test$.fitted, m1_test$Cent.97.5.ave)),
                    # RMSPE(m2_test$.fitted, m2_test$Cent.97.5.ave),
                    # RMSPE(m3_test$.fitted, m3_test$Cent.97.5.ave),
                    # RMSPE(m4_test$.fitted, m4_test$Cent.97.5.ave),
                    # RMSPE(m5_test$.fitted, m5_test$Cent.97.5.ave),
                    # RMSPE(m6_test$.fitted, m6_test$Cent.97.5.ave),
                    # RMSPE(m7_test$.fitted, m7_test$Cent.97.5.ave)),
                    # type=c("m1", "m2", "m3", "m4", "m5", "m6", "m7"),
                    type="m1",
                    iter=i)
  df <- rbind(df, df.i)
  
  df.results.i = data.frame(resid=c(m1_test$.resid),
                                    # m2_test$.resid, m3_test$.resid,
                                    # m4_test$.resid, m5_test$.resid, m6_test$.resid,
                                    # m7_test$.resid),
                    type=c(rep("m1", 10)),
                           # rep("m2", 10), rep("m3", 10),
                           # rep("m4", 10), rep("m5", 10), rep("m6", 10),
                           # rep("m7", 10)),
                    iter=i)
  df.results <- rbind(df.results, df.results.i)
  
  # ggplot(df) +
  #   geom_point(aes(dist, gamma, color=type))
  
} # end model parameter loop
t.elapsed <- round((proc.time()-prm)[3] / 60,2)
t.elapsed

ggplot(df) +
  # geom_boxplot(aes(x=type, y=RMSPE)) +
  geom_jitter(aes(x=type, y=RMSPE, color=iter), alpha=0.5, width=0.2) +
  scale_color_viridis_c() +
  theme_bw()

ggplot(df.results) +
  geom_boxplot(aes(x=type, y=resid)) +
  geom_jitter(aes(x=type, y=resid, color=iter), alpha=0.05, width=0.2) +
  scale_color_viridis_c() +
  theme_bw()

# s = c("01449360", "01466500")
# DE.s = DE.obs %>% filter(Site.ID == s)
# 
# ggplot(DE.obs.i) +
#   geom_sf() + geom_sf(data=DE.s, color="red")

# torgegram of observations
tg.orig <- Torgegram(
  formula = Cent.97.5.ave~elevation,
  ssn.object = DE_ssn,
  bins=10, cutoff=max(DE_ssn$obs$upDist)/2,
  cloud=T,
  type = c("flowcon", "flowuncon", "euclid")
)

# ave TU Variance Component Comparisons
m1.var = m2.var = m3.var = m4.var = m5.var = m6.var = m7.var = NULL
for (i in 1:n) {
  m1 = models1[[i]]
  # m2 = models2[[i]]
  # m3 = models3[[i]]
  # m4 = models4[[i]]
  # m5 = models5[[i]]
  # m6 = models6[[i]]
  # m7 = models7[[i]]
  m1.var.i = varcomp(m1)$proportion[2]
  # m2.var.i = varcomp(m2)$proportion[3]
  # m3.var.i = varcomp(m3)$proportion[3]
  # m4.var.i = varcomp(m4)$proportion[3]
  # m5.var.i = varcomp(m5)$proportion[3]
  # m6.var.i = varcomp(m6)$proportion[3]
  # m7.var.i = varcomp(m7)$proportion[3]
  m1.var = c(m1.var, m1.var.i)
  # m2.var = c(m2.var, m2.var.i)
  # m3.var = c(m3.var, m3.var.i)
  # m4.var = c(m4.var, m4.var.i)
  # m5.var = c(m5.var, m5.var.i)
  # m6.var = c(m6.var, m6.var.i)
  # m7.var = c(m7.var, m7.var.i)
}
mean(m1.var)
# mean(m2.var)
# mean(m3.var)
# mean(m4.var)
# mean(m5.var)
# mean(m6.var)
# mean(m7.var)

df.summary = df %>% group_by(type) %>% summarise(ave.rmspe = mean(RMSPE),
                                                 med.rmspe = median(RMSPE))

# write.csv(df.AIC, paste0(path.root, 'results/DE-ssn-structure/seasonal-covariance-structure-DE-flood-magnitude.csv'), row.names = F)

################################################################################
# weight by performance
df.m = df %>% filter(type == "m1")

df.m$RMSPE.rev = 1-df.m$RMSPE
df.m$weight = df.m$RMSPE.rev / sum(df.m$RMSPE.rev)

tailup.de = tailup.range = taildown.de = taildown.range = nugget = dispersion = NULL
euclid.de = euclid.range = NULL

for (i in 1:n) {
  df.m.i = df.m %>% filter(iter == i)
  tailup.de.i = models1[[i]]$coefficients$params_object$tailup[['de']]
  tailup.range.i = models1[[i]]$coefficients$params_object$tailup[['range']]
  taildown.de.i = models1[[i]]$coefficients$params_object$taildown[['de']]
  taildown.range.i = models1[[i]]$coefficients$params_object$taildown[['range']]
  euclid.de.i = models1[[i]]$coefficients$params_object$euclid[['de']]
  euclid.range.i = models1[[i]]$coefficients$params_object$euclid[['range']]
  nugget.i = models1[[i]]$coefficients$params_object$nugget[['nugget']]
  dispersion.i = models1[[i]]$coefficients$params_object$dispersion[['dispersion']]
  
  tailup.de = c(tailup.de, tailup.de.i)
  tailup.range = c(tailup.range, tailup.range.i)
  taildown.de = c(taildown.de, taildown.de.i)
  taildown.range = c(taildown.range, taildown.range.i)
  euclid.de = c(euclid.de, euclid.de.i)
  euclid.range = c(euclid.range, euclid.range.i)
  nugget = c(nugget, nugget.i)
  dispersion = c(dispersion, dispersion.i)
} # end loop over iterations

results.df = data.frame(tailup.de, tailup.range, taildown.de, taildown.range,
                        euclid.de, euclid.range,
                        nugget, dispersion,
                        weight=df.m$weight)

results.ave = results.df %>%
  summarise(w.ave.tu.de = sum(tailup.de*weight) / sum(weight),
            w.ave.tu.range = sum(tailup.range*weight) / sum(weight),
            w.ave.td.de = sum(taildown.de*weight) / sum(weight),
            w.ave.td.range = sum(taildown.range*weight) / sum(weight),
            w.ave.eu.de = sum(euclid.de*weight) / sum(weight),
            w.ave.eu.range = sum(euclid.range*weight) / sum(weight),
            w.ave.n = sum(nugget*weight) / sum(weight),
            w.ave.d = sum(dispersion*weight) / sum(weight))

# load ssn
DE_ssn <- ssn_import(paste0(path.root, "data/DE.ssn-v2.ssn"),
                     predpts="pred")

# join flood magnitude
DE.obs = DE_ssn$obs %>%
  left_join(flow.df.seas, by="Site.ID")

DE.obs = DE.obs %>%
  left_join(obs.elev[,c("Site.ID", "elevation")] %>% st_drop_geometry(), by="Site.ID")

DE_ssn$obs = DE.obs

# DE_ssn$obs = merge(DE_ssn$obs, DE_ssn$edges[,c("rid", "TotDASqKM")] %>% st_drop_geometry(),
#                       by="rid", all.x=T)
# DE_ssn$preds$pred = merge(DE_ssn$preds$pred, DE_ssn$edges[,c("rid", "TotDASqKM")] %>% st_drop_geometry(),
#                    by="rid", all.x=T)

DE_ssn$preds$pred = DE_ssn$preds$pred %>%
  left_join(preds.elev[,c("rid", "pid", "elevation")] %>% st_drop_geometry(), by=c("rid", "pid"))

# new ssn with best parameters
m.best <- ssn_glm(
  family="Gamma",
  formula = Cent.97.5.ave ~ elevation,
  ssn.object = DE_ssn,
  tailup_type = "spherical",
  taildown_type = "none",
  euclid_type = "wave",
  # tailup_type = "exponential",
  # taildown_type = "spherical",
  # euclid_type = "gaussian",
  additive = "afvArea",
  tailup_initial = tailup_initial("spherical", de=results.ave$w.ave.tu.de,
                                      range=results.ave$w.ave.tu.range,
                                      known="given"),
  taildown_initial = taildown_initial("none", de=results.ave$w.ave.td.de,
                                      range=results.ave$w.ave.td.range,
                                      known="given"),
  euclid_initial = euclid_initial("wave", de=results.ave$w.ave.eu.de,
                                  range=results.ave$w.ave.eu.range, known="given"),
  nugget_initial = nugget_initial("nugget", nugget=results.ave$w.ave.n, known="given"),
  estmethod = "reml")

plot(m.best)


# m1_predict = augment(m1, type.residuals = "response", drop=F)
m1.predict = augment(m.best, type.residuals = "response", drop=T)
DE_ssn$obs$fitted1 = m1.predict$.resid
DE_ssn$obs$prediction = m1.predict$.fitted

tg.mod1 <- Torgegram(
  formula = fitted1 ~ 1,
  ssn.object = DE_ssn,
  cloud=F, cutoff=max(DE_ssn$obs$upDist)/2,
  type = c("flowcon", "flowuncon", "euclid")
)

plot(tg.mod1)
plot(tg.mod1, "flowcon")
plot(tg.mod1, "flowuncon")
plot(tg.mod1, "euclid")

m.predict = augment(m.best, newdata="pred", interval="confidence")
m.predict$fitted = exp(m.predict$.fitted)
# m.resid = augment(m.best, se.fit = T)
# m.resid$resid = exp(m.resid$.resid)
# m.resid$rid = DE_ssn$obs$rid
# m.predict = merge(m.predict, m.resid[,c("rid", "resid")] %>% st_drop_geometry(),
#                   by="rid", all.x=T)

# add predictions to edges
edges.predict = merge(DE_ssn$edges, m.predict[,c("rid", "fitted")] %>% st_drop_geometry(),
      by="rid", all.x=T)

gg1 = ggplot() +
  geom_sf(data=edges.predict, aes(color=fitted)) +
  geom_sf(data=DE_ssn$obs, aes(color=Cent.97.5.ave)) +
  scale_color_viridis_c(option = "H", direction=-1,
                        limits=c(min(edges.predict$fitted), max(edges.predict$fitted)),
                        oob=scales::squish,
                        name="Cent.97.5.ave") +
  theme_bw() + labs(title="Original and Predictions")

# gg2 = ggplot() +
#   geom_sf(data=edges.predict, aes(color=log(fitted))) +
#   # geom_sf(data=DE_ssn$obs, aes(color=log(Cent.97.5.ave))) +
#   scale_color_viridis_c(option = "H", direction=-1,
#                         # limits=c(min(edges.predict$fitted), max(edges.predict$fitted)),
#                         # oob=scales::squish,
#                         name="Cent.97.5.ave") +
#   theme_bw() + labs(title="Original and Predictions")

# gg2 = ggplot() +
#   geom_sf(data=edges.predict, aes(color=log(fitted))) +
#   geom_sf(data=DE_ssn$obs, aes(color=log(Cent.97.5.ave))) +
#   scale_color_viridis_c(option = "H", direction=-1,
#                         limits=c(min(log(edges.predict$fitted)), max(log(edges.predict$fitted))),
#                         oob=scales::squish,
#                         name="log Cent.97.5.ave") +
#   theme_bw() + labs(title="Original and Predictions")

gg3 = ggplot() +
  geom_sf(data=edges.predict) +
  geom_sf(data=m.predict %>% drop_na(resid), aes(color=resid)) +
  scale_color_viridis_c(direction=-1) +
  theme_bw() + labs(title="Residual")

grid.arrange(gg1, gg2, nrow=1)

min.x.log = min(log(DE_ssn$obs$Cent.97.5.ave), DE_ssn$obs$prediction)
max.x.log = max(log(DE_ssn$obs$Cent.97.5.ave), DE_ssn$obs$prediction)
min.x = min(DE_ssn$obs$Cent.97.5.ave, exp(DE_ssn$obs$prediction))
max.x = max(DE_ssn$obs$Cent.97.5.ave, exp(DE_ssn$obs$prediction))

cv.predict = loocv(m.best, cv_predict=T)$cv_predict
cv.resid = exp(cv.predict) - DE_ssn$obs$Cent.97.5.ave

gg1 = ggplot(data=DE_ssn$obs) +
  geom_abline(slope=1) +
  geom_point(aes(x=cv.predict, y=log(Cent.97.5.ave), size=afvArea),
             alpha=0.6) +
  scale_y_continuous(limits=c(min.x.log, max.x.log)) +
  scale_x_continuous(limits=c(min.x.log, max.x.log)) +
  scale_size(guide="none") + xlab("LOOCV Log Prediction") + ylab("Log Cent.97.5 Ave 1949-1959") +
  theme_bw()

gg2 = ggplot(data=DE_ssn$obs) +
  geom_abline(slope=1) +
  geom_point(aes(x=exp(cv.predict), y=Cent.97.5.ave, size=afvArea),
             alpha=0.6) +
  scale_y_continuous(limits=c(min.x, max.x)) +
  scale_x_continuous(limits=c(min.x, max.x)) +
  scale_size(guide="none") + xlab("LOOCV Prediction") + ylab("Cent.97.5 Ave 1949-1959") +
  theme_bw() + theme(aspect.ratio=1)

grid.arrange(gg1, gg2, nrow=1, widths=c(1,1))

# coefficient of variation
loocv(m.best)$RMSPE / mean(DE_ssn$obs$Cent.97.5.ave) # LOOCV
RMSE(exp(DE_ssn$obs$prediction), DE_ssn$obs$Cent.97.5.ave) / mean(DE_ssn$obs$Cent.97.5.ave) # train

# confidence intervals
confint(m.best)
m.predict.ci = augment(m.best, newdata="pred", interval="confidence")
m.predict.ci$fitted = exp(m.predict.ci$.fitted)
m.predict.ci$fitted.cl = exp(m.predict.ci$.lower)
m.predict.ci$fitted.cu = exp(m.predict.ci$.upper)
m.predict.ci$difference = m.predict.ci$fitted.cu - m.predict.ci$fitted.cl
edges.predict = merge(DE_ssn$edges, m.predict.ci[,c("rid", "fitted", "fitted.cl",
                                                    "fitted.cu", "difference")] %>% st_drop_geometry(),
                      by="rid", all.x=T)

c.min = min(m.predict.ci$fitted, m.predict.ci$fitted.cl, m.predict.ci$fitted.cu)
c.max = max(m.predict.ci$fitted, m.predict.ci$fitted.cl, m.predict.ci$fitted.cu)

gg1 = ggplot() +
  geom_sf(data=edges.predict, aes(color=fitted)) +
  geom_sf(data=DE_ssn$obs, aes(color=Cent.97.5.ave)) +
  scale_color_viridis_c(limits=c(min(m.predict.ci$fitted), max(m.predict.ci$fitted)),
                        option = "H", direction=-1, oob=scales::squish,
                        name="Cent.97.5.ave") +
  theme_bw() + labs(title="Predicted Cent.97.5.ave")

gg2 = ggplot() +
  geom_sf(data=edges.predict, aes(color=fitted.cl)) +
  geom_sf(data=DE_ssn$obs, aes(color=Cent.97.5.ave)) +
  scale_color_viridis_c(limits=c(min(m.predict.ci$fitted.cl), max(m.predict.ci$fitted.cl)),
                        option = "H", direction=-1, oob=scales::squish,
                        name="Cent.97.5.ave") +
  theme_bw() + labs(title="95% Lower CI")

gg3 = ggplot() +
  geom_sf(data=edges.predict, aes(color=fitted.cu)) +
  geom_sf(data=DE_ssn$obs, aes(color=Cent.97.5.ave)) +
  scale_color_viridis_c(limits=c(min(m.predict.ci$fitted.cu), max(m.predict.ci$fitted.cu)),
                        option = "H", direction=-1, oob=scales::squish,
                        name="Cent.97.5.ave") +
  theme_bw() + labs(title="95% Upper CI")

grid.arrange(gg1, gg2, gg3, nrow=1)

gg4 = ggplot() +
  geom_sf(data=edges.predict, aes(color=difference)) +
  scale_color_viridis_c(limits=c(min(m.predict.ci$difference), max(m.predict.ci$difference)),
                        option = "H", direction=-1, oob=scales::squish,
                        name="Cent.97.5.ave") +
  theme_bw() + labs(title="CI Range")

# log confidence intervals
gg1 = ggplot() +
  geom_sf(data=edges.predict, aes(color=log(fitted))) +
  geom_sf(data=DE_ssn$obs, aes(color=log(Cent.97.5.ave))) +
  scale_color_viridis_c(limits=c(min(log(m.predict.ci$fitted)), max(log(m.predict.ci$fitted))),
                        option = "H", direction=-1, oob=scales::squish,
                        name="log Cent.97.5.ave") +
  theme_bw() + labs(title="Predicted Cent.97.5.ave")

gg2 = ggplot() +
  geom_sf(data=edges.predict, aes(color=log(fitted.cl))) +
  geom_sf(data=DE_ssn$obs, aes(color=log(Cent.97.5.ave))) +
  scale_color_viridis_c(limits=c(min(log(m.predict.ci$fitted.cl)), max(log(m.predict.ci$fitted.cl))),
                        option = "H", direction=-1, oob=scales::squish,
                        name="log Cent.97.5.ave") +
  theme_bw() + labs(title="95% Lower CI")

gg3 = ggplot() +
  geom_sf(data=edges.predict, aes(color=log(fitted.cu))) +
  geom_sf(data=DE_ssn$obs, aes(color=log(Cent.97.5.ave))) +
  scale_color_viridis_c(limits=c(min(log(m.predict.ci$fitted.cu)), max(log(m.predict.ci$fitted.cu))),
                        option = "H", direction=-1, oob=scales::squish,
                        name="log Cent.97.5.ave") +
  theme_bw() + labs(title="95% Upper CI")

grid.arrange(gg1, gg2, gg3, nrow=1)
################################################################################
# visualize spatial weights



ggplot(DE_ssn$edges) +
  geom_sf(aes(color=upDist)) +
  scale_color_viridis_c( direction=-1)
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
    formula = Cent.97.5.ave ~ upDist,
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

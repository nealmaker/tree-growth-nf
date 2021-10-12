library("tidyverse")
library("extrafont")
library("gridExtra")
library("lattice")
library(cmocean)

# export lat lon ale data to use in qgis ---------------------------------------
write.csv(ale_latlon_c, file = "ale_latlon_c.csv")


# spp / dbh / cr PDP -----------------------------------------------------------
pdp_spp_dbh_cr2 <- pdp_spp_dbh_cr

#change spp names for manuscript revisions
# pdp_spp_dbh_cr2$spp <- 
#   fct_recode(pdp_spp_dbh_cr2$spp, 
#              "Hophornbeam" = "hophornbeam",
#              "Hard Maple" = "hard maple",
#              "Eastern White Pine" = "white pine")

wireframe3way <- function(spec){
  pdp_spp_dbh_cr2 %>% 
    filter(spp == spec) %>% 
    wireframe(x = yhat~dbh*cr, data = ., drape = T,
              col = "gray20", col.regions = cmocean("speed")(100),
              xlab = "DBH", ylab = "CR", 
              zlab = list("\u0394 DBH", rot = 90),
              par.box = list(col = NA), main = paste0(spec),
              colorkey = F, zlim = c(min(pdp_spp_dbh_cr2$yhat),
                                     max(pdp_spp_dbh_cr2$yhat)),
              at = seq(from = min(pdp_spp_dbh_cr2$yhat),
                       to = max(pdp_spp_dbh_cr2$yhat),
                       length.out = 100))
}

species <- as.character(unique(pdp_spp_dbh_cr2$spp))
pdp_spp_dbh_cr2 <- as_tibble(pdp_spp_dbh_cr2)
plots3way <- lapply(species, wireframe3way)

# hophornbeam, hard maple, and white pine
grid.arrange(plots3way[[14]], plots3way[[11]], plots3way[[27]],nrow = 1)


# hemlock bal / longitude PDP --------------------------------------------------
wireframe(.value ~ lon*bal_hemlock, data = pdp_bal_hemlock_lon$results,
          xlab = list("longitude", rot = -10), 
          ylab = list("hemlock BAL", rot = 70), 
          zlab = list("\u0394 DBH", rot = 90),
          drape = TRUE, col.regions = cmocean("speed")(100),
          colorkey = T, par.box = list(col = NA),
          screen = list(z = 340, x = -60)
)


# ba fir / species PDP ---------------------------------------------------------
pdp_ba_fir_spp2 <- pdp_ba_fir_spp$clone()
pdp_ba_fir_spp2 <- pdp_ba_fir_spp2$results %>% 
  filter(spp != "yellow birch", spp != "aspen", spp != "hemlock") %>% 
  mutate(.value = .value * 2.54,
         ba_fir = ba_fir * (.0929 / .404687)) 

pdp_ba_fir_spp$results %>% 
  mutate(.value = .value * 2.54,
         ba_fir = ba_fir * (.0929 / .404687)) %>%  # to metric
  # filter(spp %in% c("fir", "spruce", "soft maple", "hard maple", "cedar",
  #                   "beech", "yellow birch", "paper birch", "hemlock",
  #                   "white pine", "aspen", "ash")) %>%
  filter(spp %in% c("yellow birch", "aspen", "hemlock")) %>%
  mutate(spp = reorder(spp, .value, FUN = mean)) %>% 
  ggplot(aes(ba_fir, .value, linetype = spp)) +
  # geom_ribbon(aes(ymin = 0, ymax = .value), 
  #             fill="#154729", color="#154729", alpha = .3) +
  # geom_point(col = "#154729", size = 1) +
  geom_smooth(data = pdp_ba_fir_spp2, mapping = aes(ba_fir, .value, group = spp), 
              col = "gray70", size = .2, span = .2, linetype = "solid", se = F) +
  geom_smooth(col = "#154729", size = 1, span = .2, se = F) +
  # facet_wrap(~ spp) +
  scale_y_continuous(name = expression(paste(Delta, "DBH (cm)"))) +
  scale_x_continuous(name = expression(paste("Fir BA ", (m^{2} %.% ha^{-1})))) +
  labs(linetype = "Species") +
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8),
        legend.text = element_text(size = 8))

ggsave("baFir-spp-pdp.pdf",
       device = "pdf", width = 90, height = 80, units = "mm", dpi = 1000)


# feature errors plots ---------------------------------------------------------
df_temp <- rbind(data.frame(model = "nonlinear least squares", dbh = test$dbh, 
                            cr = test$cr, ba = test$ba, bal = test$bal,
                            error = (test$y - test$yhat_nls), y = test$y),
                 data.frame(model = "gradient boosting machine", dbh = test$dbh, 
                            cr = test$cr, ba = test$ba, bal = test$bal,
                            error = (test$y - test$yhat_gbm), y = test$y)) %>% 
  mutate(dbh = dbh * 2.54, # to metric
         error = error * 2.54,
         y = y * 2.54,
         ba = ba * (.0929 / .404687),
         bal = bal * (.0929 / .404687)) 
ylab_temp <- "error (cm)" #expression(paste("residual ", (cm %.% yr^{-1})))

# DBH
xlab_temp <- "DBH (cm)"
perr1 <- ggplot() +
  geom_hline(yintercept = 0, col = "black", size = .4, linetype = "dotted") +
  #geom_vline(xintercept = mean(test$dbh), col = "grey", size = 2) +
  geom_smooth(data = df_temp, size = .9,
              mapping = aes(dbh, error, linetype = model), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 6),
              col = "#154729") +  
  geom_rug(data = train[sample(1:nrow(train), 20000), ],
           mapping = aes(dbh * 2.54), sides = "b",
           col = "black", size = .2) +
  scale_y_continuous(name = ylab_temp) +
  scale_x_continuous(name = xlab_temp) +
  coord_cartesian(xlim = c(0, 80),
                  ylim = c(-.35, 1)) +
  theme(legend.position="none", 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8)) 

# CR
xlab_temp <- "CR (%)"
perr2 <- ggplot() +
  geom_hline(yintercept = 0, col = "black", size = .4, linetype = "dotted") +
  #geom_vline(xintercept = mean(test$dbh), col = "grey", size = 2) +
  geom_smooth(data = df_temp, size = .9,
              mapping = aes(cr, error, linetype = model), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 6),
              col = "#154729") +  
  geom_rug(data = train[sample(1:nrow(train), 20000), ],
           mapping = aes(cr), sides = "b",
           col = "black", size = .2) +
  scale_y_continuous(name = ylab_temp) +
  scale_x_continuous(name = xlab_temp) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8))

# BA
xlab_temp <- expression(paste("BA ", (m^{2} %.% ha^{-1})))
perr3 <- ggplot() +
  geom_hline(yintercept = 0, col = "black", size = .4, linetype = "dotted") +
  #geom_vline(xintercept = mean(test$dbh), col = "grey", size = 2) +
  geom_smooth(data = df_temp, size = .9,
              mapping = aes(ba, error, linetype = model), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 6),
              col = "#154729") +  
  geom_rug(data = train[sample(1:nrow(train), 20000), ],
           mapping = aes(ba * (.0929 / .404687)), sides = "b", # to metric
           col = "black", size = .2) +
  scale_y_continuous(name = ylab_temp) +
  scale_x_continuous(name = xlab_temp) +
  coord_cartesian(xlim = c(0, 90),
                  ylim = c(-.08, .6)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8))

# BAL
xlab_temp <- expression(paste("BAL ", (m^{2} %.% ha^{-1})))
perr4 <- ggplot() +
  geom_hline(yintercept = 0, col = "black", size = .4, linetype = "dotted") +
  #geom_vline(xintercept = mean(test$dbh), col = "grey", size = 2) +
  geom_smooth(data = df_temp, size = .9,
              mapping = aes(bal, error, linetype = model), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 6),
              col = "#154729") +  
  geom_rug(data = train[sample(1:nrow(train), 20000), ],
           mapping = aes(bal * (.0929 / .404687)), sides = "b", # to metric
           col = "black", size = .2) +
  scale_y_continuous(name = ylab_temp) +
  scale_x_continuous(name = xlab_temp) +
  coord_cartesian(xlim = c(0, 90),
                  ylim = c(-.3, .6)) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8))

# #extract legend
# #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend<-g_legend(perr1)

resid_pred_plots <- grid.arrange(perr1, perr2, perr3, perr4, nrow = 2)

ggsave("resid-pred-plots.pdf", plot = resid_pred_plots,
       device = "pdf", width = 190, height = 127, units = "mm", dpi = 1000)
  
# response error plot ----------------------------------------------------------
xlab_temp <- expression(paste(Delta, DBH, " (cm)")) #"\u0394 DBH (cm)" #expression(paste("DBH increment ", (cm %.% yr^{-1})))
ggplot() +
  geom_hline(yintercept = 0, col = "black", size = .4, linetype = "dotted") +
  #geom_vline(xintercept = mean(test$dbh), col = "grey", size = 2) +
  geom_smooth(data = df_temp, size = 1,
              mapping = aes(y, error, linetype = model), 
              method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 6),
              col = "#154729") +  
  geom_rug(data = train,
           mapping = aes(y * 2.54), sides = "b",
           col = "black", size = .2) +
  scale_y_continuous(name = ylab_temp) +
  scale_x_continuous(name = xlab_temp) +
  coord_cartesian(xlim = c(0, 9),
                  ylim = c(-1.7, 6.2)) +
  theme(legend.position="none", 
        legend.text = element_text(size = 7),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8)) 

ggsave("resid-response-plot.pdf",
       device = "pdf", width = 90, height = 60, units = "mm", dpi = 1000)

# ba hm ale plot ---------------------------------------------------------------
ggplot() +
  geom_line(data = ale_baHM$results,
            mapping = aes(ba_hard.maple, .value),
            col = "#154729", size = 1) +
  geom_rug(data = train[sample(1:nrow(train), 20000), ], 
           mapping = aes(dbh), sides = "b",
           col = "black", alpha = .4) +
  scale_x_continuous(name = expression("Hard maple BA (m" ^{2})) +
  scale_y_continuous(name = "\u0394 DBH (cm)") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 14)) 


# spp error plots -------------------------------------------------------------
df_temp <- rbind(data.frame(model = "nls", spp = test$spp, error = (test$y - test$yhat_nls), y = test$y),
                 data.frame(model = "gbm", spp = test$spp, error = (test$y - test$yhat_gbm), y = test$y)) %>% 
  mutate(error = error * 2.54) 

df_temp %>% 
  group_by(spp, model) %>% 
  mutate(err_mean = mean(error)) %>% 
  ungroup() %>% 
  mutate(spp = reorder(spp, spp == spp, FUN = function(x) {1/sum(x)})) %>% 
  ggplot(aes(error, linetype = model)) +
  geom_vline(xintercept = 0, linetype = "dotted", size = .5) +
  geom_density(fill = "gray", alpha = .4, color = "#154729", size = .8) +
  xlab("error (cm)") +
  coord_cartesian(xlim = c(-3, 3)) +
  # geom_vline(aes(xintercept = err_mean, linetype = model), col = "gray20", size = 1) +
  facet_wrap(~ spp, ncol = 4) +
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8),
        strip.text.x = element_text(size = 9))

ggsave("spp-err-plots.pdf",
       device = "pdf", width = 190, height = 232, units = "mm", dpi = 500)


# spp error table --------------------------------------------------------------
View(
  test %>% 
    group_by(spp) %>% 
    summarize(errNLS = mean(y) - mean(yhat_nls),
              errGBM = mean(y) - mean(yhat_gbm),
              n = n()) %>% 
    arrange(desc(n))
)



# variable importance plot -----------------------------------------------------
fimp$results[1:25, ] %>% 
  mutate(feature = str_replace(feature, "ba_", "BA "),
         feature = str_replace(feature, "bal_", "BAL "),
         feature = str_replace(feature, "\\.", " "),
         feature = str_replace(feature, "^cr$", "CR"),
         feature = str_replace(feature, "^dbh$", "DBH"),
         feature = str_replace(feature, "^spp$", "species"),
         feature = str_replace(feature, "^lat$", "latitude"),
         feature = str_replace(feature, "^lon$", "longitude"),
         feature = str_replace(feature, "^elev$", "elevation"),
         feature = str_replace(feature, "^crown_class$", "crown class"),
         feature = reorder(feature, importance, FUN = mean)) %>% 
  ggplot() +
  geom_segment(aes(x = 0, y = feature, xend = importance, yend = feature),
                 color = "#154729", size = .5) +
  geom_point(aes(importance, feature), color = "#154729", size = 1.4) +
  scale_x_continuous(name = "Feature Importance", limits = c(0, .02)) +
  scale_y_discrete(name = "") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8),
        axis.text = element_text(size = 7))

ggsave("var-imp-plot.pdf",
       device = "pdf", width = 90, height = 80, units = "mm", dpi = 1000)


# interaction strengths --------------------------------------------------------
interactions$results %>%
  filter(.interaction > .099) %>% 
  mutate(.feature = str_replace(.feature, "ba_", "BA "),
         .feature = str_replace(.feature, "bal_", "BAL "),
         .feature = str_replace(.feature, "\\.", " "),
         .feature = str_replace(.feature, "_", " "),
         .feature = str_replace(.feature, "^cr$", "CR"),
         .feature = str_replace(.feature, "^dbh$", "DBH"),
         .feature = str_replace(.feature, "^spp$", "species"),
         .feature = str_replace(.feature, "^lat$", "latitude"),
         .feature = str_replace(.feature, "^lon$", "longitude"),
         .feature = str_replace(.feature, "^elev$", "elevation"),
         .feature = str_replace(.feature, "^crown_class$", "crown class"),
         .feature = reorder(.feature, .interaction, FUN = mean)) %>% 
  ggplot() +
  geom_segment(aes(x = 0, y = .feature, xend = .interaction, yend = .feature),
               color = "#154729", size = .5) +
  geom_point(aes(.interaction, .feature), color = "#154729", size = 1.4) +
  scale_x_continuous(name = "Interaction Strength") +
  scale_y_discrete(name = "") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 8),
        axis.text = element_text(size = 7))

ggsave("interactions-plot.pdf",
       device = "pdf", width = 90, height = 80, units = "mm", dpi = 1000)


# Pct plots with 3 or more spp -------------------------------------------------
temp <- nf_fia[index, ]
temp[, 45:72] <- ifelse(temp[, 45:72] > 0, 1, 0)
nspp <- rowSums(temp[, 45:72])
diverse <- data.frame(plot = temp$plot, nspp = nspp) %>% group_by(plot) %>% 
  summarize(diverse = mean(nspp) >= 3)
mean(diverse$diverse)


# distribution of y ------------------------------------------------------------
# 80% closest to mean
temp <- (test %>% mutate(wamm = abs(y - mean(test$y))) %>% 
     arrange((wamm)))[1:round(.8*nrow(test)),]
max(temp$y)

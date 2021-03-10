
# using the results of the elastic net
# model fits
# model performance statistics


library(caret)
library(dplyr)
library(knitr)
library(reshape2)


# read in data
res_3T <- readRDS("data/elasticnet_res_3T.RDS")
res_7T <- readRDS("data/elasticnet_res_7T.RDS")
res_3T_xover <- readRDS("data/elasticnet_res_3T_xover.RDS")
res_3T_quality <- readRDS("data/elasticnet_res_3T_quality.RDS")

ct_list_3T <- readRDS("data/ct_list_3T.RDS")
ct_list_7T <- readRDS("data/ct_list_7T.RDS")
sa_list_3T <- readRDS("data/sa_list_3T.RDS")
sa_list_7T <- readRDS("data/sa_list_7T.RDS")
ct_list_3T_xover <- readRDS("data/ct_list_3T_xover.RDS")
sa_list_3T_xover <- readRDS("data/sa_list_3T_xover.RDS")
ct_list_3T_quality <- readRDS("data/ct_list_3T_quality.RDS")
sa_list_3T_quality <- readRDS("data/sa_list_3T_quality.RDS")

NETWORK_NAMES <- c('Auditory',
                   'DMN',
                   'ECN_L',
                   'ECN_R',
                   'Hippocampal',
                   'Language',
                   'Salience',
                   'Sensorimotor',
                   'Visual_lateral',
                   'Visual_medial',
                   'Visual_occipital')

num_networks <- length(NETWORK_NAMES)

#
fit_stats <- function(ct_dat,
                      sa_dat,
                      res) {

  ct_status <- rep(1, nrow(ct_dat))
  sa_status <- rep(2, nrow(sa_dat))
  obs_status <- c(ct_status, sa_status)

  # combine controls and superagers
  dat <-
    rbind(ct_dat, sa_dat) %>%
    select(-id) %>%
    as.matrix()

  pred <- predict(res, dat)

  pred.glmnet <-
    predict(res$finalModel,
            newx = dat,
            type = "response",
            s = res$bestTune$lambda)
            # s = "lambda.min")

  ppred <- predict(res, dat, type = "prob")

  # prediction performance against data
  stats <-
    data.frame(
      RMSE = RMSE(as.numeric(pred),
                  obs_status),
      Rsquare = R2(as.numeric(pred),
                   obs_status))

  list(pred = pred,
       pred.glmnet = pred.glmnet,
       ppred = ppred,
       stats = stats,
       obs_status = obs_status)
}


############
# analysis #
############

stat_res <- list()
for (i in seq_len(num_networks)) {

  stat_res[["3T"]][[i]] <-
    fit_stats(ct_list_3T[[i]],
              sa_list_3T[[i]],
              res_3T[[i]])

  stat_res[["7T"]][[i]] <-
    fit_stats(ct_list_7T[[i]],
              sa_list_7T[[i]],
              res_7T[[i]])

  stat_res[["3T_xover"]][[i]] <-
    fit_stats(ct_list_3T_xover[[i]],
              sa_list_3T_xover[[i]],
              res_3T_xover[[i]])

  stat_res[["3T_quality"]][[i]] <-
    fit_stats(ct_list_3T_quality[[i]],
              sa_list_3T_quality[[i]],
              res_3T_quality[[i]])
}

names(stat_res[["3T"]]) <- NETWORK_NAMES
names(stat_res[["7T"]]) <- NETWORK_NAMES
names(stat_res[["3T_xover"]]) <- NETWORK_NAMES
names(stat_res[["3T_quality"]]) <- NETWORK_NAMES

save(stat_res, file = "data/predict_output.RData")


# # compare directly
# models <- list(res_3T = res_3T,
#                res_7T = res_7T)
#
# resamples(models) %>%
#   summary(metric = "RMSE")


#########
# plots #
#########

# x11()
png("output/rgn_scatterplot_3T.png")
par(mfrow = c(3,4))
for (i in 1:11) {
  plot(stat_res$`3T`[[i]]$obs_status,
       stat_res$`3T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`3T`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T`[[i]][[2]][, "1"] ~ stat_res$`3T`[[i]]$obs_status))
}
dev.off()

# x11()
png("output/rgn_scatterplot_7T.png")
par(mfrow = c(3,4))
for (i in 1:11) {
  plot(stat_res$`7T`[[i]]$obs_status,
       stat_res$`7T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`7T`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`7T`[[i]][[2]][, "1"] ~ stat_res$`7T`[[i]]$obs_status))
}
dev.off()

# x11()
png("output/rgn_scatterplot_3T_xover.png")
par(mfrow = c(3,4))
for (i in 1:11) {
  plot(stat_res$`3T_xover`[[i]]$obs_status,
       stat_res$`3T_xover`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`3T_xover`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T_xover`[[i]][[2]][, "1"] ~ stat_res$`3T_xover`[[i]]$obs_status))
}
dev.off()

# x11()
png("output/rgn_scatterplot_3T_quality.png")
par(mfrow = c(3,4))
for (i in 1:11) {
  plot(stat_res$`3T_quality`[[i]]$obs_status,
       stat_res$`3T_quality`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`3T_quality`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T_quality`[[i]][[2]][, "1"] ~ stat_res$`3T_quality`[[i]]$obs_status))
}
dev.off()

# single plot for 3T and 7T
# png("output/rgn_scatterplot_3T_7T.png")
# tiff("output/rgn_scatterplot_3T_7T.tiff", res=300,  width = 8, height = 8, units = 'in')
pdf("output/rgn_scatterplot_3T_7T.pdf")
par(mfrow = c(3,4))
for (i in 1:11) {
  plot(stat_res$`3T`[[i]]$obs_status,
       stat_res$`3T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = gsub("_", " ", names(stat_res$`3T`)[i]),
       xlab = "Observed status",
       ylab = "Model probablity superager",
       col = "blue")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T`[[i]][[2]][, "1"] ~ stat_res$`3T`[[i]]$obs_status), col = "blue")

  points(stat_res$`7T`[[i]]$obs_status,
       stat_res$`7T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       xlab = "Observed status",
       ylab = "Model probablity superager",
       col = "red")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`7T`[[i]][[2]][, "1"] ~ stat_res$`7T`[[i]]$obs_status), col = "red")
}
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", bty = "n",
       legend = c("3T", "7T"),
       col = c("blue", "red"), lwd=2, cex=1, horiz = FALSE)
dev.off()



## binned residuals?
##TODO:


# contingency tables

purrr::map(stat_res$`3T`, ~confusionMatrix(.x$pred,
                as.factor(.x$obs_status - 1),
                dnn = c("pred", "obs"))$table %>% kable())
purrr::map(stat_res$`7T`, ~confusionMatrix(.x$pred,
                as.factor(.x$obs_status - 1),
                dnn = c("pred", "obs"))$table %>% kable())


##TODO: combined test as AND or OR



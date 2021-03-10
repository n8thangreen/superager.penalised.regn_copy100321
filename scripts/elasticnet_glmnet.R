
# superager lasso regression
# N Green
#
# https://daviddalpiaz.github.io/r4sl/regularization.html


library(glmnet)
library(dplyr)


ctr_original <-
  data.frame(readRDS(here::here("../../raw_data/011220_Demetrius/ctr_original.rds")))
sa_original <-
  data.frame(readRDS(here::here("../../raw_data/011220_Demetrius/sa_original.rds")))

#'
glmnet_superagers <- function(i = 1,
                              ctr_original,
                              sa_original,
                              start_cols =
                                seq(1, ncol(ctr_original),
                                    by = num_in_network),
                              rm_cols = c("Machine",
                                          "Type",
                                          "Subject",
                                          "Gender",
                                          "Age",
                                          "Date_GraphICA",
                                          "Manufacturer",
                                          "Model"),
                              num_in_network = 832) {
  ctr_original <-
    select(ctr_original, !rm_cols)

  sa_original <-
    select(sa_original, !rm_cols)

  controls <- cbind(status = 0,
                    data.matrix(ctr_original[, start_cols[i] + 1:num_in_network - 1]))

  superagers <- cbind(status = 1,
                      data.matrix(sa_original[, start_cols[i] + 1:num_in_network - 1]))

  dat <- rbind(controls, superagers)

  X <- dat[, -1]
  y <- as.factor(dat[, "status"])

  # cross-validation to select lambda
  # lasso
  fit_cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 10)
  # ridge regression
  fit_cv_rr <- cv.glmnet(X, y, family = "binomial", alpha = 0, nfolds = 10)

  list(fit_cv_lasso = fit_cv_lasso,
       fit_1se_lasso =
         glmnet(X, y, family = "binomial", alpha = 1, lambda = fit_cv_lasso$lambda.1se),
       glmnet_bin_lasso =
         glmnet(X, y, family = "binomial", alpha = 1),
       fit_cv_rr = fit_cv_rr,
       fit_1se_rr =
         glmnet(X, y, family = "binomial", alpha = 0, lambda = fit_cv_rr$lambda.1se),
       glmnet_bin_rr =
         glmnet(X, y, family = "binomial", alpha = 0))
}


############
# analysis #
############

res <-
  purrr::map(1:11,
             ~glmnet_superagers(.x,
                                ctr_original = ctr_original,
                                sa_original = sa_original))

# lasso --

# x11()
# par(mfrow = c(4,3))
for (i in seq_along(res)) {
  plot(res[[i]]$fit_cv_lasso)
}

# coef(res[[1]]$fit_cv)

res2 <- purrr::transpose(res)

purrr::map(res2$fit_1se_lasso,
           function(x) which(as.vector(as.matrix(x$beta)) != 0))

for (i in seq_along(res)) {
  # x11()
  par(mfrow = c(1, 2))
  plot(res2$glmnet_bin_lasso[[i]])
  plot(res2$glmnet_bin_lasso[[i]], xvar = "lambda")
}

# res$fit_cv_lasso$lambda.min
# res$fit_cv_lasso$lambda.1se


# ridge regression --

# x11()
# par(mfrow = c(4,3))
for (i in seq_along(res)) {
  plot(res[[i]]$fit_cv_rr)
}

# coef(res[[1]]$fit_cv)

res2 <- purrr::transpose(res)

for (i in seq_along(res)) {
  # x11()
  par(mfrow = c(1, 2))
  plot(res2$glmnet_bin_rr[[i]])
  plot(res2$glmnet_bin_rr[[i]], xvar = "lambda")
}

# res$fit_cv_lasso$lambda.min
# res$fit_cv_lasso$lambda.1se



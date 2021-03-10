
#' elasticnet_superagers
#'
#' @param i network id
#' @export
#' @import caret glmnet
#' @return the result of running train()
#'
elasticnet_superagers <- function(i = 1,
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

  keep_cols <- start_cols[i] + 1:num_in_network - 1

  # subset columns for single network
  controls <-
    cbind(status = 0, data.matrix(ctr_original[, keep_cols]))

  superagers <-
    cbind(status = 1, data.matrix(sa_original[, keep_cols]))

  dat <- rbind(controls, superagers)

  dat <- as.data.frame(dat)
  dat[, "status"] <- as.factor(dat[, "status"])

  # fit model
  model <- train(
    status ~ .,
    data = dat,
    method = "glmnet",
    trControl = trainControl("cv",
                             number = 10), # folds
    tuneLength = 10    # number of alpha, lambda to try
  )

  model
}



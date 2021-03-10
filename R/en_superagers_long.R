
#' en_superagers_long
#'
#' use (tidy) long data format as input
#'
#' @param i network id
#' @export
#' @import caret glmnet
#' @return the result of running train()
#' @export
#'
en_superagers_long <- function(i = 1,
                               ct_long,
                               sa_long) {

  # subset columns for single network
  controls <-
    ct_long %>%
    filter(network == i) %>%
    arrange(region) %>%
    dcast(id ~ region, value.var = "value") %>%
    mutate(status = 0) %>%
    select(-id)

  superagers <-
    sa_long %>%
    filter(network == i) %>%
    arrange(region) %>%
    dcast(id ~ region, value.var = "value") %>%
    mutate(status = 1) %>%
    select(-id)

  dat <-
    rbind(controls, superagers) %>%
    as.data.frame() %>%
    mutate(status = as.factor(status))

  # fit model
  model <- train(
    status ~ .,
    data = dat,
    method = "glmnet",
    # tuneGrid = expand.grid(alpha = c(0, 0.5, 1),
    #                       lambda = seq(0.0001, 1, length = 10)),
    trControl = trainControl(method = "cv",
                             selectionFunction = "oneSE",
                             number = 10), # folds
    tuneLength = 10    # number of alpha, lambda to try
  )

  model
}



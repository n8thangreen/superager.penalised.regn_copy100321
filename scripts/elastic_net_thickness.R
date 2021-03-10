
# superagers analysis
# elastic net regression
# with thickness data


library(caret)
library(dplyr)
library(reshape2)


set.seed(123)

ctr_original <-
  data.frame(readRDS(
    here::here("../../raw_data/081120_Demetrius/ctr_t1.rds")))
sa_original <-
  data.frame(readRDS(
    here::here("../../raw_data/081120_Demetrius/sa_t1.rds")))


res <- elasticnet_superagers(ctr_original = ctr_original,
                             sa_original = sa_original)

res <- list(res)
# best tuning parameter
bestTune <- purrr::map(res, ~.$bestTune)

finalModel <-
  purrr::map(res, function(x) coef(x$finalModel,
                                   x$bestTune$lambda))

coef_ids <- purrr::map(finalModel, ~.@i)
coef_vals <- purrr::map(finalModel, ~.@x)

exp_coef_vals <- purrr::map(coef_vals, exp)

melt_id <- melt(coef_ids)
melt_or <- melt(exp_coef_vals)

out <-
  cbind(melt_id,
        or = melt_or[, 1]) %>%
  rename(network = L1) %>%
  mutate(or = round(or, 4))

xx <- dcast(out, network~value, value.var = "or")
xx[is.na(xx)] <- ""

## replace number id with named label

position <-
  readr::read_csv("~/Jasmina UCL/superagers/raw_data/position.csv")

labels <- position$Label
names(labels) <- 1:832

is_id <- names(xx) %in% as.character(1:832)
ids <- names(xx)[is_id]

names(xx)[is_id] <- labels[ids]

write.csv(xx, file = "data/elasticnet_thickness_table.csv")


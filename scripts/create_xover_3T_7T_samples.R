
# create same patient samples
# for both 3T and 7T
# for cross over analysis


ct_dat_3T <-
  readRDS(here::here("../../raw_data/190121/data/crt_3T_zv.rds"))

sa_dat_3T <-
  readRDS(here::here("../../raw_data/190121/data/sa_3T_zv.rds"))

ct_dat_7T <-
  readRDS(here::here("../../raw_data/190121/data/crt_7T_zv.rds"))

sa_dat_7T <-
  readRDS(here::here("../../raw_data/190121/data/sa_7T_zv.rds"))

ct_dat_3T$Subject <- gsub("_3T", "", ct_dat_3T$Subject)
ct_dat_7T$Subject <- gsub("_7T", "", ct_dat_7T$Subject)
sa_dat_3T$Subject <- gsub("_3T", "", sa_dat_3T$Subject)
sa_dat_7T$Subject <- gsub("_7T", "", sa_dat_7T$Subject)

# remove patients only in 3T

ct_dat_3T_xover <-
  merge(ct_dat_3T,
        ct_dat_7T[, "Subject", drop = FALSE],
        by = "Subject",
        all = FALSE)

sa_dat_3T_xover <-
  merge(sa_dat_3T,
        sa_dat_7T[, "Subject", drop = FALSE],
        by = "Subject",
        all = FALSE)

saveRDS(ct_dat_3T_xover,
        here::here("../../raw_data/190121/data/crt_3T_xover_zv.rds"))
saveRDS(sa_dat_3T_xover,
        here::here("../../raw_data/190121/data/sa_3T_xover_zv.rds"))

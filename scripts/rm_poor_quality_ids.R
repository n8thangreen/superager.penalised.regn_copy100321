
# removing poor quality 3T
#


ct_dat <-
  readRDS(here::here(glue("../../raw_data/190121/data/crt_3T_zv.rds")))
sa_dat <-
  readRDS(here::here(glue("../../raw_data/190121/data/sa_3T_zv.rds")))

# remove columns
ct_dat <-
  ct_dat[, !names(ct_dat) %in% c(
    "Machine",
    "Type",
    "Gender",
    "Age",
    "Date_GraphICA",
    "Manufacturer",
    "Model")]
sa_dat <-
  sa_dat[, !names(sa_dat) %in% c(
    "Machine",
    "Type",
    "Gender",
    "Age",
    "Date_GraphICA",
    "Manufacturer",
    "Model")]

ct_subjects_to_rm <-
  c("CT_-1_3T", "CT_36_3T", "CT_68_3T", "CT_71_3T", "CT_72_3T", "CT_74_3T")

ct_dat2 <-
  ct_dat %>%
  mutate(Subject = as.character(Subject)) %>%
  dplyr::filter(!Subject %in% ct_subjects_to_rm)

sa_subjects_to_rm <-
  c("SA_24_3T", "SA_75_3T")

sa_dat2 <-
  sa_dat %>%
  mutate(Subject = as.character(Subject)) %>%
  dplyr::filter(!Subject %in% sa_subjects_to_rm)

saveRDS(ct_dat2, file = here::here("../../raw_data/190121/data/crt_3T_quality_zv.rds"))
saveRDS(sa_dat2, file = here::here("../../raw_data/190121/data/sa_3T_quality_zv.rds"))


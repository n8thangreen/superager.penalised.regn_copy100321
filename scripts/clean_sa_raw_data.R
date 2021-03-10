
# include missing patients
#
# need to do this before filtering
# for other things


sa_dat <-
  readRDS(here::here("../../raw_data/190121/data/sa_3T_zv.rds"))

if (all(sa_dat$Subject != "SA_26_3T")) {

  # missing patient
  missing_SA_3T <-
    readRDS(here::here("../../raw_data/040321/Re__Next_Superager_meeting_(05_03)/Data_zv.rds")) %>%
    .[.$Subject == "ID_26_3T", ]

  missing_SA_3T[, "Subject"] <- as.character(missing_SA_3T[, "Subject"])
  missing_SA_3T[1, "Subject"] <- "SA_26_3T"

  sa_dat <- rbind(sa_dat,
                  missing_SA_3T)

}

sa_dat$Subject <- gsub("_CORRECT", "", sa_dat$Subject)

saveRDS(sa_dat, file = here::here("../../raw_data/190121/data/sa_3T_zv.rds"))


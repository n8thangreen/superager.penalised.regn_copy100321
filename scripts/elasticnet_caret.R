
# elastic net regression ----
# main script
# N Green

# www.sthda.com/english/articles/37-model-selection-essentials-in-r/153-penalized-regression-essentials-ridge-lasso-elastic-net/#elastic-net


library(caret)
library(dplyr)
library(reshape2)
library(tidyr)
library(glue)

set.seed(5234)
# set.seed(123)

# TENSOR <- "3T"
# TENSOR <- "7T"
# TENSOR <- "3T_xover"
TENSOR <- "3T_quality"


########
# prep #
########

ct_dat <-
  readRDS(here::here(glue("../../raw_data/190121/data/crt_{TENSOR}_zv.rds")))
sa_dat <-
  readRDS(here::here(glue("../../raw_data/190121/data/sa_{TENSOR}_zv.rds")))

# remove columns
ct_dat <-
  ct_dat[, !names(ct_dat) %in% c(
    "Machine",
    "Type",
    "Subject",
    "Gender",
    "Age",
    "Date_GraphICA",
    "Manufacturer",
    "Model")]
sa_dat <-
  sa_dat[, !names(sa_dat) %in% c(
    "Machine",
    "Type",
    "Subject",
    "Gender",
    "Age",
    "Date_GraphICA",
    "Manufacturer",
    "Model")]

ct_long <-
  ct_dat %>%
  mutate(id = 1:n()) %>%
  melt(id.vars = "id") %>%
  separate(variable, c("region", "network")) %>%
  mutate(region = as.numeric(region),
         network = as.numeric(network) + 1,
         network = ifelse(is.na(network), 1, network))

sa_long <-
  sa_dat %>%
  mutate(id = 1:n()) %>%
  melt(id.vars = "id") %>%
  separate(variable, c("region", "network")) %>%
  mutate(region = as.numeric(region),
         network = as.numeric(network) + 1,
         network = ifelse(is.na(network), 1, network))

ct_list <-
  ct_long %>%
  split(ct_long$network) %>%
  purrr::map(~dcast(.x,
                    formula = id ~ region,
                    value.var = "value"))
sa_list <-
  sa_long %>%
  split(sa_long$network) %>%
  purrr::map(~dcast(.x,
                    formula = id ~ region,
                    value.var = "value"))

saveRDS(ct_list, file = glue("data/ct_list_{TENSOR}.RDS"))
saveRDS(sa_list, file = glue("data/sa_list_{TENSOR}.RDS"))

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
n_networks <- length(NETWORK_NAMES)


############
# analysis #
############

res <-
  purrr::map(1:n_networks,
             ~en_superagers_long(.x, ct_long, sa_long))

saveRDS(res, file = glue("data/elasticnet_res_{TENSOR}.RDS"))


print(res[[1]])
plot(res[[1]])

# best tuning parameter
bestTune <- purrr::map(res, ~.$bestTune)

finalModel <-
  purrr::map(res,
             function(x) coef(x$finalModel,
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

or_table <- dcast(out,
                  network ~ value,
                  value.var = "or")

or_table[is.na(or_table)] <- ""

## replace number id with named label

regions <-
  readr::read_csv(here::here("../../raw_data/position.csv"))

n_regions <- nrow(regions)
labels <- regions$Label
names(labels) <- regions$Region

# region columns
is_id <- names(or_table) %in% as.character(1:n_regions)
ids <- names(or_table)[is_id]

names(or_table)[is_id] <- labels[ids]

or_table$network_name <- NETWORK_NAMES

or_table <- relocate(or_table, network_name)

write.csv(or_table, file = glue("data/elastic_net_{TENSOR}_table.csv"))


#########
# plots #
#########

list_nodes_network_male <-
  readRDS("~/Jasmina UCL/superagers/raw_data/list_nodes_network_male.rds")

## L1 plots

png(glue("output/L1_plots_{TENSOR}.png"))
par(mfrow = c(4,3))
purrr::map(res, ~plot(.x$finalModel))
dev.off()

## bar plots by network

or_long <-
  or_table[, !names(or_table) %in% c("network", "0")] %>%
  melt(id.vars = "network_name") %>%
  filter(value != "") %>%
  mutate(value = as.numeric(value),
         network_name = factor(network_name, levels = NETWORK_NAMES))

plot_dat <-
  or_long %>%
  mutate(
    Auditory_mask = variable %in% list_nodes_network_male[[1]],
    DMN_mask = variable %in% list_nodes_network_male[[2]],
    ECN_L_mask = variable %in% list_nodes_network_male[[3]],
    ECN_R_mask = variable %in% list_nodes_network_male[[4]],
    Hippocampal_mask = variable %in% list_nodes_network_male[[5]],
    Language_mask = variable %in% list_nodes_network_male[[6]],
    Salience_mask = variable %in% list_nodes_network_male[[7]],
    Sensorimotor_mask = variable %in% list_nodes_network_male[[8]],
    Visual_lateral_mask = variable %in% list_nodes_network_male[[9]],
    Visual_medial_mask = variable %in% list_nodes_network_male[[10]],
    Visual_occipita_mask = variable %in% list_nodes_network_male[[11]]) %>%
  select(-variable, -value) %>%
  group_by(network_name) %>%
  summarise(across(everything(), ~sum(as.numeric(.x)))) %>%
  melt(id.vars = "network_name",
       variable.name = "mask") %>%
  mutate(network_name =
           factor(network_name, levels = NETWORK_NAMES))

# bar plot

gg <-
  ggplot(plot_dat, aes(network_name, value)) +
  geom_bar(aes(fill = mask), position = "dodge", stat = "identity") +
  coord_flip() +
  theme_bw() +
  ylab("Count") +
  scale_x_discrete("Network name",
                   breaks = factor(NETWORK_NAMES),
                   drop = FALSE) +
  scale_fill_brewer(palette = "Set3")

gg

ggsave(plot = gg, glue("output/barplots_network_{TENSOR}.png"), width = 5)

# tile plot

ggplot(plot_dat, aes(network_name, mask)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_x_discrete("Network name",
                   breaks = factor(NETWORK_NAMES),
                   drop = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(glue("output/tile_plot_{TENSOR}.png"), width = 5)

# grid.arrange(p1, p2, nrow = 1)

library(viridis)
# https://www.r-graph-gallery.com/89-box-and-scatter-plot-with-ggplot2.html
# x11()
ggplot(or_long, aes(x=network_name, y=value)) +
  geom_hline(yintercept=1, linetype="dashed", color = "black") +
  geom_boxplot() +
  scale_x_discrete("Network name",
                   breaks = factor(NETWORK_NAMES),
                   drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="blue", size=0.4, alpha=0.9) +
  ylab("OR") +
  ylim(0.95, 1.05)

ggsave(glue("output/boxplot_plot_{TENSOR}.png"), width = 5)


##########
# tables #
##########

summary_tab <-
  or_long %>%
  mutate(value = ifelse(value == "", NA, value),
         value = as.numeric(value)) %>%
  group_by(network_name) %>%
  summarise(OR = mean(value, na.rm = TRUE),
            L95 = quantile(value, probs = 0.025, na.rm = TRUE),
            U95 = quantile(value, probs = 0.975, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            count = sum(!is.na(value)),
            prop = count/n())
summary_tab

write.csv(summary_tab, file = glue("data/summary_table_{TENSOR}.csv"))


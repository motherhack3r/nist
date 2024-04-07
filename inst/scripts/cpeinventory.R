library(dplyr)
library(tidyr)

# PARAMETERS
seed <- 42

# INIT
set.seed(seed)

# LOAD INPUT DATA
df_cpes <- readRDS("data-raw/df_cpes_extra.rds")
df_raw_inventory <- nist::getInventory()
write.csv(df_raw_inventory, "data-raw/winventory_cased.csv", fileEncoding = "UTF-8")
df_inventory <- df_raw_inventory
df_inventory$software <- df_inventory$title
df_inventory$title <- tolower(df_inventory$title)
write.csv(df_inventory, "data-raw/winventory.csv", fileEncoding = "UTF-8")

# RUN INFERENCE WITH NER MODELS
# - output: ner_inventory.csv
df_ner_out <- read.csv("data-raw/ner_inventory.csv")
names(df_ner_out)[1] <- "id"
df_ner_out$id <- df_ner_out$id + 1
df_ner <- nist::ner_to_inventory(df_ner_out, df_cpes)

df_final <- nist::clean_invetory_cpe(df_inventory, df_ner)






#
# ###############
# df_ner_vers <- df_ner[ , stringr::str_detect(names(df_ner), "(version|id)")]
# df_chk_vpv <- df_ner_vers[df_ner_vers$ner_version_vpv == "", c("id","ner_version_raw_vpv")]
# df_chk_vv <- df_ner_vers[df_ner_vers$ner_version_vv == "", c("id","ner_version_raw_vv")]
# df_chk_pv <- df_ner_vers[df_ner_vers$ner_version_pv == "", c("id","ner_version_raw_pv")]
# df_chk_vers <- df_ner_vers[df_ner_vers$ner_version_vers == "", c("id","ner_version_raw_vers")]
#
# df_chk <- df_inventory %>%
#   left_join(df_chk_vpv, by = "id") %>%
#   left_join(df_chk_vv, by = "id") %>%
#   left_join(df_chk_pv, by = "id") %>%
#   left_join(df_chk_vers, by = "id")
#
#
#
#
#
# df_chk <- df_ner %>%
#   select("id", "cpe_vendor", "cpe_product") %>%
#   inner_join(df_chk_vv, by = "id")
#
#
# ###############
#
# df_train <- readr::read_csv("data-raw/train_cpener_vpv_500k_wgh42.csv.gz")

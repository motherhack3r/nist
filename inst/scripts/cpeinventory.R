library(dplyr)
library(tidyr)

# PARAMETERS
seed <- 42

# INIT
set.seed(seed)

# LOAD INPUT DATA
df_cpes <- readRDS("data-raw/df_cpes_extra.rds")
df_inventory <- nist::getInventory()
write.csv(df_inventory, "data-raw/winventory.csv", fileEncoding = "UTF-8")

# RUN INFERENCE WITH NER MODELS
text::textrpp_initialize(condaenv = "rgpu", save_profile = TRUE)
dfi_vpv <- nist::predict_cpe(df_inventory, model_name = "Neurona/cpegen_vpv", device = "gpu")
dfi_vp <- nist::predict_cpe(df_inventory, model_name="Neurona/cpegen_vp")
dfi_pv <- nist::predict_cpe(df_inventory, model_name="Neurona/cpegen_pv")
dfi_vv <- nist::predict_cpe(df_inventory, model_name="Neurona/cpegen_vv")
dfi_vend <- nist::predict_cpe(df_inventory, model_name="Neurona/cpegen_vend")
dfi_prod <- nist::predict_cpe(df_inventory, model_name="Neurona/cpegen_prod")
dfi_vers <- nist::predict_cpe(df_inventory, model_name="Neurona/cpegen_vers")

# dfi_vpv <- text::textNER("adobe reader dc 9.5", model="Neurona/cpegen_vpv")
# dfi_vp <- text::textNER("adobe reader dc 9.5", model="Neurona/cpegen_vp")
# dfi_pv <- text::textNER("adobe reader dc 9.5", model="Neurona/cpegen_pv")
# dfi_vv <- text::textNER("adobe reader dc 9.5", model="Neurona/cpegen_vv")
# dfi_vend <- text::textNER("adobe reader dc 9.5", model="Neurona/cpegen_vend")
# dfi_prod <- text::textNER("adobe reader dc 9.5", model="Neurona/cpegen_prod")
# dfi_vers <- text::textNER("adobe reader dc 9.5", model="Neurona/cpegen_vers")

# - output: ner_inventory.csv
df_ner_out <- read.csv("data-raw/ner_inventory.csv")
names(df_ner_out)[1] <- "id"
df_ner_out$id <- df_ner_out$id + 1
df_ner <- nist::ner_to_inventory(df_ner_out, df_cpes)

df_ner <- nist::clean_invetory_cpe(df_inventory, df_ner)

df_final <- df_ner %>%
  separate(col = cpe , sep = ":", extra = "merge",
           into = c("std", "v", "part", "vendor", "product", "version", "tail")) %>%
  select("id", "vendor", "product", "version") %>%
  mutate(cpelite = paste0(":", paste(vendor, product, sep = ":"), ":"))




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

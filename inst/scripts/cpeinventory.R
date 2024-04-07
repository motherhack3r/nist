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
# df_ner_out <- readr::read_csv("data-raw/ner_inventory.csv", show_col_types = FALSE, trim_ws = TRUE)
df_ner_out <- read.csv("data-raw/ner_inventory.csv")
names(df_ner_out)[1] <- "id"
df_ner_out$id <- df_ner_out$id + 1
# df_ner_out$software <- df_ner_out$title
# df_ner_out$title <- tolower(df_ner_out$title)
df_ner <- nist::ner_to_inventory(df_ner_out, df_cpes)

df_chk <- df_inventory %>%
  left_join(df_ner %>% select(-"title"), by = "id")

df_chk_version <- df_chk[df_chk$version == "", ]
df_chk_version <- df_chk_version[df_chk_version$cpe_version_score < 1, ]

col_version <- character()
for (i in 1:nrow(df_chk_version)) {
  if (stringr::str_detect(df_chk_version$cpe_version[i], df_chk_version$cpe_vendor[i])) {
    col_version <- c(col_version, "-")
  } else {
    if (stringr::str_detect(df_chk_version$cpe_version[i], df_chk_version$cpe_product[i])) {
      col_version <- c(col_version, "-")
    } else {
      if (stringr::str_detect(df_chk_version$cpe_version[i], stringr::fixed(tolower(df_chk_version$product[i])))) {
        col_version <- c(col_version, "-")
      } else {
        if (stringr::str_detect(df_chk_version$cpe_version[i], stringr::fixed(tolower(df_chk_version$vendor[i])))) {
          col_version <- c(col_version, "-")
        } else {
          if (stringr::str_detect(tolower(df_chk_version$vendor[i]), stringr::fixed(tolower(df_chk_version$cpe_version[i])))) {
            col_version <- c(col_version, "-")
          } else {
            if (stringr::str_detect(tolower(df_chk_version$product[i]), stringr::fixed(tolower(df_chk_version$cpe_version[i])))) {
              col_version <- c(col_version, "-")
            } else {
              if (stringr::str_replace_all(df_chk_version$title[i],"\\s", "") == df_chk_version$cpe_version[i]) {
                col_version <- c(col_version, "-")
              } else {
                col_version <- c(col_version, df_chk_version$cpe_version[i])
              }
            }
          }
        }
      }
    }
  }
}
df_chk_version$cpe_version <- col_version

df_final <- rbind(df_chk[!(df_chk$id %in% df_chk_version$id), ],
                  df_chk_version) %>%
  arrange("id") %>%
  select("id", "software", "cpe_vendor", "cpe_product", "cpe_version")








###############
df_ner_vers <- df_ner[ , stringr::str_detect(names(df_ner), "(version|id)")]
df_chk_vpv <- df_ner_vers[df_ner_vers$ner_version_vpv == "", c("id","ner_version_raw_vpv")]
df_chk_vv <- df_ner_vers[df_ner_vers$ner_version_vv == "", c("id","ner_version_raw_vv")]
df_chk_pv <- df_ner_vers[df_ner_vers$ner_version_pv == "", c("id","ner_version_raw_pv")]
df_chk_vers <- df_ner_vers[df_ner_vers$ner_version_vers == "", c("id","ner_version_raw_vers")]

df_chk <- df_inventory %>%
  left_join(df_chk_vpv, by = "id") %>%
  left_join(df_chk_vv, by = "id") %>%
  left_join(df_chk_pv, by = "id") %>%
  left_join(df_chk_vers, by = "id")





df_chk <- df_ner %>%
  select("id", "cpe_vendor", "cpe_product") %>%
  inner_join(df_chk_vv, by = "id")


###############

df_train <- readr::read_csv("data-raw/train_cpener_vpv_500k_wgh42.csv.gz")

library(nist)
library(dplyr)

seed <- 42
num_top <- 2000
num_samples <- 20000

set.seed(seed)

# Download latest official CPE dataset
tini <- Sys.time()
print(paste0("[", tini,"] Get CPE data "))
cpes_file <- getLatestdata()
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

tini <- Sys.time()
print(paste0("[", tini,"] Parse XML "))
df_cpes <- parseCPExml(cpes_file)
saveRDS(df_cpes, "data-raw/df_cpes.rds")
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

df_cpes <- readRDS("data-raw/df_cpes.rds")

tini <- Sys.time()
print(paste0("[", tini,"] Expanse with CPE components "))
df_cpes_ner <- cpe2wfn(df_cpes)
saveRDS(df_cpes_ner, "data-raw/df_cpes_ner.rds")
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

df_cpes_ner <- readRDS("data-raw/df_cpes_ner.rds")

tini <- Sys.time()
print(paste0("[", tini,"] Add NER annotation (VPV)"))
df_cpe_tag_vpv <- cpeNERannotate(cpes = df_cpes_ner)
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

tini <- Sys.time()
print(paste0("[", tini,"] Add NER annotation (VP)"))
df_cpe_tag_vp <- cpeNERannotate(cpes = df_cpes_ner, version = FALSE)
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

tini <- Sys.time()
print(paste0("[", tini,"] Add NER annotation (Vendor)"))
df_cpe_tag_vend <- cpeNERannotate(cpes = df_cpes_ner, product = FALSE, version = FALSE)
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

tini <- Sys.time()
print(paste0("[", tini,"] Add NER annotation (Product)"))
df_cpe_tag_prod <- cpeNERannotate(cpes = df_cpes_ner, vendor = FALSE, version = FALSE)
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

topvends <- getTopVendors(df_cpes_ner, num_top)
topprods <- getTopProducts(df_cpes_ner, num_top)

allvends <- getTopVendors(df_cpes_ner, 0)
allprods <- getTopProducts(df_cpes_ner, 0)

# VPV
allvpv <- df_cpe_tag_vpv %>%
  inner_join(allvends, by = "vendor", keep = FALSE) %>%
  inner_join(allprods, by = "product", keep = FALSE) %>%
  select("cpe", "vendor", "product") %>%
  group_by(vendor, product) %>%
  summarise(num_rows = n(), .groups = "keep") %>%
  ungroup()
if (num_top > 0) {
  allvpv <- allvpv %>%
    slice_max(num_rows, n = num_top)
}
df_train <- df_cpe_tag_vpv %>%
  inner_join(allvpv, by = join_by(vendor, product)) %>%
  mutate(num_rows = round(abs(scale(num_rows)[,1]), 4)) %>%
  slice_sample(n = num_samples) %>%
  slice_sample(n = num_samples, weight_by = num_rows)


# VEND
print(paste0("[VEND] Add NER annotation and select sample"))
allvend <- df_cpe_tag_vend %>%
  inner_join(allvends, by = "vendor", keep = FALSE) %>%
  select("cpe", "vendor") %>%
  group_by(vendor) %>%
  summarise(num_rows = n(), .groups = "keep") %>%
  ungroup()
if (num_top > 0) {
  allvend <- allvend %>%
    slice_max(num_rows, n = num_top)
}
df_train <- df_cpe_tag_vend %>%
  inner_join(allvend, by = join_by(vendor)) %>%
  mutate(num_rows = round(abs(scale(num_rows)[, 1]), 4)) %>%
  slice_sample(n = num_samples, weight_by = num_rows)



# VPV
df_train <- df_cpe_tag_vpv %>%
  inner_join(allvends, by = "vendor", keep = FALSE) %>%
  inner_join(allprods, by = "product", keep = FALSE) %>%
  select("cpe", "vendor", "product") %>%
  group_by(vendor, product) %>%
  summarise(num_rows = n(), .groups = "keep") %>%
  ungroup()
if (num_top > 0) {
  df_train <- df_train %>%
    slice_sample(n = num_top, weight_by = num_rows)
}
df_train <- df_train %>%
  left_join(distinct(df_cpe_tag_vpv, vendor, product, .keep_all = TRUE),
            by = join_by(vendor, product))



df_train <- df_cpe_tag_vp %>%
  inner_join(allvends, by = "vendor", keep = FALSE) %>%
  inner_join(allprods, by = "product", keep = FALSE) %>%
  select("cpe", "vendor", "product") %>%
  group_by(vendor, product) %>%
  summarise(num_rows = n(), .groups = "keep") %>%
  ungroup() %>%
  slice_sample(n = num_top, weight_by = num_rows) %>%
  left_join(distinct(df_cpe_tag_vp, vendor, product, .keep_all = T), by = join_by(vendor, product))

df_train_vend <- df_cpe_tag_vend %>%
  arrange(title) %>%
  inner_join(topvends, by = "vendor", keep = FALSE) %>%
  arrange(title) %>%
  slice_sample(n = num_samples) %>%
  select("title", "annotated")
write.csv(df_train_vend, "data-raw/train_vendors.csv", row.names = FALSE)

df_train_prod <- df_cpe_tag_prod %>%
  arrange(title) %>%
  inner_join(topprods, by = "product", keep = FALSE) %>%
  arrange(title) %>%
  slice_sample(n = num_samples) %>%
  select("title", "annotated")
write.csv(df_train_prod, "data-raw/train_products.csv", row.names = FALSE)

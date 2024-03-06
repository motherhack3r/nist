library(nist)

# Download latest official CPE dataset
tini <- Sys.time()
print(paste0("[", tini,"] Get CPE data "))
cpes_file <- getLatestdata()
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

tini <- Sys.time()
print(paste0("[", tini,"] Parse XML "))
df_cpes <- parseCPExml(cpes_file)
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

tini <- Sys.time()
print(paste0("[", tini,"] Expanse with CPE components "))
df_cpes_ner <- cpe2wfn(df_cpes)
fini <- Sys.time()
print(paste0("[", fini,"] done in ", round(fini - tini, 2), "s"))

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



library(dplyr)

ents_vendor <- df_cpes_ner %>%
  select(cpe, vendor, product) %>%
  # group_by(vendor, product) %>%
  group_by(vendor) %>%
  summarise(num_rows = n(), .groups = "keep") %>%
  arrange(desc(num_rows)) %>%
  ungroup()

ents_product <- df_cpes_ner %>%
  select(cpe, vendor, product) %>%
  # group_by(vendor, product) %>%
  group_by(product) %>%
  summarise(num_rows = n(), .groups = "keep") %>%
  arrange(desc(num_rows)) %>%
  ungroup()


df_train_vend <- df_cpe_tag_vend[df_cpe_tag_vend$vendor %in% head(ents_vendor$vendor, 1000), ] %>%
  sample_n(10000)
write.csv(df_train_vend, "data-raw/train_vendors.csv", row.names = FALSE)

df_train_prod <- df_cpe_tag_prod[df_cpe_tag_prod$product %in% head(ents_product$product, 1000), ] %>%
  sample_n(10000)
write.csv(df_train_prod, "data-raw/train_products.csv", row.names = FALSE)

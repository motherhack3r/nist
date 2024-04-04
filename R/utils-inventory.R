#' Return data.frame with installed software name, version and vendor.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' inventory <- getInventory()
getInventory <- function(){
  if (.Platform$OS.type == "windows") {
    # Windows with powershell
    sw1 <- system("powershell.exe \"Get-ItemProperty HKLM:\\Software\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
    sw2 <- system("powershell.exe \"Get-ItemProperty HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
    sw3 <- system("powershell.exe \"Get-WmiObject Win32_Product | Sort-Object Name | Format-List Name, Version, Vendor\"", intern = T)

    sw1 <- stringi::stri_conv(sw1, from = "CP850", to = "UTF-8")
    sw2 <- stringi::stri_conv(sw2, from = "CP850", to = "UTF-8")
    sw3 <- stringi::stri_conv(sw3, from = "CP850", to = "UTF-8")

    NewSWEntry <- function(name = "", version = "", vendor = ""){return(data.frame(name = name, version = version, vendor = vendor, stringsAsFactors = F))}

    getInventoryTitle <- function(df = data.frame()) {
      df$test <- mapply(grepl, pattern = df$version, x = df$name, fixed = TRUE)
      df <- dplyr::mutate(df, title = dplyr::if_else(.data$test, paste(.data$vendor, .data$name), paste(.data$vendor, .data$name, .data$version)))
      df <- dplyr::rename(df, "product" = "name")
      df <- dplyr::select(df, "title", "vendor", "product", "version")
      df$title <- trimws(tolower(df$title))

      return(df)
    }

    # SW1
    df.sw1 <- NewSWEntry()
    for (linia in sw1) {
      new.sw <- NewSWEntry()
      new.row <- lapply(stringr::str_split(linia, ":"), stringr::str_trim)[[1]]
      switch(new.row[1],
             "DisplayName" = {
               new.sw.name <- new.row[2]
             },
             "DisplayVersion" = {
               new.sw.version <- new.row[2]
             },
             "Publisher" = {
               new.sw.publisher <- new.row[2]
             },
             "InstallDate" = {
               new.sw <- NewSWEntry(new.sw.name, new.sw.version, new.sw.publisher)
               new.sw.name <- new.sw.version <- new.sw.publisher <- ""
               df.sw1 <- dplyr::bind_rows(df.sw1, new.sw)
             },
             {
               # Default
             }
      )
    }
    df.sw1 <- df.sw1[!apply(df.sw1 == "", 1, all),]

    # SW2
    df.sw2 <- NewSWEntry()
    for (linia in sw2) {
      new.sw <- NewSWEntry()
      new.row <- lapply(stringr::str_split(linia, ":"), stringr::str_trim)[[1]]
      switch(new.row[1],
             "DisplayName" = {
               new.sw.name <- new.row[2]
             },
             "DisplayVersion" = {
               new.sw.version <- new.row[2]
             },
             "Publisher" = {
               new.sw.publisher <- new.row[2]
             },
             "InstallDate" = {
               new.sw <- NewSWEntry(new.sw.name, new.sw.version, new.sw.publisher)
               new.sw.name <- new.sw.version <- new.sw.publisher <- ""
               df.sw2 <- dplyr::bind_rows(df.sw2, new.sw)
             },
             {
               # Default
             }
      )
    }
    df.sw2 <- df.sw2[!apply(df.sw2 == "", 1, all),]

    # SW3
    df.sw3 <- NewSWEntry()
    for (linia in sw3) {
      new.sw <- NewSWEntry()
      new.row <- lapply(stringr::str_split(linia, ":"), stringr::str_trim)[[1]]
      switch(new.row[1],
             "Name" = {
               new.sw.name <- new.row[2]
             },
             "Version" = {
               new.sw.version <- new.row[2]
             },
             "Vendor" = {
               new.sw.publisher <- new.row[2]
               new.sw <- NewSWEntry(new.sw.name, new.sw.version, new.sw.publisher)
               new.sw.name <- new.sw.version <- new.sw.publisher <- ""
               df.sw3 <- dplyr::bind_rows(df.sw3, new.sw)
             },
             {
               # Default
             }
      )
    }
    df.sw3 <- df.sw3[!apply(df.sw3 == "", 1, all),]

    df.sw <- dplyr::bind_rows(df.sw1, df.sw2, df.sw3)
    df.sw <- df.sw[!duplicated(df.sw),]
    df.sw <- dplyr::arrange(df.sw, "name")

    df.sw$name <- stringr::str_conv(df.sw$name, "UTF-8")

    df.sw <- getInventoryTitle(df.sw)

    return(df.sw)
  }

  if (.Platform$OS.type == "unix") {
    # Debian
    sw <- system("dpkg-query -W -f='${binary:Package}\\;${Architecture}\\;${Version}\\;${Maintainer}\\n'", intern = T)
    df.sw <- utils::read.csv(text = sw, sep = ";", header = F,
                             col.names = c("name", "architecture", "version", "mantainer"))
    df.sw$name <- sapply(df.sw$name, function(x) stringr::str_split(x, ":")[[1]][1])

    df.sw$name <- stringr::str_conv(df.sw$name, "UTF-8")

    return(df.sw)
  }

  print("Only tested on Windows 10 and Debian. Sorry.")
  return(NA)
}

inventory_to_ner <- function() {

}

ner_to_inventory <- function(df_ner = data.frame(), df_cpes = cpe2wfn()) {

  # df_ner <- ner_inventory

  # Load official vendors and products
  cpe_vendors <- nist::getTopVendors(df_cpes, 0)
  cpe_products <- nist::getTopProducts(df_cpes, 0)

  # Use first column as id
  names(df_ner)[1] <- "id"

  # Replace NA with empty string
  df_ner <- df_ner %>% replace(is.na(.), "")

  # Split by CPE component
  df_ner_vend <- df_ner[ , stringr::str_detect(names(df_ner), "(vendor|id)")]
  df_ner_prod <- df_ner[ , stringr::str_detect(names(df_ner), "(product|id)")]
  df_ner_vers <- df_ner[ , stringr::str_detect(names(df_ner), "(version|id)")]

  ##############
  # Find official vendors

  #  - keep ner columns
  df_vendors <- df_ner_vend[,stringr::str_starts(names(df_ner_vend), "(ner_|id)")]
  #  - replace spaces with _
  df_vendors <- data.frame(lapply(df_vendors, function(x) {gsub("\\s+", "_", x)}))
  # Replacing "+" by "plus"
  df_vendors <- data.frame(lapply(df_vendors, function(x) {stringr::str_replace_all(x, "_*\\+", "_plus")}))
  # Replacing "r" or "r_core_team" by "r_project"
  df_vendors <- data.frame(lapply(df_vendors, function(x) {stringr::str_replace_all(x, "^r$", "r_project")}))
  df_vendors <- data.frame(lapply(df_vendors, function(x) {stringr::str_replace_all(x, "^r_core_team$", "r_project")}))
  df_vendors$id <- as.numeric(df_vendors$id)
  # - Find official candidates
  t_vend_vpv <- dplyr::inner_join(df_vendors[, c("id", "ner_vendor_vpv")], cpe_vendors, by = c("ner_vendor_vpv" = "vendor"))
  t_vend_vp <- dplyr::inner_join(df_vendors[, c("id", "ner_vendor_vp")], cpe_vendors, by = c("ner_vendor_vp" = "vendor"))
  t_vend_vv <- dplyr::inner_join(df_vendors[, c("id", "ner_vendor_vv")], cpe_vendors, by = c("ner_vendor_vv" = "vendor"))
  t_vend_vnd <- dplyr::inner_join(df_vendors[, c("id", "ner_vendor_vend")], cpe_vendors, by = c("ner_vendor_vend" = "vendor"))
  vendors <- dplyr::full_join(t_vend_vpv, t_vend_vp, by = "id")
  vendors <- dplyr::full_join(vendors, t_vend_vv, by = "id")
  vendors <- dplyr::full_join(vendors, t_vend_vnd, by = "id")
  # Cleansing: each row contains the official vendor or empty
  vendors <- vendors %>%
    rowwise() %>%
    mutate(cpe_vendor = head(sort(unique(as.character(pick(where(is.character), -id)))), 1)) %>%
    select("id", "cpe_vendor") %>%
    ungroup()
  vendors$cpe_vendor_score <- 1

  vendors_ner <- df_ner_vend[!(df_ner_vend$id %in% vendors$id), ]
  vendors_ner <- rbind((vendors_ner %>% select(id, ner_vendor_vpv, scr_vendor_vpv) %>% rename(cpe_vendor = ner_vendor_vpv, score = scr_vendor_vpv)),
                       (vendors_ner %>% select(id, ner_vendor_vp, scr_vendor_vp) %>% rename(cpe_vendor = ner_vendor_vp, score = scr_vendor_vp)),
                       (vendors_ner %>% select(id, ner_vendor_vv, scr_vendor_vv) %>% rename(cpe_vendor = ner_vendor_vv, score = scr_vendor_vv)),
                       (vendors_ner %>% select(id, ner_vendor_vend, scr_vendor_vend) %>% rename(cpe_vendor = ner_vendor_vend, score = scr_vendor_vend))) %>%
    filter(nchar(cpe_vendor) > 0) %>%
    group_by(id, cpe_vendor) %>%
    summarise(score = mean(score)) %>%
    ungroup() %>%
    arrange(id) %>%
    group_by(id) %>%
    filter(score == max(score)) %>%
    ungroup() %>%
    arrange(id) %>%
    rename(cpe_vendor_score = score)
  #  - replace spaces with _
  vendors_ner <- data.frame(lapply(vendors_ner, function(x) {gsub("\\s+", "_", x)}))

  df_cpe_vend <- rbind(vendors, vendors_ner) %>% arrange(id)
  df_cpe_vend$id <- as.numeric(df_cpe_vend$id)

  df_ner_cpe <- dplyr::left_join(df_ner, df_cpe_vend, by = "id") %>% ungroup()


  ##############
  # Find official products
  #
  #  - keep ner columns
  df_products <- df_ner_prod[,stringr::str_starts(names(df_ner_prod), "(ner_|id)")]
  #  - replace spaces with _
  df_products <- data.frame(lapply(df_products, function(x) {gsub("\\s+", "_", x)}))
  # # Replacing "+" by "plus"
  # df_vendors <- data.frame(lapply(df_vendors, function(x) {stringr::str_replace_all(x, "_*\\+", "_plus")}))
  # # Replacing "r" or "r_core_team" by "r_project"
  # df_vendors <- data.frame(lapply(df_vendors, function(x) {stringr::str_replace_all(x, "^r$", "r_project")}))
  # df_vendors <- data.frame(lapply(df_vendors, function(x) {stringr::str_replace_all(x, "^r_core_team$", "r_project")}))
  df_products$id <- as.numeric(df_products$id)
  # - Find official candidates
  t_prod_vpv <- dplyr::inner_join(df_products[, c("id", "ner_product_vpv")], cpe_products, by = c("ner_product_vpv" = "product"))
  t_prod_vp <- dplyr::inner_join(df_products[, c("id", "ner_product_vp")], cpe_products, by = c("ner_product_vp" = "product"))
  t_prod_pv <- dplyr::inner_join(df_products[, c("id", "ner_product_pv")], cpe_products, by = c("ner_product_pv" = "product"))
  t_prod_prd <- dplyr::inner_join(df_products[, c("id", "ner_product_prod")], cpe_products, by = c("ner_product_prod" = "product"))
  products <- dplyr::full_join(t_prod_vpv, t_prod_vp, by = "id")
  products <- dplyr::full_join(products, t_prod_pv, by = "id")
  products <- dplyr::full_join(products, t_prod_prd, by = "id")
  # Cleansing: each row contains the official vendor or empty
  products <- products %>%
    rowwise() %>%
    mutate(cpe_product = head(sort(unique(as.character(pick(where(is.character), -id)))), 1)) %>%
    select("id", "cpe_product") %>%
    ungroup()
  products$cpe_product_score <- 1

  products_ner <- df_ner_prod[!(df_ner_prod$id %in% products$id), ]
  products_ner <- rbind((products_ner %>% select(id, ner_product_vpv, scr_product_vpv) %>% rename(cpe_product = ner_product_vpv, score = scr_product_vpv)),
                       (products_ner %>% select(id, ner_product_vp, scr_product_vp) %>% rename(cpe_product = ner_product_vp, score = scr_product_vp)),
                       (products_ner %>% select(id, ner_product_pv, scr_product_pv) %>% rename(cpe_product = ner_product_pv, score = scr_product_pv)),
                       (products_ner %>% select(id, ner_product_prod, scr_product_prod) %>% rename(cpe_product = ner_product_prod, score = scr_product_prod))) %>%
    filter(nchar(cpe_product) > 0) %>%
    group_by(id, cpe_product) %>%
    summarise(score = mean(score)) %>%
    ungroup() %>%
    arrange(id) %>%
    group_by(id) %>%
    filter(score == max(score)) %>%
    ungroup() %>%
    arrange(id) %>%
    rename(cpe_product_score = score)
  #  - replace spaces with _
  products_ner <- data.frame(lapply(products_ner, function(x) {gsub("\\s+", "_", x)}))

  df_cpe_prod <- rbind(products, products_ner) %>% arrange(id)
  df_cpe_prod$id <- as.numeric(df_cpe_prod$id)

  df_ner_cpe <- dplyr::left_join(df_ner_cpe, df_cpe_prod, by = "id") %>% ungroup()

  # Find official versions


  return(df_ner_cpe)
}

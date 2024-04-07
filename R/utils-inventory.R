#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
get_mac_inventory <- function(verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE) {
  print("Only tested on Windows 10 and Debian. Sorry.")
  return(NA)
}


#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
get_unix_inventory <- function(verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE) {
  if (.Platform$OS.type == "unix") {
    # Debian
    if (verbose) print(paste0("[-] ", "Running dpkg ..."))
    sw <- system("dpkg-query -W -f='${binary:Package}\\;${Architecture}\\;${Version}\\;${Maintainer}\\n'", intern = T)
    df.sw <- utils::read.csv(text = sw, sep = ";", header = F,
                             col.names = c("name", "architecture", "version", "mantainer"))
    if (verbose) print(paste0("[-] ", "Inventory normalization..."))
    df.sw$name <- sapply(df.sw$name, function(x) stringr::str_split(x, ":")[[1]][1])
    df.sw$product <- stringr::str_conv(df.sw$name, "UTF-8")
    df.sw$vendor <- df.sw$mantainer
    df.sw$id <- 1:nrow(df.sw)
    df.sw <- df.sw[, c("id", "vendor", "product", "version")]

    return(df.sw)
  }


}


#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param include_libs logical
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
get_win_inventory <- function(include_libs = FALSE, verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE){
  NewSWEntry <- function(name = "", version = "", vendor = "") {
    return(data.frame(name = name,
                      version = version,
                      vendor = vendor,
                      stringsAsFactors = F))}

  # SW1
  if (verbose) print(paste0("[-] ", "Searching installed software in SCCM registry ..."))
  sw1 <- system("powershell.exe \"Get-ItemProperty HKLM:\\Software\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
  sw1 <- stringi::stri_conv(sw1, from = "CP850", to = "UTF-8")

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
  if (verbose) print(paste0("[-] ", "Searching installed software in CurrentVersion registry ..."))
  sw2 <- system("powershell.exe \"Get-ItemProperty HKLM:\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\* | Select-Object DisplayName, DisplayVersion, Publisher, InstallDate | Format-List\"", intern = T)
  sw2 <- stringi::stri_conv(sw2, from = "CP850", to = "UTF-8")

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
  if (include_libs) {
    if (verbose) print(paste0("[-] ", "Searching installed software in WMI objects ..."))
    sw3 <- system("powershell.exe \"Get-WmiObject Win32_Product | Sort-Object Name | Format-List Name, Version, Vendor\"", intern = T)
    sw3 <- stringi::stri_conv(sw3, from = "CP850", to = "UTF-8")

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
    df.sw2 <- dplyr::bind_rows(df.sw2, df.sw3)
  }

  if (verbose) print(paste0("[-] ", "Merge and normalize inventory ..."))
  df.sw <- dplyr::bind_rows(df.sw1, df.sw2)
  df.sw <- df.sw[!duplicated(df.sw),]
  df.sw <- dplyr::arrange(df.sw, "name")

  df.sw$product <- stringr::str_conv(df.sw$name, "UTF-8")
  df.sw$id <- 1:nrow(df.sw)
  df.sw <- df.sw[, c("id", "vendor", "product", "version")]

  return(df.sw)
}



#' Return data.frame with installed software name, version and vendor.
#' Set predict_cpes as TRUE to predict CPEs using ML.
#' Set predict_cves as TRUE to predict CPEs and its vulnerabilities as CVEs
#'
#' @param include_libs logical
#' @param verbose logical
#' @param predict_cpes logical
#' @param predict_cves logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' inventory <- getInventory()
getInventory <- function(include_libs = FALSE, verbose = FALSE, predict_cpes = FALSE, predict_cves = FALSE) {
  if (verbose) print(paste0("[*] ", "Checking Operating System ..."))
  switch(Sys.info()[['sysname']],
         Windows = {
           if (verbose) print(paste0("[-] Windows detected."))
           df_inventory <- get_win_inventory(include_libs = include_libs,
                                             verbose = verbose,
                                             predict_cpes = predict_cpes,
                                             predict_cves = predict_cves)
         },
         Linux   = {
           if (verbose) print(paste0("[-] Linux detected."))
           df_inventory <- get_unix_inventory(verbose = verbose,
                                              predict_cpes = predict_cpes,
                                              predict_cves = predict_cves)
         },
         Darwin  = {
           if (verbose) print(paste0("[-] Mac detected."))
           df_inventory <- get_mac_inventory(verbose = verbose,
                                             predict_cpes = predict_cpes,
                                             predict_cves = predict_cves)
         })
  df_inventory <- cpe_make_title(df_inventory = df_inventory, verbose = verbose) %>%
    arrange("title")
  df_inventory$id <- 1:nrow(df_inventory)

  # if (predict_cpes) {
  #   df_inventory <- cpe_generate(df_inventory = df_inventory, verbose = verbose)
  #   if (predict_cves) {
  #     df_inventory <- left_join(df_inventory,
  #                               df_inventory %>%
  #                                 filter(cpe_score > 0.5) %>%
  #                                 separate(col = cpe , sep = ":", extra = "merge",
  #                                          into = c("std", "v", "part", "vendor", "product", "version", "tail")) %>%
  #                                 select(id, vendor, product, version, vendor, product, version) %>%
  #                                 mutate(cpelite = paste0(":", paste(vendor, product, sep = ":"), ":")) %>%
  #                                 select(id, cpelite, version) %>%
  #                                 rowwise() %>%
  #                                 mutate(cves = cpelite_vulnerable_configs(x = cpelite, x_vers = version, verbose = verbose)) %>%
  #                                 ungroup() %>% select(id, cves),
  #                               by = "id")
  #   }
  # }

  return(df_inventory)
}


#' Title
#'
#' @param df_inventory data.frame
#' @param verbose logical
#'
#' @return data.frame
#' @export
cpe_make_title <- function(df_inventory = getInventory(), verbose = FALSE) {
  df_inventory <- inventory_to_ner(df_sccm = df_inventory, verbose = verbose)
  return(df_inventory)
}

#' Title
#'
#' @param verbose logical
#' @param df_sccm data.frame
#'
#' @return data.frame
inventory_to_ner <- function(df_sccm = data.frame(), verbose = FALSE) {
  if (!("id" %in% names(df_sccm))) {
    if (verbose) print(paste0("[*] ", "Adding id column..."))
    df_sccm$id <- 1:nrow(df_sccm)
  }
  df <- df_sccm
  if (verbose) print(paste0(" |> ", "Input rows: ", nrow(df)))

  # Clean vendor strings
  if (verbose) print(paste0("[|] ", "Clean vendor strings"))
  df_sccm$wfn_vendor <- iconv(df_sccm$vendor, from = "UTF-8", to = 'ASCII//TRANSLIT')
  df_sccm$wfn_vendor <- stringr::str_replace_all(df_sccm$wfn_vendor, "\\?", "")
  df_sccm$wfn_vendor <- stringr::str_trim(df_sccm$wfn_vendor)
  df_sccm$wfn_vendor <- textclean::replace_white(textclean::replace_html(textclean::replace_emoji(df_sccm$wfn_vendor)))
  df_sccm$bad_vendor <- (stringr::str_count(df_sccm$wfn_vendor, "[^a-zA-Z0-9 \\.]")
                         / sapply(df_sccm$wfn_vendor, nchar)
  ) > 0.2
  df_sccm$bad_vendor[is.na(df_sccm$bad_vendor)] <- TRUE
  df_vendor <- df_sccm[!df_sccm$bad_vendor, c("id", "wfn_vendor")]
  df_vendor <- dplyr::rename(df_vendor, vendor = .data$wfn_vendor)
  df_vendor$wfn_vendor <- cpe_wfn_vendor(df_vendor$vendor)
  df_vendor <- df_vendor[stringr::str_length(df_vendor$wfn_vendor) > 1, c("id", "wfn_vendor")]
  df_vendor <- dplyr::rename(df_vendor, vendor = .data$wfn_vendor)
  if (verbose) print(paste0(" |> ", "Good vendor rows: ", nrow(df_vendor)))

  # Clean product strings
  if (verbose) print(paste0("[|] ", "Clean product strings"))
  df_sccm$wfn_product <- iconv(df_sccm$product, from = "UTF-8", to = 'ASCII//TRANSLIT')
  df_sccm$wfn_product <- stringr::str_replace_all(df_sccm$wfn_product, "\\?", "")
  df_sccm$wfn_product <- stringr::str_trim(df_sccm$wfn_product)
  df_sccm$wfn_product <- textclean::replace_white(textclean::replace_html(textclean::replace_emoji(df_sccm$wfn_product)))
  df_sccm$bad_product <- (stringr::str_count(df_sccm$wfn_product, "[^a-zA-Z0-9 \\.\\+]")
                          / sapply(df_sccm$wfn_product, nchar)
  ) > 0.2
  df_sccm$bad_product[is.na(df_sccm$bad_product)] <- TRUE
  df_product <- df_sccm[!df_sccm$bad_product, c("id", "wfn_product")]
  df_product <- dplyr::rename(df_product, product = .data$wfn_product)
  df_product$wfn_product <- cpe_wfn_product(df_product$product)
  df_product <- df_product[stringr::str_length(df_product$wfn_product) > 1, c("id", "wfn_product")]
  df_product <- dplyr::rename(df_product, product = .data$wfn_product)
  if (verbose) print(paste0(" |> ", "Good product rows: ", nrow(df_product)))

  # Clean version strings
  if (verbose) print(paste0("[|] ", "Clean version strings"))
  df_sccm$wfn_version <- df_sccm$version
  df_sccm$wfn_version <- textclean::replace_white(textclean::replace_html(textclean::replace_emoji(df_sccm$wfn_version)))
  df_sccm$wfn_version[(grepl("\\d+(, \\d+)+", df_sccm$wfn_version))] <-
    gsub("\\, ", "\\.", df_sccm$wfn_version[(grepl("\\d+(, \\d+)+", df_sccm$wfn_version))])
  df_sccm$wfn_version[(grepl("\\d+(,\\d+)+", df_sccm$wfn_version))] <-
    gsub("\\,", "\\.", df_sccm$wfn_version[(grepl("\\d+(,\\d+)+", df_sccm$wfn_version))])
  df_sccm$wfn_version[(grepl("\\d+(\\. \\d+)+", df_sccm$wfn_version))] <-
    gsub("\\. ", "\\.", df_sccm$wfn_version[(grepl("\\d+(\\. \\d+)+", df_sccm$wfn_version))])

  df_sccm$wfn_version <- stringr::str_extract(df_sccm$wfn_version, "\\d+(\\.\\d+)*")
  # df_sccm$wfn_version <- stringr::str_replace_all(df_sccm$wfn_version, "(.*)(\\s|,|-)+[vers\\.]*([\\d|\\.]+)(.*)$", "\\3")
  df_version <- df_sccm[, c("id", "wfn_version")]
  df_version <- dplyr::rename(df_version, version = .data$wfn_version)
  df_version$version[is.na(df_version$version)] <- ""
  if (verbose) print(paste0(" |> ", "Good version rows: ", nrow(df_version)))

  if (verbose) print(paste0("[|] ", "General cleansing and remove NAs"))
  df_inv <- dplyr::inner_join(df_vendor, df_product, by = "id")
  df_inv <- dplyr::inner_join(df_inv, df_version, by = "id")
  # df_inv <- dplyr::full_join(df_version, df_vendor, by = "id")
  df_inv$vendor[is.na(df_inv$vendor)] <- ""
  # df_inv <- dplyr::full_join(df_inv, df_product, by = "id")
  df_inv$product[is.na(df_inv$product)] <- ""
  df_inv$version[is.na(df_inv$version)] <- ""

  df_inv$vendor <- stringr::str_replace_all(df_inv$vendor, "\\s+", " ")
  df_inv$vendor <- stringr::str_trim(df_inv$vendor)
  df_inv$product <- stringr::str_replace_all(df_inv$product, "\\s+", " ")
  df_inv$product <- stringr::str_trim(df_inv$product)
  df_inv$version <- stringr::str_replace_all(df_inv$version, "\\s+", " ")
  df_inv$version <- stringr::str_trim(df_inv$version)

  df_inv$product[nchar(df_inv$version) == 0] <- df_sccm$product[df_sccm$id %in% df_inv$id[nchar(df_inv$version) == 0]]
  if (verbose) print(paste0(" |> ", "Good rows: ", nrow(df_inv)))

  if (verbose) print(paste0("[|] ", "Removing duplicated vendor in title"))
  df_inv <- dplyr::mutate(df_inv,
                          title = ifelse(stringr::str_starts(.data$product, stringr::fixed(paste0(.data$vendor, " "))),
                                         paste(.data$product, .data$version),
                                         paste(.data$vendor, .data$product, .data$version)))

  df_inv$title <- stringr::str_replace_all(df_inv$title, "\\s+", " ")
  df_inv$title <- stringr::str_replace_all(df_inv$title, "\\b(\\w+\\s)\\1\\b(.*)", "\\1\\2")
  df_inv$title <- stringr::str_trim(df_inv$title)

  if (verbose) print(paste0("[|] ", "Custom common title cleansing ..."))
  df_inv$title <- stringr::str_replace_all(df_inv$title, "Microsoft vs ", "Microsoft Visual Studio ")
  df_inv$title <- stringr::str_replace_all(df_inv$title, "Microsoft vcpp ", "Microsoft Visual C++ ")

  df <- dplyr::left_join(df, dplyr::select(df_inv, "id", "title"), by = "id")
  df <- df[, c("id", "title", "vendor", "product", "version")]

  # Final cleansing
  if (verbose) print(paste0("[|] ", "Final cleansing..."))
  df <- as.data.frame(df)
  df <- df[!is.na(df$title), ]
  df$title <- iconv(df$title, to = 'ASCII//TRANSLIT')
  df$version <- iconv(df$version, to = 'ASCII//TRANSLIT')
  df <- df[!is.na(df$title), ]
  df$valid <- stringr::str_detect(str73enc(df$title), "\\*", negate = T)
  df <- df[df$valid, c("id", "title", "vendor", "product", "version")]
  if (verbose) print(paste0(" |> ", "Good rows: ", nrow(df)))

  return(df)
}


#' Title
#'
#' @param df_ner data.frame
#' @param df_cpes data.frame
#'
#' @return data.frame
#' @export
#'
ner_to_inventory <- function(df_ner = data.frame(), df_cpes = cpe2wfn()) {

  # df_ner <- df_ner_out

  # Load official vendors and products
  cpe_vendors <- nist::getTopVendors(df_cpes, 0)
  cpe_products <- nist::getTopProducts(df_cpes, 0)

  # Use first column as id
  names(df_ner)[1] <- "id"

  # Replace NA with empty string
  df_ner <- mutate(df_ner, across(everything(), ~replace_na(.x, "")))

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
    mutate(cpe_vendor = head(sort(unique(as.character(pick(where(is.character), -"id")))), 1)) %>%
    select("id", "cpe_vendor") %>%
    ungroup()
  vendors$cpe_vendor_score <- 1

  vendors_ner <- df_ner_vend[!(df_ner_vend$id %in% vendors$id), ]
  vendors_ner <- rbind((vendors_ner %>% select("id", "ner_vendor_vpv", "scr_vendor_vpv") %>% rename(cpe_vendor = "ner_vendor_vpv", score = "scr_vendor_vpv")),
                       (vendors_ner %>% select("id", "ner_vendor_vp", "scr_vendor_vp") %>% rename(cpe_vendor = "ner_vendor_vp", score = "scr_vendor_vp")),
                       (vendors_ner %>% select("id", "ner_vendor_vv", "scr_vendor_vv") %>% rename(cpe_vendor = "ner_vendor_vv", score = "scr_vendor_vv")),
                       (vendors_ner %>% select("id", "ner_vendor_vend", "scr_vendor_vend") %>% rename(cpe_vendor = "ner_vendor_vend", score = "scr_vendor_vend"))) %>%
    filter(nchar(.data$cpe_vendor) > 0) %>%
    group_by(.data$id, .data$cpe_vendor) %>%
    summarise(score = mean(.data$score)) %>%
    ungroup() %>%
    arrange("id") %>%
    group_by(.data$id) %>%
    filter(.data$score == max(.data$score)) %>%
    ungroup() %>%
    arrange("id") %>%
    rename(cpe_vendor_score = "score")
  #  - replace spaces with _
  vendors_ner <- data.frame(lapply(vendors_ner, function(x) {gsub("\\s+", "_", x)}))

  df_cpe_vend <- rbind(vendors, vendors_ner) %>% arrange("id")
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
    mutate(cpe_product = head(sort(unique(as.character(pick(where(is.character), -"id")))), 1)) %>%
    select("id", "cpe_product") %>%
    ungroup()
  products$cpe_product_score <- 1

  products_ner <- df_ner_prod[!(df_ner_prod$id %in% products$id), ]
  products_ner <- rbind((products_ner %>% select("id", "ner_product_vpv", "scr_product_vpv") %>% rename(cpe_product = "ner_product_vpv", score = "scr_product_vpv")),
                       (products_ner %>% select("id", "ner_product_vp", "scr_product_vp") %>% rename(cpe_product = "ner_product_vp", score = "scr_product_vp")),
                       (products_ner %>% select("id", "ner_product_pv", "scr_product_pv") %>% rename(cpe_product = "ner_product_pv", score = "scr_product_pv")),
                       (products_ner %>% select("id", "ner_product_prod", "scr_product_prod") %>% rename(cpe_product = "ner_product_prod", score = "scr_product_prod"))) %>%
    filter(nchar(.data$cpe_product) > 0) %>%
    group_by(.data$id, .data$cpe_product) %>%
    summarise(score = mean(.data$score)) %>%
    ungroup() %>%
    arrange("id") %>%
    group_by(.data$id) %>%
    filter(.data$score == max(.data$score)) %>%
    ungroup() %>%
    arrange("id") %>%
    rename(cpe_product_score = "score")
  #  - replace spaces with _
  products_ner <- data.frame(lapply(products_ner, function(x) {gsub("\\s+", "_", x)}))

  df_cpe_prod <- rbind(products, products_ner) %>% arrange("id")
  df_cpe_prod$id <- as.numeric(df_cpe_prod$id)

  df_ner_cpe <- dplyr::left_join(df_ner_cpe, df_cpe_prod, by = "id") %>% ungroup()

  ##############
  # Find official versions
  #
  cpe_versions <- df_cpes %>%
    dplyr::select("cpe", "version") %>%
    dplyr::group_by(.data$version) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::select("version")

  #  - keep ner columns
  df_versions <- df_ner_vers[,stringr::str_starts(names(df_ner_vers), "(ner_|id)")]
  #  - replace spaces with _
  df_versions <- data.frame(lapply(df_versions, function(x) {gsub("\\s+", "_", x)}))
  df_versions$id <- as.numeric(df_versions$id)
  # - Find official candidates
  t_vers_vpv <- dplyr::inner_join(df_versions[, c("id", "ner_version_vpv")], cpe_versions, by = c("ner_version_vpv" = "version"))
  t_vers_vv <- dplyr::inner_join(df_versions[, c("id", "ner_version_vv")], cpe_versions, by = c("ner_version_vv" = "version"))
  t_vers_pv <- dplyr::inner_join(df_versions[, c("id", "ner_version_pv")], cpe_versions, by = c("ner_version_pv" = "version"))
  t_vers_vrs <- dplyr::inner_join(df_versions[, c("id", "ner_version_vers")], cpe_versions, by = c("ner_version_vers" = "version"))
  versions <- dplyr::full_join(t_vers_vpv, t_vers_vv, by = "id")
  versions <- dplyr::full_join(versions, t_vers_pv, by = "id")
  versions <- dplyr::full_join(versions, t_vers_vrs, by = "id")
  # Cleansing: each row contains the official version or empty
  versions <- versions %>%
    rowwise() %>%
    mutate(cpe_version = head(sort(unique(as.character(pick(where(is.character), -"id")))), 1)) %>%
    select("id", "cpe_version") %>%
    ungroup()
  versions$cpe_version_score <- 1

  versions_ner <- df_ner_vers[!(df_ner_vers$id %in% versions$id), ]
  versions_ner <- rbind((versions_ner %>% select("id", "ner_version_vpv", "scr_version_vpv") %>% rename(cpe_version = "ner_version_vpv", score = "scr_version_vpv")),
                        (versions_ner %>% select("id", "ner_version_vv", "scr_version_vv") %>% rename(cpe_version = "ner_version_vv", score = "scr_version_vv")),
                        (versions_ner %>% select("id", "ner_version_pv", "scr_version_pv") %>% rename(cpe_version = "ner_version_pv", score = "scr_version_pv")),
                        (versions_ner %>% select("id", "ner_version_vers", "scr_version_vers") %>% rename(cpe_version = "ner_version_vers", score = "scr_version_vers"))) %>%
    filter(nchar(.data$cpe_version) > 0) %>%
    group_by(.data$id, .data$cpe_version) %>%
    summarise(score = mean(.data$score)) %>%
    ungroup() %>%
    arrange("id") %>%
    group_by(.data$id) %>%
    filter(.data$score == max(.data$score)) %>%
    ungroup() %>%
    arrange("id") %>%
    rename(cpe_version_score = "score")
  #  - replace spaces with _
  versions_ner <- data.frame(lapply(versions_ner, function(x) {gsub("\\s+", "_", x)}))

  df_cpe_vers <- rbind(versions, versions_ner) %>% arrange("id")
  df_cpe_vers$id <- as.numeric(df_cpe_vers$id)

  df_ner_cpe <- dplyr::left_join(df_ner_cpe, df_cpe_vers, by = "id") %>% ungroup()


  return(df_ner_cpe)
}

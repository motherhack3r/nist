#' Title
#'
#' @param datapath character
#'
#' @return character
#' @export
getLatestdata <- function(datapath = "data-raw") {
  if (!dir.exists(datapath)) dir.create(datapath)
  cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.gz"
  tf <- file.path(datapath, "official-cpe-dictionary_v2.3.xml.gz")
  ff <- file.path(datapath, "official-cpe-dictionary_v2.3.xml")

  ## Download the zip file
  download.file(cpes_url, tf)

  ## Unzip it in the temp folder
  R.utils::gunzip(tf, ff, remove = FALSE, overwrite = TRUE)

  return(ff)
}

#' Title
#'
#' @param key character
#'
#' @return character
wfn_charmap <- function(key = NA) {
  lcharmap <- rbind(c('%21','!'),
                    c('%22', '\"'),
                    c('%23', '#'),
                    c('%24', '$'),
                    c('%25', '%'),
                    c('%26', '&'),
                    c('%27', '\''),
                    c('%28', '('),
                    c('%29', ')'),
                    c('%2a', '*'),
                    c('%2b', '+'),
                    c('%2c', ','),
                    c('%2f', '/'),
                    c('%3a', ':'),
                    c('%3b', ';'),
                    c('%3c', '<'),
                    c('%3d', '='),
                    c('%3e', '>'),
                    c('%3f', '?'),
                    c('%40', '@'),
                    c('%5b', '['),
                    c('%5c', '\\'),
                    c('%5d', ']'),
                    c('%5e', '^'),
                    c('%60', '`'),
                    c('%7b', '{'),
                    c('%7c', '|'),
                    c('%7d', '}'),
                    c('%7e', '~')
  )
  vcharmap <- lcharmap[,2]
  names(vcharmap) <- lcharmap[,1]

  if (is.na(key))
    return(vcharmap)
  else if (key %in% lcharmap[, 1])
    return(as.character(vcharmap[key]))
  else
    return(NA)
}

#' Title
#'
#' @param x character
#'
#' @return character
wfn_encode <- function(x = character()) {
  # convert to ASCII with transliteration
  x <- iconv(x, to = "ASCII//TRANSLIT")
  map <- wfn_charmap()
  for (encoded_char in names(map)) {
    x <- gsub(paste0("\\", map[encoded_char]), encoded_char, x, fixed = T)
  }
  return(x)
}

#' Title
#'
#' @param x character
#'
#' @return character
wfn_decode <- function(x = character()) {
  # convert to ASCII with transliteration
  x <- iconv(x, to = "ASCII//TRANSLIT")
  map <- wfn_charmap()
  for (encoded_char in names(map)) {
    x <- gsub(encoded_char, paste0("\\", map[encoded_char]), x, fixed = T)
  }
  return(x)
}

#' Title
#'
#' @param xml_path character
#'
#' @return data.frame
#' @export
parseCPExml <- function(xml_path = getLatestdata()) {
  df_cpes <- plyr::ldply(
    xml2::as_list(
      rvest::html_elements(
        rvest::read_html(xml_path, encoding = "UTF-8"), "cpe-item")),
    function(x) data.frame(title = x$title[[1]][1],
                           cpe = attributes(x$`cpe23-item`)$name[1])
  )

  return(df_cpes)
}

#' Title
#'
#' @param df_cpes data.frame
#' @param map character
#'
#' @return data.frame
#' @export
cpe2wfn <- function(df_cpes = parseCPExml(), map = wfn_charmap()) {
  df_cpes$cpe <- wfn_encode(df_cpes$cpe)
  df_cpes <- tidyr::separate(
    data = df_cpes,
    col = "cpe",
    into = c('nist','release','part','vendor','product','version',
             'update','edition','language','sw_edition',
             'target_sw','target_hw','other'),
    sep = ":",
    remove = F)

  df_cpes <- as.data.frame(sapply(df_cpes, function(x) wfn_decode(x)))

  df_cpes <- df_cpes[which(stringr::str_count(df_cpes$title, "\\?") <= 1), ]
  df_cpes$title <- stringr::str_squish(df_cpes$title)

  return(df_cpes)
}

#' Title
#'
#' @param xml_path character
#' @param map character
#'
#' @return data.frame
#' @export
cpes_etl <- function(xml_path = getLatestdata(), map = wfn_charmap()) {
  df_cpes <- tidyr::separate(
    data = plyr::ldply(
      xml2::as_list(
        rvest::html_elements(
          rvest::read_html(xml_path, encoding = "UTF-8"), "cpe-item")),
      function(x) {
        xtitle <- x$title[[1]][1]
        xcpe <- attributes(x$`cpe23-item`)$name[1]
        for (encoded_char in names(map)) {
          xcpe <- gsub(paste0("\\", map[encoded_char]), encoded_char, xcpe, fixed = T)
        }
        data.frame(title = xtitle, cpe = xcpe)
      }),
    col = "cpe",
    into = c('nist','release','part','vendor','product','version',
             'update','edition','language','sw_edition',
             'target_sw','target_hw','other'),
    sep = ":",
    remove = F)

  df_cpes <- as.data.frame(sapply(df_cpes, function(x) wfn_decode(x)))

  return(df_cpes)
}


#' Annotate CPEs
#'
#' @param vendor logical
#' @param product logical
#' @param version logical
#' @param cpes data.frame
#'
#' @return data.frame
#' @export
cpeNERannotate <- function(cpes = cpes_etl(),
                           vendor = TRUE, product = TRUE, version = TRUE) {
  # part is not included in title
  df_ner <- dplyr::select(cpes, -"part")
  # remove rows with escaped chars due to WFN
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$vendor), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$product), ]
  df_ner <- df_ner[!grepl(pattern = "\\\\", df_ner$version), ]
  # replace WFN _ to space
  df_ner$vendor <- stringr::str_replace_all(df_ner$vendor, "_", " ")
  df_ner$product <- stringr::str_replace_all(df_ner$product, "_", " ")
  # lowercase title
  df_ner$title <- tolower(df_ner$title)
  # vendor entities candidates
  df_ner$train_v <- rep(F, nrow(df_ner))
  df_ner$train_v <- stringr::str_detect(df_ner$title, df_ner$vendor)
  # product entities candidates
  df_ner$train_p <- rep(F, nrow(df_ner))
  df_ner$train_p <- stringr::str_detect(df_ner$title, df_ner$product)
  # version entities candidates
  df_ner$train_r <- rep(F, nrow(df_ner))
  df_ner$train_r <- stringr::str_detect(df_ner$title, df_ner$version)

  if (vendor & product & version) {
    # Keep only titles with all entities
    df_ner <- dplyr::select(dplyr::filter(df_ner, .data$train_v & .data$train_p & .data$train_r),
                            -"train_v", -"train_p", -"train_r")
    # remove titles with equal vendor and product
    df_ner <- df_ner[which(df_ner$vendor != df_ner$product), ]

    # Add tags
    df_ner$annotated <- df_ner$title
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                                 pattern = paste0("(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)(", df_ner$version,")"),
                                                 replacement = "\\[\\1\\]\\(vendor\\)\\2\\[\\3\\]\\(product\\)\\4\\[\\5\\]\\(version\\)")

    df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(vendor\\).*\\]\\(product\\).*\\]\\(version\\)", df_ner$annotated)), ]

  } else if (vendor & product & !version) {
    # Keep only titles with vendor and product entities
    df_ner <- dplyr::select(dplyr::filter(df_ner, .data$train_v & .data$train_p),
                            -"train_v", -"train_p", -"train_r")
    # remove titles with equal vendor and product
    df_ner <- df_ner[which(df_ner$vendor != df_ner$product), ]

    # Add tags
    df_ner$annotated <- df_ner$title
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                                 pattern = paste0("(", df_ner$vendor,")(\\s.*)(", df_ner$product,")(.*)"),
                                                 replacement = "\\[\\1\\]\\(vendor\\)\\2\\[\\3\\]\\(product\\)\\4")

    df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(vendor\\).*\\]\\(product\\).*", df_ner$annotated)), ]

  } else if (!vendor & product & version) {
    # Keep only titles with vendor and product entities
    df_ner <- dplyr::select(dplyr::filter(df_ner, .data$train_p & .data$train_r),
                            -"train_v", -"train_p", -"train_r")

    # Add tags
    df_ner$annotated <- df_ner$title
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                                 pattern = paste0("(", df_ner$product,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\[\\1\\]\\(product\\)\\2\\[\\3\\]\\(version\\)\\4")

    df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(product\\).*\\]\\(version\\).*", df_ner$annotated)), ]

  } else if (vendor & !product & version) {
    # Keep only titles with vendor and version entities
    df_ner <- dplyr::select(dplyr::filter(df_ner, .data$train_v & .data$train_r),
                            -"train_v", -"train_p", -"train_r")

    # Add tags
    df_ner$annotated <- df_ner$title
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                                 pattern = paste0("(", df_ner$vendor,")(\\s.*)(", df_ner$version,")(.*)"),
                                                 replacement = "\\[\\1\\]\\(vendor\\)\\2\\[\\3\\]\\(version\\)\\4")

    df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(vendor\\).*\\]\\(version\\).*", df_ner$annotated)), ]

  } else if (vendor & !product & !version) {
    # Keep only titles with vendor entity
    df_ner <- dplyr::select(dplyr::filter(df_ner, .data$train_v),
                            -"train_v", -"train_p", -"train_r")

    # Add tags
    df_ner$annotated <- df_ner$title
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                                 pattern = paste0("(", df_ner$vendor,")(.*)"),
                                                 replacement = "\\[\\1\\]\\(vendor\\)\\2")

    df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(vendor\\).*", df_ner$annotated)), ]

  } else if (!vendor & product & !version) {
    # Keep only titles with product entity
    df_ner <- dplyr::select(dplyr::filter(df_ner, .data$train_p),
                            -"train_v", -"train_p", -"train_r")

    # Add tags
    df_ner$annotated <- df_ner$title
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                                 pattern = paste0("(", df_ner$product,")(.*)"),
                                                 replacement = "\\[\\1\\]\\(product\\)\\2")

    df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(product\\).*", df_ner$annotated)), ]

  } else if (!vendor & !product & version) {
    # Keep only titles with product entity
    df_ner <- dplyr::select(dplyr::filter(df_ner, .data$train_r),
                            -"train_v", -"train_p", -"train_r")

    # Add tags
    df_ner$annotated <- df_ner$title
    df_ner$annotated <- stringr::str_replace_all(string = df_ner$annotated,
                                                 pattern = paste0("(", df_ner$version,")(.*)"),
                                                 replacement = "\\[\\1\\]\\(version\\)\\2")

    df_ner <- df_ner[which(grepl(pattern = ".*\\]\\(version\\).*", df_ner$annotated)), ]

  } else {

  }

  df_ner <- dplyr::select(df_ner, "title", "cpe", "vendor", "product", "version", "annotated")

  return(df_ner)
}


#' Title
#'
#' @param df_cpes data.frame
#' @param num_top numeric
#'
#' @return data.frame
#' @export
getTopVendors <- function(df_cpes = cpe2wfn(), num_top = 100) {
  ents_vp <- df_cpes %>%
    dplyr::select("cpe", "vendor", "product") %>%
    dplyr::group_by(.data$vendor, .data$product) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup()

  ents_vend <- ents_vp %>%
    dplyr::select("vendor", "product") %>%
    dplyr::group_by(.data$vendor) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup()

  ents_vendor <- df_cpes %>%
    dplyr::select("cpe", "vendor", "product") %>%
    dplyr::group_by(.data$vendor) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup()

  sample_vend <- ents_vend %>%
    dplyr::bind_rows(ents_vendor) %>%
    dplyr::group_by(.data$vendor) %>%
    dplyr::summarise(num_rows = sum(.data$num_rows), .groups = "keep") %>%
    dplyr::ungroup()

  if (num_top > 0) {
    sample_vend <- sample_vend %>%
      dplyr::slice_sample(n = num_top, weight_by = .data$num_rows)
  }

  sample_vend <- sample_vend %>% dplyr::select("vendor")

  return(sample_vend)
}

#' Title
#'
#' @param df_cpes data.frame
#' @param num_top numeric
#'
#' @return data.frame
#' @export
getTopProducts <- function(df_cpes = cpe2wfn(), num_top = 100) {
  ents_vp <- df_cpes %>%
    dplyr::select("cpe", "vendor", "product") %>%
    dplyr::group_by(.data$vendor, .data$product) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup()

  ents_prod <- ents_vp %>%
    dplyr::select("vendor", "product") %>%
    dplyr::group_by(.data$product) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup()

  ents_product <- df_cpes %>%
    dplyr::select("cpe", "vendor", "product") %>%
    dplyr::group_by(.data$product) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup()

  sample_prod <- ents_prod %>%
    dplyr::bind_rows(ents_product) %>%
    dplyr::group_by(.data$product) %>%
    dplyr::summarise(num_rows = sum(.data$num_rows), .groups = "keep") %>%
    dplyr::ungroup()

  if (num_top > 0) {
    sample_prod <- sample_prod %>%
      dplyr::slice_sample(n = num_top, weight_by = .data$num_rows)
  }

  sample_prod <- sample_prod %>% dplyr::select("product")

  return(sample_prod)
}

#' Title
#'
#' @param df data.frame
#' @param num_samples numeric
#' @param randomize logical
#'
#' @return data.frame
#' @export
getCPEsample <- function(df = cpe2wfn(), num_samples = 5000, randomize = FALSE) {
  df_sample <- df %>%
    dplyr::select("cpe", "vendor", "product") %>%
    dplyr::group_by(.data$vendor, .data$product) %>%
    dplyr::summarise(num_rows = dplyr::n(), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(num_rows = round(abs(scale(.data$num_rows)[,1]), 4))
  if (randomize) {
    df_sample <- df_sample %>% dplyr::slice_sample(n = num_samples)
  } else {
    df_sample <- df_sample %>% dplyr::slice_sample(n = num_samples, weight_by = .data$num_rows)
  }
  df_train <- df %>%
    dplyr::inner_join(df_sample, by = dplyr::join_by("vendor", "product")) %>%
    dplyr::slice_sample(n = num_samples) %>%
    dplyr::select(-"num_rows")

  return(df_train)
}

#' Title
#'
#' @param x character
#'
#' @return character
cpe_wfn_vendor <- function(x = "Microsoft Corporation") {
  # Remove (c) (tm) (r)
  x <- stringr::str_replace_all(x, "(?i)\\([c|tm|r]\\)", "")
  x <- stringr::str_replace_all(x, "\\u00AE", "")
  x <- stringr::str_replace_all(x, "\\u00A9", "")

  # Normalize vendor: First apply translit, then remove bad words and HTML entities
  x <- iconv(x, to = 'ASCII//TRANSLIT', sub = "")

  # CUSTOM MODIFICATORS
  # R Core Team --> r_project
  x <- stringr::str_replace_all(x, "(?i)R Core Team", "r_project")
  # The R Foundation --> r_foundation
  x <- stringr::str_replace_all(x, "(?i)The R Foundation", "r_foundation")

  # If starts with (, remove parenthesis and keep text until end or )
  x <- stringr::str_replace_all(x, "(?i)^\\(([^\\)]+)\\){0,1}", "\\1")
  # Extract text inside parenthesis
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)\\)", "")
  # Remove any combination of development|core and team|company, separated by space, comma or -
  x <- stringr::str_replace_all(x, "(?i)(\\s|,|-)*(development|core){0,1}(\\s|,|-)*(team|company){0,1}$", " ")
  x <- stringr::str_trim(x)
  # Remove any text equal or equivalent to: corporation, incorporated, international, etc.
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(co|corp|corporation|corporations|ltd|llc|cc|inc|incorporated|company|international)\\.{0,1}$", "")
  # Extract word software or soft
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(software|soft)(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove S.A. and S.L. variants
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+s\\.(a|l)\\.(\\s|\\,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove L.P. variants
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+l\\.p\\.(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove word foundation
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+foundation(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove word systems
  # x <- stringr::str_replace_all(x, "(?i)(\\s|,)+systems(\\s|,){0,1}", " ")
  # x <- stringr::str_trim(x)
  # Remove word technologies
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+technologies(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove word limited
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+limited(\\s|,){0,1}", " ")
  x <- stringr::str_trim(x)
  # Remove words with only numbers or -
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+[\\d\\-]+(\\s)*", " ")
  x <- stringr::str_trim(x)
  # Again remove any combination of development|core and team|company, separated by space, comma or -
  x <- stringr::str_replace_all(x, "(?i)(\\s|,|-)*(development|core){0,1}(\\s|,|-)*(team|company){0,1}$", " ")
  x <- stringr::str_trim(x)
  # Again remove any text equal or equivalent to: corporation, incorporated, international, etc.
  x <- stringr::str_replace_all(x, "(?i)(\\s|,)+(co|corp|corporation|corporations|ltd|llc|cc|inc|incorporated|company|international)\\.{0,1}$", "")
  x <- stringr::str_trim(x)
  # Extract text inside HTML tags
  x <- sapply(x, function(y) xml2::xml_text(xml2::read_html(paste0("<x>",y,"</x>"))))

  x <- stringr::str_replace_all(x, "^[^a-zA-Z0-9]+$", "")
  x <- stringr::str_replace_all(x, "^[^a-zA-Z0-9]+\\s([a-zA-Z0-9].+)$", "\\1")
  # Extract text inside ${}
  x <- stringr::str_replace_all(x, "^\\${0,1}\\{(.+)\\}$", "")
  # Extract text between $
  x <- stringr::str_replace_all(x, "^\\$(.+)\\$$", "")
  # Remove ()
  x <- stringr::str_replace_all(x, "\\(\\)", "")
  # Text finishing with () --> remove ()
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)$", "")
  # Remove []
  x <- stringr::str_replace_all(x, "\\[\\]", "")
  # Extract text between '' or ""
  x <- stringr::str_replace_all(x, "^'([^']+)'$", "\\1")
  x <- stringr::str_replace_all(x, "^\"([^']+)\"$", "\\1")

  # Errors from SCCM query
  x <- stringr::str_replace_all(x, "(?i)CFullName", "")

  # CUSTOM MODIFICATORS
  # sap_xx --> sap
  x <- stringr::str_replace_all(x, "(?i)sap_[a-z]{2}", "sap")
  # Advanced Micro Devices --> AMD
  x <- stringr::str_replace_all(x, "(?i)Advanced Micro Devices", "AMD")
  # ASUSTek Computer --> ASUSTEK
  x <- stringr::str_replace_all(x, "(?i)ASUSTek(\\s|\\.)*Computer(\\s|\\.)*(inc){0,1}", "ASUSTEK")
  # Hewlett-Packard --> hp
  x <- stringr::str_replace_all(x, "(?i)Hewlett(\\s|\\.|\\-)*Packard(\\s|\\.|\\-)*", "HP ")
  # Internet Testing Systems --> ITS
  x <- stringr::str_replace_all(x, "(?i)Internet Testing Systems", "ITS")
  # Amazon Web Services --> Amazon
  x <- stringr::str_replace_all(x, "(?i)Amazon Web Services", "Amazon")
  # Adobe Systems Incorporated (+variations)--> Adobe
  x <- stringr::str_replace_all(x, "(?i)Adobe([[:punct:]]|\\s)*(System|s)*([[:punct:]]|\\s|\\t)*(Inc)*([[:punct:]]|\\s)*(orporated){0,1}([[:punct:]]|\\s)*(Company)*", "Adobe")

  x <- stringr::str_replace_all(x, "(?i)(\\s)+S\\.(A|p)\\.(S|a)\\.(\\s|$)", " ")
  x <- stringr::str_trim(x)

  x[x == "NA"] <- NA
  x[is.na(x)] <- ""

  return(x)
}

#' Title
#'
#' @param x character
#'
#' @return character
cpe_wfn_product <- function(x = "Oracle VM VirtualBox 6.1.34") {
  x <- stringr::str_replace_all(x, "\\u00AE", "")
  x <- stringr::str_replace_all(x, "\\u00A9", "")
  x <- iconv(x, to = 'ASCII//TRANSLIT')
  x <- stringr::str_replace_all(x, "\\(.*$", "")
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "(\\s|,|-)+v*(\\d+\\.{0,1})+\\.{0,1}\\d*$", "")
  x <- stringr::str_replace_all(x, "\\s", "_")
  x <- stringr::str_replace_all(x, "_\\-_.*$", "")
  x <- stringr::str_replace_all(x, "_\\d+\\.\\d+.*$", "")
  x <- stringr::str_replace_all(x, "(?i)_{0,1}(x|amd)(32|64|86).*$", "")
  x <- stringr::str_replace_all(x, "(?i)_for_.*$", "")
  x <- stringr::str_replace_all(x, "\\(\\)", "")
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)\\)", "")
  x <- stringr::str_replace_all(x, "\\(([^\\)]+)$", "")
  x <- stringr::str_replace_all(x, "(?i)\\(r\\)", "")
  x <- stringr::str_replace_all(x, "(?i)\\(tm\\)", "")
  x <- stringr::str_replace_all(x, "(?i)\\(c\\)", "")
  x <- stringr::str_replace_all(x, "_", " ")
  x <- stringr::str_replace_all(x, "(?i)\\([c|tm|r]\\)", "")
  x <- stringr::str_replace_all(x, "(?i)^\\(([^\\)]+)\\)", "\\1")
  x <- stringr::str_trim(x)

  return(x)
}

#' Encode strings to only 73 accepted characters for custom WFN.
#'   - Replace accents, dieresis, etc to simple ASCII chars
#'   - Replace tabs with spaces
#'   - Deal with valid escaped symbols
#'   - Replace not valid characters with "*"
#'
#' @param name character vector with CPE names
#' @param na_replace character, no valid chars will be replaced with "*" by default
#'
#' @return character
str73enc <- function(name = character(), na_replace = "*") {
  encname <- iconv(name, to = 'ASCII//TRANSLIT', sub = na_replace)

  # valid_dec_chars <- dec_valid_chars("input")
  # valid_chars <- sapply(valid_dec_chars, DescTools::AscToChar)
  valid_chars <- " !&()+,-./0123456789:ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  regex_notvalid <- stringr::fixed(paste0("[^", paste0(valid_chars, collapse = ""), "]"))

  # Deal with tabs `\t`
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "^[\\x09]", "")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "\\s[\\x09]", " ")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x09]", " ")
  selected_rows <- stringr::str_detect(encname, "[\\x09]")

  # Deal with `escaped and accepted symbols`
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\-", "-")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\!", "!")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\&", "&")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\(", "(")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\)", ")")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\,", ",")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\.", ".")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\/", "/")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\:", ":")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\_", "_")
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5c]\\+", "+")

  # Finally, replace all escaped
  selected_rows <- stringr::str_detect(encname, "[\\x5c]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows],
                                                     paste0("[\\x5c]+(", regex_notvalid, ")"), "\\1")

  # Replace underscore with spaces
  selected_rows <- stringr::str_detect(encname, "[\\x5f]")
  encname[selected_rows] <- stringr::str_replace_all(encname[selected_rows], "[\\x5f]", " ")

  # Remove not valid characters
  encname <- stringi::stri_replace_all_regex(encname, regex_notvalid, na_replace)

  return(encname)
}


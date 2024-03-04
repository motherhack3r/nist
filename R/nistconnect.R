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
wfn.charmap <- function(key = NA) {
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
  map <- wfn.charmap()
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
  map <- wfn.charmap()
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
        rvest::read_html(xml_path,
                         encoding = "UTF-8"),
        "cpe-item")
    ),
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
cpe2wfn <- function(df_cpes = parseCPExml(), map = wfn.charmap()) {
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

  return(df_cpes)
}

#' Title
#'
#' @param xml_path character
#' @param map character
#'
#' @return data.frame
#' @export
cpes_etl <- function(xml_path = getLatestdata(), map = wfn.charmap()) {
  df_cpes <- tidyr::separate(
    data = plyr::ldply(
      xml2::as_list(
        rvest::html_elements(
          rvest::read_html(xml_path,
                           encoding = "UTF-8"),
          "cpe-item")
      ),
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


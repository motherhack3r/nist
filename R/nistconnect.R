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

# tini <- Sys.time()
# print(paste0("[", tini,"] START "))
# df_cpes_lite <- cpes_etl()
# fini <- Sys.time()
# print(paste0("[", fini,"] ", round(fini - tini, 2)))


# tini <- Sys.time()
# print(paste0("[", tini,"] START "))
# df_cpes_lite <- parseCPExml()
# fini <- Sys.time()
# print(paste0("[", fini,"] ", round(fini - tini, 2)))
# df_cpes <- cpe2wfn(df_cpes_lite)
# fini <- Sys.time()
# print(paste0("[", fini,"] ", round(fini - tini, 2)))
# tini <- fini
# fini <- Sys.time()


#
#
#
# tini <- Sys.time()
# print(paste0("[", tini,"] START "))
# cpes_file <- getLatestdata()
# fini <- Sys.time()
# print(paste0("[", fini,"] ", round(fini - tini, 2)))
# tini <- fini
# fini <- Sys.time()
# print(paste0("[", fini,"] ", round(fini - tini, 2)))
# df_cpes <- parseCPExml(cpes_file)
# fini <- Sys.time()
# print(paste0("[", fini,"] ", round(fini - tini, 2)))
# tini <- fini
# fini <- Sys.time()


#################



# wfn.charmap <- function(key = NA) {
#   lcharmap <- rbind(c('%21','!'),
#                     c('%22', '\"'),
#                     c('%23', '#'),
#                     c('%24', '$'),
#                     c('%25', '%'),
#                     c('%26', '&'),
#                     c('%27', '\''),
#                     c('%28', '('),
#                     c('%29', ')'),
#                     c('%2a', '*'),
#                     c('%2b', '+'),
#                     c('%2c', ','),
#                     c('%2f', '/'),
#                     c('%3a', ':'),
#                     c('%3b', ';'),
#                     c('%3c', '<'),
#                     c('%3d', '='),
#                     c('%3e', '>'),
#                     c('%3f', '?'),
#                     c('%40', '@'),
#                     c('%5b', '['),
#                     c('%5c', '\\'),
#                     c('%5d', ']'),
#                     c('%5e', '^'),
#                     c('%60', '`'),
#                     c('%7b', '{'),
#                     c('%7c', '|'),
#                     c('%7d', '}'),
#                     c('%7e', '~')
#   )
#   vcharmap <- lcharmap[,2]
#   names(vcharmap) <- lcharmap[,1]
#
#   if (is.na(key))
#     return(vcharmap)
#   else if (key %in% lcharmap[, 1])
#     return(as.character(vcharmap[key]))
#   else
#     return(NA)
# }
#
# wfn_encode <- function(x = character()) {
#   # convert to ASCII with transliteration
#   x <- iconv(x, to = "ASCII//TRANSLIT")
#   # # encode_escaped_double_points
#   # x <- stringr::str_replace_all(x, "\\\\:", "\\\\%3a")
#   # # encode_escaped_tildes
#   # x <- stringr::str_replace_all(x, "\\\\~", "\\\\%7e")
#   # # encode_non_alphanumeric_characters
#   # x <- stringr::str_replace_all(x, "\\\\~", "\\\\%7e")
#   map <- wfn.charmap()
#   for (encoded_char in names(map)) {
#     # x <- gsub(map[encoded_char], paste0("\\", encoded_char), x, fixed = T)
#     x <- gsub(paste0("\\", map[encoded_char]), encoded_char, x, fixed = T)
#     # x <- gsub(map[encoded_char], encoded_char, x, fixed = T)
#   }
#
#   return(x)
# }
# wfn_encode(x)
#
#
# sapply(wfn.charmap(), function(x) x)
#
#
# df <- df_cpes[2000:2010,c("title", "cpe")]
#
#
# k <- rjson::fromJSON(charmap_json)
# k <- jsonlite::fromJSON(charmap_json)
# k <- jsonStrings::jsonString$new(charmap_json)
#
#
#
#
#
#
#
#
# x <- df %>% extract(col = "cpe",
#                     into = c('nist','release','part','vendor','product','version',
#                              'update','edition','language','sw_edition',
#                              'target_sw','target_hw','other'),
#                     regex = "^([^:]):$")
#
# x <- df %>% separate(col = "cpe",
#                      into = c('nist','release','part','vendor','product','version',
#                               'update','edition','language','sw_edition',
#                               'target_sw','target_hw','other'),
#                      sep = ":",
#                      remove = F)








#################

# tini <- Sys.time()
# df_cpes <- plyr::ldply(
#   xml2::as_list(
#     rvest::html_elements(
#       rvest::read_html("data-raw/official-cpe-dictionary_v2.3.xml",
#                        encoding = "UTF-8"),
#       "cpe-item")
#   ),
#   function(x) {
#     xtitle <- x$title[[1]][1]
#     xcpe <- attributes(x$`cpe23-item`)$name[1]
#     data.frame(title = xtitle, cpe = xcpe)
#   })
# fini <- Sys.time()
#
#
#
# k <- head(df_cpes, 100) %>% separate(col = "cpe", into = c('nist','release','part','vendor','product','version','update','edition','language','sw_edition','target_sw','target_hw','other'), sep = ":", remove = F)
#
#
#
# ldoc <- xml2::as_list(
#   rvest::html_elements(
#     rvest::read_html("data-raw/official-cpe-dictionary_v2.3.xml",
#                      encoding = "UTF-8"),
#     "cpe-item")
#   )
#
#
#
#
#
#
# doc <- rvest::read_html("data-raw/official-cpe-dictionary_v2.3.xml", encoding = "UTF-8")
# docitems <- rvest::html_elements(doc, "cpe-item")
# xml2doctitles <- rvest::html_elements(doc, "cpe-item.title")
#
# lcpes <- data.frame(title = character(), cpe = character())
# num_items <- length(docitems)
# num_items <- 1000
# for (x in 1:num_items) {
#   title = rvest::html_text(rvest::html_element(docitems[[x]], "title"))[1]
#   cpe = rvest::html_attr(rvest::html_element(docitems[[x]], "cpe23-item"), "name")
#   lcpes <- rbind(lcpes, data.frame(title = title, cpe = cpe))
# }
#
# rvest::html_name(cpes)
# rvest::html_name(rvest::html_children(cpes))

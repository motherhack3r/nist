#' Title
#'
#' @param title character
#'
#' @return character
#' @export
ml_ner_quick_vendor <- function(title = "") {

  ners <- sapply(title,
                 function(t) {
                   x <- entity::organization_entity(t)[[1]]
                   ifelse(is.null(x), "", as.character(x))
                 })
  names(ners) <- NULL

  return(ners)
}

#' Title
#'
#' @param text data.frame , first and second column are used as input and target texts
#' @param input logical , default set as TRUE, otherwise text characters will be used
#' @param output logical , default set as TRUE, otherwise text characters will be used
#'
#' @return list , with input_texts,target_texts,input_characters,target_characters,num_encoder_tokens,num_decoder_tokens,max_encoder_seq_length,max_decoder_seq_length
#' @export
ml_lstm_tokenize <- function(text,
                             input = c("wfn","cpe","txt")[sample.int(3,1)],
                             output = c("wfn","cpe","txt")[sample.int(3,1)]) {
  ## Vectorization of the data.
  input_texts  <- text[[1]]
  input_texts  <- dplyr::lapply(input_texts, function(s) strsplit(s, split = "")[[1]])
  target_texts <- paste0('\t',text[[2]],'\n')
  target_texts <- dplyr::lapply(target_texts, function(s) strsplit(s, split = "")[[1]])

  if (input == "wfn") {
    input_characters  <- getWFNchars()
  } else if (input == "cpe") {
    input_characters  <- getCPEchars()
  } else if (input == "txt") {
    input_characters  <- sort(unique(unlist(input_texts)))
  } else {
    print("[ERROR] wrong input value.")
  }

  if (output == "wfn") {
    target_characters <- getWFNchars()
  } else if (output == "cpe") {
    target_characters <- getCPEchars()
  } else if (output == "txt") {
    target_characters <- sort(unique(unlist(target_texts)))
  } else {
    print("[ERROR] wrong output value.")
  }

  num_encoder_tokens <- length(input_characters)
  num_decoder_tokens <- length(target_characters)
  max_encoder_seq_length <- max(sapply(input_texts,length))
  max_decoder_seq_length <- max(sapply(target_texts,length))


  cat('Number of samples:', length(input_texts),'\n')
  cat('Number of unique input tokens:', num_encoder_tokens, '\n')
  cat('Number of unique output tokens:', num_decoder_tokens,'\n')
  cat('Max sequence length for inputs:', max_encoder_seq_length,'\n')
  cat('Max sequence length for outputs:', max_decoder_seq_length,'\n')

  return(list(input_texts = input_texts,
              target_texts = target_texts,
              input_characters = input_characters,
              target_characters = target_characters,
              num_encoder_tokens = num_encoder_tokens,
              num_decoder_tokens = num_decoder_tokens,
              max_encoder_seq_length = max_encoder_seq_length,
              max_decoder_seq_length = max_decoder_seq_length))
}

#' Title
#'
#' @param input_texts character
#' @param target_texts character
#' @param input_characters character
#' @param target_characters character
#' @param num_encoder_tokens numeric
#' @param num_decoder_tokens numeric
#' @param max_encoder_seq_length numeric
#' @param max_decoder_seq_length numeric
#'
#' @return list
#' @export
ml_lstm_layers <- function(input_texts = "",
                           target_texts = "",
                           input_characters= "",
                           target_characters= "",
                           num_encoder_tokens= 0,
                           num_decoder_tokens= 0,
                           max_encoder_seq_length= 0,
                           max_decoder_seq_length= 0) {

  input_token_index  <- 1:length(input_characters)
  names(input_token_index) <- input_characters
  target_token_index <- 1:length(target_characters)
  names(target_token_index) <- target_characters
  encoder_input_data <- array(
    0, dim = c(length(input_texts), max_encoder_seq_length, num_encoder_tokens))
  decoder_input_data <- array(
    0, dim = c(length(input_texts), max_decoder_seq_length, num_decoder_tokens))
  decoder_target_data <- array(
    0, dim = c(length(input_texts), max_decoder_seq_length, num_decoder_tokens))

  for (i in 1:length(input_texts)) {
    d1 <- sapply( input_characters, function(x) { as.integer(x == input_texts[[i]]) })
    encoder_input_data[i,1:nrow(d1),] <- d1
    d2 <- sapply( target_characters, function(x) { as.integer(x == target_texts[[i]]) })
    decoder_input_data[i,1:nrow(d2),] <- d2
    d3 <- sapply( target_characters, function(x) { as.integer(x == target_texts[[i]][-1]) })
    decoder_target_data[i,1:nrow(d3),] <- d3
  }

  ##----------------------------------------------------------------------
  ## Create the model
  ##----------------------------------------------------------------------

  ## Define an input sequence and process it.
  encoder_inputs  <- keras::layer_input(shape = list(NULL,num_encoder_tokens))
  encoder         <- keras::layer_lstm(units = latent_dim, return_state = TRUE)
  encoder_results <- encoder_inputs %>% encoder
  ## We discard `encoder_outputs` and only keep the states.
  encoder_states  <- encoder_results[2:3]

  ## Set up the decoder, using `encoder_states` as initial state.
  decoder_inputs  <- keras::layer_input(shape = list(NULL, num_decoder_tokens))
  ## We set up our decoder to return full output sequences,
  ## and to return internal states as well. We don't use the
  ## return states in the training model, but we will use them in inference.
  decoder_lstm    <- keras::layer_lstm(units = latent_dim, return_sequences = TRUE,
                                return_state = TRUE, stateful = FALSE)
  decoder_results <- decoder_lstm(decoder_inputs, initial_state = encoder_states)
  decoder_dense   <- keras::layer_dense(object = NULL, units = num_decoder_tokens, activation = 'softmax')
  decoder_outputs <- decoder_dense(decoder_results[[1]])

  lstm_layers <- list(encoder_input_data = encoder_input_data,
                      decoder_input_data = decoder_input_data,
                      decoder_target_data = decoder_target_data,
                      encoder_inputs = encoder_inputs,
                      decoder_inputs = decoder_inputs,
                      decoder_outputs = decoder_outputs)

  return(lstm_layers)
}

#' Title
#'
#' @param encoder_input_data list
#' @param decoder_input_data list
#' @param decoder_target_data list
#' @param encoder_inputs list
#' @param decoder_inputs list
#' @param decoder_outputs list
#' @param optimizer character
#' @param loss character
#' @param batch_size numeric
#' @param epochs numeric
#' @param validation_split numeric
#' @param save_path character
#' @param model_name character
#'
#' @return character
#' @export
ml_lstm_model_train <- function(encoder_input_data = NA,
                                decoder_input_data = NA,
                                decoder_target_data = NA,
                                encoder_inputs = NA,
                                decoder_inputs = NA,
                                decoder_outputs = NA,
                                optimizer = "rmsprop",
                                loss = "categorical_crossentropy",
                                batch_size = 32,
                                epochs = 100,
                                validation_split = 0.2,
                                save_path = "models",
                                model_name = make.names(paste(gsub("\\.", "", OpenRepGrid::randomWords(3)), collapse = "_"))) {

  ## Define the model that will turn
  ## `encoder_input_data` & `decoder_input_data` into `decoder_target_data`
  model <- keras::keras_model(inputs = list(encoder_inputs, decoder_inputs),
                              outputs = decoder_outputs )

  ## Compile model
  model %>% keras::compile(optimizer = optimizer, loss = loss)

  ## Run model
  model %>% keras::fit( list(encoder_input_data, decoder_input_data), decoder_target_data,
                        batch_size = batch_size,
                        epochs = epochs,
                        validation_split = 0.2)

  ## Save model
  keras::save_model_hdf5(model, paste0(model_name,'.h5'))
  keras::save_model_weights_hdf5(model,paste0(model_name,'-wt.h5'))

  return(model)
}


#' Title
#'
#' @return character
#' @export
getWFNchars <- function() {
  ascii_digit_chars <- strsplit(intToUtf8(48:57), split = "")[[1]]
  ascii_alphacap_chars <- strsplit(intToUtf8(65:90), split = "")[[1]]
  ascii_alpha_chars <- strsplit(intToUtf8(97:122), split = "")[[1]]
  ascii_punct_chars <- strsplit("?*-_!\"#$%&'()+,./:;<=>@[]^`{|}~", split = "")[[1]]
  ascii_cpe_chars <- sort(unique(c(ascii_digit_chars,
                                   ascii_alphacap_chars,
                                   ascii_alpha_chars,
                                   ascii_punct_chars)))

  return(ascii_cpe_chars)
}

#' Title
#'
#' @param title character
#' @param vendor character
#' @param product character
#' @param version character
#' @param cpes data.frame
#'
#' @return character
#' @export
getCPEchars <- function(title = TRUE, vendor = TRUE, product = TRUE, version = TRUE, cpes = cpe2wfn()) {
  cpe_chars <- character()

  if (title) {
    cpe_title_chars <- sort(unique(unlist(lapply(unique(cpes$title), function(s) strsplit(s, split = "")[[1]]))))
    cpe_chars <- c(cpe_chars,cpe_title_chars)
  }
  if (vendor) {
    cpe_vendor_chars <- sort(unique(unlist(lapply(unique(cpes$vendor), function(s) strsplit(s, split = "")[[1]]))))
    cpe_chars <- c(cpe_chars,cpe_vendor_chars)
  }
  if (product) {
    cpe_product_chars <- sort(unique(unlist(lapply(unique(cpes$product), function(s) strsplit(s, split = "")[[1]]))))
    cpe_chars <- c(cpe_chars,cpe_product_chars)
  }
  if (vendor) {
    cpe_version_chars <- sort(unique(unlist(lapply(unique(cpes$version), function(s) strsplit(s, split = "")[[1]]))))
    cpe_chars <- c(cpe_chars,cpe_version_chars)
  }
  cpe_chars <- sort(unique(cpe_chars))

  return(cpe_chars)
}

#' #' Title
#' #'
#' #' @param text character
#' #' @param cpes data.frame
#' #'
#' #' @return character
#' #' @export
#' vectorizeCPE <- function(text, cpes) {
#'   ## Vectorization of the data.
#'   input_texts  <- text[[1]]
#'   input_texts  <- dplyr::lapply(input_texts, function(s) strsplit(s, split = "")[[1]])
#'   target_texts <- paste0('\t',text[[2]],'\n')
#'   target_texts <- dplyr::lapply(target_texts, function(s) strsplit(s, split = "")[[1]])
#'
#'   input_characters  <- sort(unique(unlist(input_texts)))
#'   target_characters <- sort(unique(unlist(target_texts)))
#'
#'   ascii_cpe_chars <- sort(unique(unlist(lapply(unique(cpes$version), function(s) strsplit(s, split = "")[[1]]))))
#'   ascii_printable_chars <- sort(strsplit(intToUtf8(32:126), split="")[[1]])
#'   ascii_extended_chars <- sort(strsplit(intToUtf8(c(8:13,32:126,128:155,160:165,174:175,182:183,224:237)), split = "")[[1]])
#'
#'   all_ascii_characters <- sort(unique(c(input_characters,
#'                                         target_characters,
#'                                         ascii_cpe_chars)))
#'
#'   # input_ascii_chars <- ascii_printable_chars[which(!(ascii_printable_chars %in% input_characters ))]
#'   # target_ascii_chars <- ascii_printable_chars[which(!(ascii_printable_chars %in% target_characters ))]
#'   # input_extra_chars <- ascii_extended_chars[which(!(ascii_extended_chars %in% input_characters ))]
#'   # target_extra_chars <- ascii_extended_chars[which(!(ascii_extended_chars %in% target_characters ))]
#'   # input_characters <- sort(c(input_characters, input_ascii_chars))
#'   # target_characters <- sort(c(target_characters, target_ascii_chars))
#'
#'   input_characters <- sort(unique(c(input_characters, ascii_cpe_chars)))
#'   target_characters <- sort(unique(c(target_characters, ascii_cpe_chars)))
#'
#'   num_encoder_tokens <- length(input_characters)
#'   num_decoder_tokens <- length(target_characters)
#'   max_encoder_seq_length <- max(sapply(input_texts,length))
#'   max_decoder_seq_length <- max(sapply(target_texts,length))
#'
#'
#'   cat('Number of samples:', length(input_texts),'\n')
#'   cat('Number of unique input tokens:', num_encoder_tokens, '\n')
#'   cat('Number of unique output tokens:', num_decoder_tokens,'\n')
#'   cat('Max sequence length for inputs:', max_encoder_seq_length,'\n')
#'   cat('Max sequence length for outputs:', max_decoder_seq_length,'\n')
#'
#'   return(list(input_texts = input_texts,
#'               target_texts = target_texts,
#'               input_characters = input_characters,
#'               target_characters = target_characters,
#'               num_encoder_tokens = num_encoder_tokens,
#'               num_decoder_tokens = num_decoder_tokens,
#'               max_encoder_seq_length = max_encoder_seq_length,
#'               max_decoder_seq_length = max_decoder_seq_length))
#' }
#'
#' #' Title
#' #'
#' #' @param cpe_vect
#' #' @param model_filename
#' #'
#' #' @return
#' #' @export
#' trainLSTM <- function(cpe_vect, model_filename = "str2vendor") {
#'   input_texts <- cpe_vect$input_texts
#'   target_texts <- cpe_vect$target_texts
#'   input_characters <- cpe_vect$input_characters
#'   target_characters <- cpe_vect$target_characters
#'   num_encoder_tokens <- cpe_vect$num_encoder_tokens
#'   num_decoder_tokens <- cpe_vect$num_decoder_tokens
#'   max_encoder_seq_length <- cpe_vect$max_encoder_seq_length
#'   max_decoder_seq_length <- cpe_vect$max_decoder_seq_length
#'
#'   input_token_index  <- 1:length(input_characters)
#'   names(input_token_index) <- input_characters
#'   target_token_index <- 1:length(target_characters)
#'   names(target_token_index) <- target_characters
#'   encoder_input_data <- array(
#'     0, dim = c(length(input_texts), max_encoder_seq_length, num_encoder_tokens))
#'   decoder_input_data <- array(
#'     0, dim = c(length(input_texts), max_decoder_seq_length, num_decoder_tokens))
#'   decoder_target_data <- array(
#'     0, dim = c(length(input_texts), max_decoder_seq_length, num_decoder_tokens))
#'
#'   for(i in 1:length(input_texts)) {
#'     d1 <- sapply( input_characters, function(x) { as.integer(x == input_texts[[i]]) })
#'     encoder_input_data[i,1:nrow(d1),] <- d1
#'     d2 <- sapply( target_characters, function(x) { as.integer(x == target_texts[[i]]) })
#'     decoder_input_data[i,1:nrow(d2),] <- d2
#'     d3 <- sapply( target_characters, function(x) { as.integer(x == target_texts[[i]][-1]) })
#'     decoder_target_data[i,1:nrow(d3),] <- d3
#'   }
#'
#'   ##----------------------------------------------------------------------
#'   ## Create the model
#'   ##----------------------------------------------------------------------
#'
#'   ## Define an input sequence and process it.
#'   encoder_inputs  <- layer_input(shape=list(NULL,num_encoder_tokens))
#'   encoder         <- layer_lstm(units=latent_dim, return_state=TRUE)
#'   encoder_results <- encoder_inputs %>% encoder
#'   ## We discard `encoder_outputs` and only keep the states.
#'   encoder_states  <- encoder_results[2:3]
#'
#'   ## Set up the decoder, using `encoder_states` as initial state.
#'   decoder_inputs  <- layer_input(shape=list(NULL, num_decoder_tokens))
#'   ## We set up our decoder to return full output sequences,
#'   ## and to return internal states as well. We don't use the
#'   ## return states in the training model, but we will use them in inference.
#'   decoder_lstm    <- layer_lstm(units=latent_dim, return_sequences=TRUE,
#'                                 return_state=TRUE, stateful=FALSE)
#'   decoder_results <- decoder_lstm(decoder_inputs, initial_state=encoder_states)
#'   decoder_dense   <- layer_dense(units = num_decoder_tokens, activation = 'softmax')
#'   decoder_outputs <- decoder_dense(decoder_results[[1]])
#'
#'   ## Define the model that will turn
#'   ## `encoder_input_data` & `decoder_input_data` into `decoder_target_data`
#'   model <- keras_model( inputs = list(encoder_inputs, decoder_inputs),
#'                         outputs = decoder_outputs )
#'
#'   ## Compile model
#'   model %>% compile(optimizer='rmsprop', loss='categorical_crossentropy')
#'
#'   ## Run model
#'   model %>% fit( list(encoder_input_data, decoder_input_data), decoder_target_data,
#'                  batch_size=batch_size,
#'                  epochs=epochs,
#'                  validation_split=0.2)
#'
#'   ## Save model
#'   save_model_hdf5(model, paste0(model_filename,'.h5'))
#'   save_model_weights_hdf5(model,paste0(model_filename,'-wt.h5'))
#'
#'   return(model)
#' }
#'

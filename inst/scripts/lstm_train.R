# LSTM for Inventory to CPE
keras::use_condaenv("rgpu")
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(keras)

vectorizeData <- function(text, cpes) {
  ## Vectorize the data.
  input_texts  <- text[[1]]
  input_texts  <- lapply( input_texts, function(s) strsplit(s, split = "")[[1]])
  target_texts <- paste0('\t',text[[2]],'\n')
  target_texts <- lapply( target_texts, function(s) strsplit(s, split = "")[[1]])

  input_characters  <- sort(unique(unlist(input_texts)))
  target_characters <- sort(unique(unlist(target_texts)))

  ascii_cpe_chars <- sort(unique(unlist(lapply(unique(cpes$version), function(s) strsplit(s, split="")[[1]]))))
  ascii_printable_chars <- sort(strsplit(intToUtf8(32:126), split="")[[1]])
  ascii_extended_chars <- sort(strsplit(intToUtf8(c(8:13,32:126,128:155,160:165,174:175,182:183,224:237)), split="")[[1]])

  all_ascii_characters <- sort(unique(c(input_characters,
                                        target_characters,
                                        ascii_cpe_chars)))

  # input_ascii_chars <- ascii_printable_chars[which(!(ascii_printable_chars %in% input_characters ))]
  # target_ascii_chars <- ascii_printable_chars[which(!(ascii_printable_chars %in% target_characters ))]
  # input_extra_chars <- ascii_extended_chars[which(!(ascii_extended_chars %in% input_characters ))]
  # target_extra_chars <- ascii_extended_chars[which(!(ascii_extended_chars %in% target_characters ))]
  # input_characters <- sort(c(input_characters, input_ascii_chars))
  # target_characters <- sort(c(target_characters, target_ascii_chars))

  input_characters <- sort(unique(c(input_characters, ascii_cpe_chars)))
  target_characters <- sort(unique(c(target_characters, ascii_cpe_chars)))

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

trainLSTM <- function(cpe_vect, model_filename = "str2vendor") {
  input_texts <- cpe_vect$input_texts
  target_texts <- cpe_vect$target_texts
  input_characters <- cpe_vect$input_characters
  target_characters <- cpe_vect$target_characters
  num_encoder_tokens <- cpe_vect$num_encoder_tokens
  num_decoder_tokens <- cpe_vect$num_decoder_tokens
  max_encoder_seq_length <- cpe_vect$max_encoder_seq_length
  max_decoder_seq_length <- cpe_vect$max_decoder_seq_length

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

  for(i in 1:length(input_texts)) {
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
  encoder_inputs  <- layer_input(shape=list(NULL,num_encoder_tokens))
  encoder         <- layer_lstm(units=latent_dim, return_state=TRUE)
  encoder_results <- encoder_inputs %>% encoder
  ## We discard `encoder_outputs` and only keep the states.
  encoder_states  <- encoder_results[2:3]

  ## Set up the decoder, using `encoder_states` as initial state.
  decoder_inputs  <- layer_input(shape=list(NULL, num_decoder_tokens))
  ## We set up our decoder to return full output sequences,
  ## and to return internal states as well. We don't use the
  ## return states in the training model, but we will use them in inference.
  decoder_lstm    <- layer_lstm(units=latent_dim, return_sequences=TRUE,
                                return_state=TRUE, stateful=FALSE)
  decoder_results <- decoder_lstm(decoder_inputs, initial_state=encoder_states)
  decoder_dense   <- layer_dense(units = num_decoder_tokens, activation = 'softmax')
  decoder_outputs <- decoder_dense(decoder_results[[1]])

  ## Define the model that will turn
  ## `encoder_input_data` & `decoder_input_data` into `decoder_target_data`
  model <- keras_model( inputs = list(encoder_inputs, decoder_inputs),
                        outputs = decoder_outputs )

  ## Compile model
  model %>% compile(optimizer='rmsprop', loss='categorical_crossentropy')

  ## Run model
  model %>% fit( list(encoder_input_data, decoder_input_data), decoder_target_data,
                 batch_size=batch_size,
                 epochs=epochs,
                 validation_split=0.2)

  ## Save model
  save_model_hdf5(model, paste0(model_filename,'.h5'))
  save_model_weights_hdf5(model,paste0(model_filename,'-wt.h5'))

  return(model)
}

# Model training parameters (default)
seed = 42
batch_size = 16  # Batch size for training.
epochs = 100  # Number of epochs to train for.
latent_dim = 32  # Latent dimensionality of the encoding space.
num_samples = 10000  # Number of samples to train on.

set.seed(seed)


df_cpes <- nist::cpe2wfn(readRDS("data-raw/df_cpes.rds"))
cpe2version <- nist::getCPEsample(df_cpes, num_samples, TRUE) %>% select("title", "version")
cpe_vect_version <- vectorizeData(cpe2version, df_cpes)


model4version <- trainLSTM(cpe_vect_version, 'lstm2version')

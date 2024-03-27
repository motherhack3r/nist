# Inference
library(dplyr, warn.conflicts = FALSE)
library(stringr)
Sys.setenv(TF_ENABLE_ONEDNN_OPTS = 0)
library(keras)
keras::use_condaenv("rgpu")

# Inference parameters
seed <-  42
model_path <- 'str2vendor.h5'
model_weights_path <- 'str2vendor-wt.h5'
latent_dim = 128  # Latent dimensionality of the encoding space.

set.seed(seed)

# PREPARE INPUT
# input_txt  <- mitre::getInventory()
input_txt <- dplyr::sample_n(cpes, 1000)
input_txt <- input_txt[[1]]
input_txt <- tolower(input_txt) %>% textclean::replace_non_ascii()
input_txt <- iconv(input_txt, "UTF-8", "ASCII", sub = "")

## Define an input sequence and process it.
# version
input_txt  <- lapply( input_txt, function(s) strsplit(s, split="")[[1]])
input_chars  <- sort(unique(unlist(input_txt)))
num_enc_tokens <- length(input_chars)
encoder_in  <- layer_input(shape=list(NULL,num_enc_tokens))
enc_lstm <- layer_lstm(units=latent_dim, return_state=TRUE)
encoder_res <- encoder_in %>% enc_lstm
## We discard `encoder_outputs` and only keep the states.
encoder_sts  <- encoder_res[2:3]

num_dec_tokens <- num_enc_tokens
dec_inputs  <- layer_input(shape=list(NULL, num_dec_tokens))


# Load Model
model <- load_model_hdf5(model_path)
load_model_weights_hdf5(model,model_weights_path)

##----------------------------------------------------------------------
## Next: inference mode (sampling).
##----------------------------------------------------------------------
## Here's the drill:
## 1) encode input and retrieve initial decoder state
## 2) run one step of decoder with this initial state
## and a "start of sequence" token as target.
## Output will be the next target token
## 3) Repeat with the current target token and current states

## Define sampling models
enc_model <-  keras_model(encoder_in, encoder_sts)
dec_state_input_h <- layer_input(shape=latent_dim)
dec_state_input_c <- layer_input(shape=latent_dim)
dec_states_inputs <- c(dec_state_input_h, dec_state_input_c)
dec_lstm    <- layer_lstm(units=latent_dim, return_sequences=TRUE,
                          return_state=TRUE, stateful=FALSE)
dec_results <- dec_lstm(dec_inputs, initial_state=dec_states_inputs)
dec_states  <- dec_results[2:3]
dec_dense   <- layer_dense(units = num_dec_tokens, activation = 'softmax')
dec_outputs <- dec_dense(dec_results[[1]])
dec_model   <- keras_model(
  inputs  = c(dec_inputs, dec_states_inputs),
  outputs = c(dec_outputs, dec_states))

## Reverse-lookup token index to decode sequences back to
## something readable.
reverse_input_char_index  <- as.character(input_chars)
reverse_target_char_index <- as.character(input_chars)

input_token_index  <- 1:length(input_chars)
names(input_token_index) <- input_chars
target_token_index <- 1:length(input_chars)
names(target_token_index) <- input_chars
target_chars <- ascii_printable_chars <- sort(strsplit(intToUtf8(32:126), split="")[[1]])
max_encoder_seq_length <- max(sapply(input_txt,length))
max_decoder_seq_length <- max_encoder_seq_length

encoder_input_data <- array(
  0, dim = c(length(input_txt), max_encoder_seq_length, num_enc_tokens))
decoder_input_data <- array(
  0, dim = c(length(input_txt), max_decoder_seq_length, num_dec_tokens))
decoder_target_data <- array(
  0, dim = c(length(input_txt), max_decoder_seq_length, num_dec_tokens))

for(i in 1:length(input_txt)) {
  d1 <- sapply( input_chars, function(x) { as.integer(x == input_txt[[i]]) })
  encoder_input_data[i,1:nrow(d1),] <- d1
  d2 <- sapply( target_chars, function(x) { as.integer(x == input_txt[[i]]) })
  decoder_input_data[i,1:nrow(d2),] <- d2
  d3 <- sapply( target_chars, function(x) { as.integer(x == input_txt[[i]][-1]) })
  decoder_target_data[i,1:nrow(d3),] <- d3
}

max_enc_seq_length <- max(sapply(input_txt,length))
max_dec_seq_length <- max_enc_seq_length



decode_sequence <- function(input_seq) {
  ## Encode the input as state vectors.
  states_value <- predict(enc_model, input_seq)

  ## Generate empty target sequence of length 1.
  target_seq <- array(0, dim=c(1, 1, num_dec_tokens))
  ## Populate the first character of target sequence with the start character.
  target_seq[1, 1, target_token_index['\t']] <- 1.

  ## Sampling loop for a batch of sequences
  ## (to simplify, here we assume a batch of size 1).
  stop_condition = FALSE
  decoded_sentence = ''
  maxiter = max_dec_seq_length
  niter = 1
  while (!stop_condition && niter < maxiter) {

    ## output_tokens, h, c = decoder_model.predict([target_seq] + states_value)
    decoder_predict <- predict(dec_model, c(list(target_seq), states_value))
    output_tokens <- decoder_predict[[1]]

    ## Sample a token
    sampled_token_index <- which.max(output_tokens[1, 1, ])
    sampled_char <- reverse_target_char_index[sampled_token_index]
    decoded_sentence <-  paste0(decoded_sentence, sampled_char)
    decoded_sentence

    ## Exit condition: either hit max length
    ## or find stop character.
    if (sampled_char == '\n' ||
        length(decoded_sentence) > max_dec_seq_length) {
      stop_condition = TRUE
    }

    ## Update the target sequence (of length 1).
    ## target_seq = np.zeros((1, 1, num_decoder_tokens))
    target_seq[1, 1, ] <- 0
    target_seq[1, 1, sampled_token_index] <- 1.

    ## Update states
    h <- decoder_predict[[2]]
    c <- decoder_predict[[3]]
    states_value = list(h, c)
    niter <- niter + 1
  }
  return(decoded_sentence)
}





for (seq_index in 1:100) {
  ## Take one sequence (part of the training test)
  ## for trying out decoding.
  input_seq = encoder_input_data[seq_index,,,drop=FALSE]
  decoded_sentence = decode_sequence(input_seq)
  # target_sentence <- gsub("\t|\n","",paste(target_texts[[seq_index]],collapse=''))
  input_sentence  <- paste(input_txt[[seq_index]],collapse='')
  cat(paste0('- [', seq_index,']\n'))
  cat('Input sentence  : ', input_sentence,'\n')
  # cat('Target sentence : ', target_sentence,'\n')
  cat('Decoded sentence: ', decoded_sentence,'\n')
}

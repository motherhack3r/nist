keras::use_condaenv("rgpu")
library(dplyr, warn.conflicts = FALSE)

df_cpes <- readRDS("data-raw/df_cpes_ner.rds")

seed = 42
num_samples = 100000  # Number of samples to train on.

set.seed(seed)

cpe2version <- df_cpes[grepl("^(\\.|\\d)+$",df_cpes$version), ] %>%
  sample_n(num_samples) %>%
  select("title", "version")

# cpe2version <- df_cpes[grepl("^(\\.|\\d|\\-)+$",df_cpes$version), ] %>%
#   mutate(nvers = nchar(version)) %>%
#   sample_n(num_samples, weight = nvers) %>%
#   select("title", "version")
#
# cpe2version <- df_cpes %>%
#   mutate(nvers = nchar(version)) %>%
#   sample_n(num_samples, weight = nvers) %>%
#   select("title", "version")

# cpe2version <- nist::getCPEsample(df_cpes, num_samples, TRUE) %>% select("title", "version")
# cpe2version <- nist::getCPEsample(df_cpes, num_samples, TRUE)

# Model training parameters (default)
batch_size = 32  # Batch size for training.
epochs = 20  # Number of epochs to train for.
latent_dim = 64  # Latent dimensionality of the encoding space.
optimizer = "rmsprop"
loss = "categorical_crossentropy"
validation_split = 0.2
save_path = "models"
model_name = "nist_cpe"


cpe_tokens <- nist::ml_lstm_tokenize(text = cpe2version, input = "wfn", output = "txt")
cpe_layers <- nist::ml_lstm_layers(input_texts = cpe_tokens$input_texts,
                                   target_texts = cpe_tokens$target_texts,
                                   input_characters = cpe_tokens$input_characters,
                                   target_characters = cpe_tokens$target_characters,
                                   num_encoder_tokens = cpe_tokens$num_encoder_tokens,
                                   num_decoder_tokens = cpe_tokens$num_decoder_tokens,
                                   max_encoder_seq_length = cpe_tokens$max_encoder_seq_length,
                                   max_decoder_seq_length = cpe_tokens$max_decoder_seq_length,
                                   latent_dim = latent_dim)
cpe_model <- nist::ml_lstm_model_train(encoder_input_data = cpe_layers$encoder_input_data,
                                       decoder_input_data = cpe_layers$decoder_input_data,
                                       decoder_target_data = cpe_layers$decoder_target_data,
                                       encoder_inputs = cpe_layers$encoder_inputs,
                                       decoder_inputs = cpe_layers$decoder_inputs,
                                       decoder_outputs = cpe_layers$decoder_outputs,
                                       optimizer = optimizer,
                                       loss = loss,
                                       batch_size = batch_size,
                                       epochs = epochs,
                                       validation_split = validation_split,
                                       save_path = save_path,
                                       model_name = model_name)



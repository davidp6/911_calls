# ----------------------------------------------------------------------------
# David Phillips
#
# 5/29/2018
# Fit a neural network to the Seattle data to enable classification of other calls
# Source: https://data.seattle.gov/Public-Safety/Seattle-Police-Department-911-Incident-Response/3k2p-39jp
# The working directory should be the root of this repo
# The downloaded data should be in the root of this repo (but not committed to the remote!)
#
# Outputs: An rdata file with three objects:
# - model 			An object of class keras_model_sequential fit to the Seattle data
# - vectorizer 	A text2vec object to help vectorize new text using the same training terms
# - y_codebook	A data.table with two columns relating y indices with values
# See classify_calls.r for application of results
# ----------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
set.seed(1)
library(data.table)
library(text2vec)
library(keras)
library(ggplot2)
# --------------------


# ---------------------------------------------------------------
# Files and directories

# input file
inFileSea = './Seattle_Police_Department_911_Incident_Response.csv'

# output files
outFile = './model_fit.rdata'
# ---------------------------------------------------------------


# ------------------------------------------------------------
# Load/prep data

# load all files
dataSea = fread(inFileSea)

# subset
x = dataSea[initial_type_group!='' & initial_type_description!='']$initial_type_description
y = dataSea[initial_type_group!='' & initial_type_description!='']$initial_type_group
# ------------------------------------------------------------


# --------------------------------------------
# Vectorize text

# make x word index
it_x = itoken(x, preprocessor = tolower,
             tokenizer = word_tokenizer)
word_index = create_vocabulary(it_x)

# make x word matrix
vectorizer = vocab_vectorizer(word_index)
dtm_x = create_dtm(it_x, vectorizer)

# make y word index
y_index = as.numeric(as.factor(y))
y_codebook = unique(data.table(y, y_index))

# make y word matrix
y_onehot = to_categorical(y_index)
# --------------------------------------------


# --------------------------------------------------------
# Balance the training data by resampling with inverse probability

# set up frame
balanced = data.table(y)
balanced[, freq:=.N, by='y']

# draw sample
resample_idx = sample(1:nrow(balanced), size=length(y),
										replace=TRUE, prob=1/balanced$freq)

# smaller sample for testing purposes
resample_idx = sample(1:nrow(balanced), size=5000,
										replace=TRUE, prob=1/balanced$freq)

# resample x and y
x_balanced = dtm_x[resample_idx,]
y_balanced = y_onehot[resample_idx,]
# --------------------------------------------------------


# ---------------------------------------------------------------------
# Fit model

# create model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = "relu",
			input_shape = ncol(x_balanced)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = ncol(y_balanced), activation = "softmax")

# compile model
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# fit model (note: the fit function clashes between text2vec and keras)
model %>% keras::fit(x_balanced, y_balanced, epochs=4, batch_size=512)
# ---------------------------------------------------------------------


# -------------------------------------------------------
# Save essential objects to apply classifier elsewhere

# "serialize" the model object into an R object
# (which is actually stored outside of R)
serialized_model = serialize_model(model)

# save objects
objects = c('serialized_model','vectorizer','y_codebook')
save(list=objects, file=outFile)
# -------------------------------------------------------

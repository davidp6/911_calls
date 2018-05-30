# ----------------------------------------------------------------------------
# David Phillips
#
# 5/29/2018
# Fit a neural network to the Seattle data to enable classification of other calls
# Source: https://data.seattle.gov/Public-Safety/Seattle-Police-Department-911-Incident-Response/3k2p-39jp
# The working directory should be the root of this repo
# The downloaded data should be in the root of this repo (but not committed to the remote!)
#
# This saves an rdata file with two objects: model and vectorizer
# The model contains the neural net, the vectorizer is a text2vec object to help vectorize new text
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
inFileBal = './911_Police_Calls_for_Service.csv'

# output files
graphFile = './descriptive_analysis.pdf'
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# Load/prep data

# load all files
dataSea = fread(inFileSea)
dataBal = fread(inFileBal) # doesn't have event classifications

# rename
setnames(dataSea, c('record_id', 'event_number', 'general_offense_number',
				'clearance_code', 'clearance_description', 'clearance_subgroup',
				'clearance_group', 'call_date_time', 'address', 'district',
				'zone', 'census_tract', 'longitude', 'latitude', 'location',
				'initial_type_description', 'initial_type_subgroup',
				'initial_type_group', 'at_scene_time'))
setnames(dataBal, c('record_id', 'call_date_time', 'priority', 'district',
 				'initial_type_description', 'event_number', 'address', 'location'))

# subset
x = dataSea[initial_type_group!='']$initial_type_description
y = dataSea[initial_type_group!='']$initial_type_group
unlabeled_x = c(dataSea[initial_type_group=='']$initial_type_group,
							dataBal$initial_type_description)
# ---------------------------------------------------------------


# -------------------------------------------------
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

# make unlabeled x word index based on labeled x word index
it_ux = itoken(unlabeled_x, preprocessor = tolower,
             tokenizer = word_tokenizer)

# make x word matrix
dtm_x_unlabeled = create_dtm(it_ux, vectorizer)
# -------------------------------------------------


# ------------------------------------------------------------
# Balance the training data

# resample to acheive class balance
balanced = data.table(y)
balanced[, freq:=.N, by='y']
resample_idx = sample(1:nrow(balanced), size=length(y),
										replace=TRUE, prob=1/balanced$freq)
# resample_idx = sample(1:nrow(balanced), size=10000,
#										replace=TRUE, prob=1/balanced$freq) # smaller for speed
x_balanced = dtm_x[resample_idx,]
y_balanced = y_onehot[resample_idx,]
# ------------------------------------------------------------


# ------------------------------------------------------------------------
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
model %>% keras::fit(x_balanced, y_balanced, epochs = 4, batch_size = 512)

# predict among unlabeled data
oos_preds = model %>% predict_classes(dtm_x_unlabeled)
# ------------------------------------------------------------------------


# ---------------------------------------------------
# Manually assess classification for reasonableness

# traffic
print('Calls classified as TRAFFIC RELATED CALLS:')
idx = which(oos_preds==34)
table(unlabeled_x[idx][unlabeled_x[idx]!=''])

# animals
print('Calls classified as TRAFFIC RELATED CALLS:')
idx = which(oos_preds==1)
table(unlabeled_x[idx][unlabeled_x[idx]!=''])

# assault
print('Calls classified as ASSAULTS:')
idx = which(oos_preds==2)
table(unlabeled_x[idx][unlabeled_x[idx]!=''])

# crisis
print('Calls classified as GUN CALLS:')
idx = which(oos_preds==9)
table(unlabeled_x[idx][unlabeled_x[idx]!=''])
# ---------------------------------------------------

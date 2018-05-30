# ----------------------------------------------
# David Phillips
#
# 5/17/2018
# Descriptive analysis of 911 calls in the Seattle area
# Source: https://data.seattle.gov/Public-Safety/Seattle-Police-Department-911-Incident-Response/3k2p-39jp
# The working directory should be the root of this repo
# The downloaded data should be in the root of this repo (but not committed to the remote!)
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
set.seed(1)
library(data.table)
library(text2vec)
library(keras)
library(irr)
library(ggplot2)
# --------------------


# ---------------------------------------------------------------
# Files and directories

# input file
inFileSea = './Seattle_Police_Department_911_Incident_Response.csv'

# output files
graphFile = './descriptive_analysis.pdf'
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# Load/prep data

# load all files
dataSea = fread(inFileSea)

# rename
setnames(dataSea, c('record_id', 'event_number', 'general_offense_number',
				'clearance_code', 'clearance_description', 'clearance_subgroup',
				'clearance_group', 'call_date_time', 'address', 'district',
				'zone', 'census_tract', 'longitude', 'latitude', 'location',
				'initial_type_description', 'initial_type_subgroup',
				'initial_type_group', 'at_scene_time'))

# subset
x = dataSea[initial_type_group!='']$initial_type_description
y = dataSea[initial_type_group!='']$initial_type_group

# make completely random y for the "impossible test"
# y = y[sample(1:length(y), replace=FALSE)]
# ---------------------------------------------------------------


# ------------------------------------------------------
# Vectorize text

# make train word index
prep_fun = tolower
tok_fun = word_tokenizer

it_x = itoken(x, preprocessor = prep_fun,
             tokenizer = tok_fun)
word_index = create_vocabulary(it_x)

# make train word matrix
vectorizer = vocab_vectorizer(word_index)
dtm_x = create_dtm(it_x, vectorizer)

# make y word index
y_index = as.numeric(as.factor(y))
y_codebook = unique(data.table(y, y_index))

# make y word matrix
y_onehot = to_categorical(y_index)
# ------------------------------------------------------


# ------------------------------------------------------
# Hold-outs
random = runif(length(x))
test_idx = random < .995
# test_idx = y=='SUSPICIOUS CIRCUMSTANCES' # the "inference test"
# test_idx = y %in% c('SUSPICIOUS CIRCUMSTANCES','TRAFFIC RELATED CALLS') # the "impossible inference test"
test_x = dtm_x[test_idx,]
test_y = y_onehot[test_idx,]
train_x = dtm_x[!test_idx,]
train_y = y_onehot[!test_idx,]
# ------------------------------------------------------


# ------------------------------------------------------------
# Balance the training data

# resample to acheive class balance
balanced = data.table(y[!test_idx])
balanced[, freq:=.N, by='V1']
undersample_idx = sample(1:nrow(balanced), size=nrow(train_y),
										replace=TRUE, prob=1/balanced$freq)
train_x = train_x[undersample_idx,]
train_y = train_y[undersample_idx,]
# ------------------------------------------------------------


# ------------------------------------------------------
# Fit model

# create model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = "relu",
			input_shape = ncol(train_x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = ncol(train_y), activation = "softmax")

# compile model
model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

# fit model (note: the fit function clashes between text2vec and keras)
model %>% keras::fit(train_x, train_y, epochs = 4, batch_size = 512)
oos_preds = model %>% predict_classes(test_x)
is_preds = model %>% predict_classes(train_x)
# ------------------------------------------------------


# -----------------------------------------------------------------
# Evaluate OOS PV

# concordance
mean(oos_preds==y_index[test_idx])
mean(is_preds==y_index[!test_idx][undersample_idx])

# kappa
kappa2(data.table(oos_preds, y_index[test_idx]))$value
kappa2(data.table(is_preds, y_index[!test_idx][undersample_idx]))$value

# set up to graph
test_eval = y_codebook[data.table(y_index=oos_preds), on='y_index']
setnames(test_eval, c('pred_text','pred'))
test_eval[, truth:=y[test_idx]]

# collapse
graphDataTest = test_eval[, .N, by=c('pred_text', 'truth')]
graphDataTest[, pct:=N/sum(N), by='truth']

# set up to graph
train_eval = y_codebook[data.table(y_index=is_preds), on='y_index']
setnames(train_eval, c('pred_text','pred'))
train_eval[, truth:=y[!test_idx][undersample_idx]]

# collapse
graphDataTrain = train_eval[, .N, by=c('pred_text', 'truth')]
graphDataTrain[, pct:=N/sum(N), by='truth']

# graph
ggplot(data=graphDataTest, aes(y=pred_text, x=truth, fill=pct)) +
	geom_tile() +
	labs(y='Predicted', x='Truth') +
	theme_minimal() +
	theme(axis.text.x = element_text(angle = 60, hjust = 1))
# -----------------------------------------------------------------

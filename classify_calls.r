# ----------------------------------------------------------------------------
# David Phillips
#
# 5/31/2018
# Take a neural network from the Seattle data and use it to classify unlabeled calls
# The working directory should be the root of this repo
# The downloaded data should be in the root of this repo (but not committed to the remote!)
#
# Outputs: A feather file with one data.table
# - data 			The appended data with predicted classes
# See fit_classifier.r for info on how the model was fit
# ----------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
set.seed(1)
library(data.table)
library(text2vec)
library(keras)
# --------------------


# ---------------------------------------------------------------
# Files and directories

# input files
inFileSea = './Seattle_Police_Department_911_Incident_Response.csv'
inFileBal = './911_Police_Calls_for_Service.csv'

# model file
modelFile = './model_fit.rdata'

# output files
outFile = './classified_data.rds'
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

# handle variable classes
for(v in names(dataSea)) {
	if (class(dataSea[[v]])!='character') {
		dataSea[, (v):=as.character(get(v))]
} }

# identify cities
for(c in c('Sea', 'Bal')) get(paste0('data', c))[, city:=c]

# rbind
data = rbind(dataSea, dataBal, fill=TRUE)

# store call descriptions (even the ones that already have categories)
xname = 'initial_type_description'
x = data[[xname]]
# ---------------------------------------------------------------


# ------------------------------------------
# Load the model object

# load
load(modelFile)

# "unserialize" the model object
# (to pass it back to tensorFlow)
model = unserialize_model(serialized_model)
# ------------------------------------------


# ---------------------------------------------------------------
# Vectorize text

# make x word index
it_x = itoken(x, preprocessor=tolower, tokenizer=word_tokenizer)

# make x word matrix
# (based on "vectorizer" from the labeled data to only use available words)
dtm_x = create_dtm(it_x, vectorizer)
# ---------------------------------------------------------------


# ----------------------------------------------------
# Extract predictions from model

# predict among unlabeled data
# (using Keras model object)
oos_preds = model %>% predict_classes(dtm_x)

# append to data
data[, y_index:=oos_preds]

# decode predictions
setnames(y_codebook, 'y', 'initial_type_group_predicted')
data = merge(data, y_codebook, 'y_index', all.x=TRUE)
data$y_index = NULL
# ----------------------------------------------------


# -----------------------------------------------------------
# Manually assess classification for reasonableness

# traffic
print('Calls classified as TRAFFIC RELATED CALLS:')
yname = 'initial_type_group_predicted'
t = table(data[get(yname)=='TRAFFIC RELATED CALLS'][[xname]])
t[order(t)]

# animals
print('Calls classified as ANIMAL COMPLAINTS:')
t = table(data[get(yname)=='ANIMAL COMPLAINTS'][[xname]])
t[order(t)]

# assault
print('Calls classified as ASSAULTS:')
t = table(data[get(yname)=='ASSAULTS'][[xname]])
t[order(t)]

# crisis
print('Calls classified as GUN CALLS:')
t = table(data[get(yname)=='GUN CALLS'][[xname]])
t[order(t)]
# -----------------------------------------------------------


# ---------------------
# Save results
# (as rds for space)
saveRDS(data, outFile)
# ---------------------

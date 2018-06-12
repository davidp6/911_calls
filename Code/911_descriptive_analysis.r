# --------------------------------------------------------------------
# David Phillips
#
# 5/17/2018
# Descriptive analysis of 911 calls, classified using a neural network
# Source: https://data.seattle.gov/Public-Safety/
# The working directory should be the same as the downloaded data
# Outputs:
# - A pdf file of graphs and statistics
# See fit_classifier.r and classify_calls.r for details on neural net
# --------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(tools)
library(data.table)
library(reshape2)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
# --------------------


# ---------------------------------------
# Files and directories

# input file
inFile = './Data/Cleaned Data/classified_data.rds'

# output files
graphFile = './Output/Descriptive Analysis/descriptive_analysis.pdf'
# ---------------------------------------


# ---------------------------------------------------------------
# Load/prep data

# load all files
data = readRDS(inFile)

# missing values
data[initial_type_group=='', initial_type_group:='NOT RECORDED']

# remove calls with no description
data[initial_type_description=='', initial_type_group_predicted:='NO DESCRIPTION']

# date formats
data[, c('date_str', 'time_str', 'AMPM'):=tstrsplit(at_scene_time, ' ', fixed=TRUE)]
data[is.na(date_str), c('date_str', 'time_str', 'AMPM'):=tstrsplit(call_date_time, ' ', fixed=TRUE)]
data[, call_date:=as.Date(date_str, '%m/%d/%Y')]
data[, call_month:=month(call_date)]
data[, call_year:=year(call_date)]
data[, call_my:=as.Date(paste('01',call_month,call_year), '%d %m %Y')]

# categorize according to whether initial classification is immediately threatening
clear_danger = c('CRISIS CALL' ,'CASUALTIES', 'ASSAULTS', 'PERSONS - LOST, FOUND, MISSING',
					'THREATS, HARASSMENT', 'GUN CALLS', 'HAZARDS', 'WEAPONS CALLS',
					'SEX OFFENSE (NO RAPE)', 'PERSON DOWN/INJURY', 'MENTAL CALL')
possible_danger = c('TRESPASS', 'RESIDENTIAL BURGLARIES', 'ANIMAL COMPLAINTS', 'ROBBERY',
					'RECKLESS BURNING', 'ROAD RAGE')

data[initial_type_group_predicted %in% clear_danger, initial_type_danger:='Clear Danger']
data[initial_type_group_predicted %in% possible_danger, initial_type_danger:='Possible Danger']
data[!(initial_type_group_predicted %in% clear_danger) & !(initial_type_group_predicted %in% possible_danger),
					initial_type_danger:='No Immediate Danger']
data[, initial_type_danger:=factor(initial_type_danger,
		levels=c('Clear Danger',  'Possible Danger', 'No Immediate Danger'), ordered=TRUE)]

# convert to title case (warning: slow)
# data[, initial_type_group_predicted:=toTitleCase(initial_type_group_predicted)]
# ---------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Descriptive Graphs

# 911 cals by initial group
tmpAgg = data[, .N, by=c('initial_type_danger', 'initial_type_group_predicted', 'city')]
p1 = ggplot(tmpAgg, aes(y=N, x=reorder(initial_type_group_predicted, -N), fill=initial_type_danger)) +
	geom_bar(stat='identity') +
	facet_wrap(~city, ncol=1) +
	labs(title='Initial 911 Call Category', y='Frequency', x='') +
	scale_fill_manual('', values=brewer.pal(3, 'Set1')) +
	theme_bw() +
	theme(axis.text.x=element_text(angle=315, hjust=0), plot.title=element_text(hjust=.5))

# 911 calls by day
tmpAgg = data[, .N, by='call_date']
p2 = ggplot(tmpAgg, aes(y=N, x=call_date)) +
	geom_line() +
	labs(title='911 Calls per Day', y='Frequency', x='') +
	theme_bw() +
	theme(axis.text.x=element_text(angle=315, hjust=0), plot.title=element_text(hjust=.5))

# 911 calls by month
tmpAgg = data[, .N, by='call_my']
p3 = ggplot(tmpAgg, aes(y=N, x=call_my)) +
	geom_line() +
	labs(title='911 Calls per Month', y='Frequency', x='') +
	theme_bw() +
	theme(axis.text.x=element_text(angle=315, hjust=0), plot.title=element_text(hjust=.5))

# 911 calls by initial group over time
tmpAgg = data[, .N, by=c('call_my', 'initial_type_danger')]
p4 = ggplot(tmpAgg, aes(y=N, x=call_my, color=initial_type_danger)) +
	geom_line() +
	labs(title='911 Calls per Month', y='Frequency', x='') +
	scale_color_manual('', values=brewer.pal(3, 'Set1')) +
	theme_bw() +
	theme(axis.text.x=element_text(angle=315, hjust=0), plot.title=element_text(hjust=.5))
tmpAgg[, pct:=N/sum(N), by='call_my']
p5 = ggplot(tmpAgg, aes(y=pct*100, x=call_my, fill=initial_type_danger)) +
	geom_bar(stat='identity', position='stack') +
	labs(title='911 Calls per Month', y='Percentage', x='') +
	scale_fill_manual('', values=brewer.pal(3, 'Set1')) +
	theme_bw() +
	theme(axis.text.x=element_text(angle=315, hjust=0), plot.title=element_text(hjust=.5))

# 911 calls by location

# 911 calls by location and initial group

# comparison of initial group to event clearance group

# time between call time and at-scene time

# time between call time and at-scene time by initial group

# ------------------------------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
p1
grid.arrange(p2, p3)
grid.arrange(p4, p5)
dev.off()
# --------------------------------

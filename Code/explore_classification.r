# ----------------------------------------------------------
#
# 5/31/2018
# Assess the quality of the classifications out of sample
#
# Outputs:
# - a pdf with figures
#
# The current working directory should be the root of this repo
# ----------------------------------------------------------

# to do
# print tables by city because baltimore seems to be over-classifying to causalities/guns


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(rowr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
# --------------------


# ---------------------------------------------------------------------
# Files and directories

# input files
inFile = './Data/Cleaned Data/classified_data.rds'

# an easy-viewing pdf of the top descriptions per class
graphFile = './Output/Model Diagnostics/classificiation_exploration.pdf'

# a spreadsheet of the top descriptions per class
outFile = './Output/Model Diagnostics/top_descriptions_per_class.csv'
# ---------------------------------------------------------------------


# ----------------------------------------------------------------------
# Load data
data = readRDS(inFile)

# make frequency table
freq = data[, .N, by=
	c('initial_type_group_predicted', 'initial_type_description', 'city')]

# reshape for better reading
i=1
for(g in unique(freq$initial_type_group_predicted)) {
	tmp = freq[initial_type_group_predicted==g]
	tmp = tmp[,c('city','initial_type_description','N'),with=FALSE]
	setnames(tmp, c('City', paste(g,'Description'),'Frequency'))
	tmp = tmp[order(City, -Frequency)]
	if (i==1) outTable = tmp
	if (i>1) outTable = cbind.fill(outTable, tmp, fill=NA)
	i=i+1
}
# ----------------------------------------------------------------------


# -----------------------------------------------------------
# Manually assess classification for reasonableness

# save a table
write.csv(outTable, outFile)

# open pdf
pdf(graphFile, height=6, width=9)

# print the main x descriptions in each category
x=20
yname = 'initial_type_group_predicted'
xname = 'initial_type_description'
for(c in unique(data[[yname]])) {
	t = data[get(yname)==c, .N, by=xname]
	p = t[order(-N)][1:x]
	t1 <- tableGrob(p)
	title <- textGrob(paste('Descriptions Classified as', c), gp=gpar(fontsize=16))
	padding <- unit(5,"mm")

	table <- gtable_add_rows(
	     t1,
	     heights = grobHeight(title) + padding,
	     pos = 0)
	table <- gtable_add_grob(
	    table,
	    title,
	    1, 1, 1, ncol(table))

	grid.newpage()
	grid.draw(table)
}

dev.off()
# -----------------------------------------------------------

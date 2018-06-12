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
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
# --------------------


# ----------------------------------------------
# Files and directories

# input files
inFile = './Data/Cleaned Data/classified_data.rds'

# output files
graphFile = './Output/Model Diagnostics/classificiation_exploration.pdf'
# ----------------------------------------------


# ---------------------
# Load data
data = readRDS(inFile)
# ---------------------


# -----------------------------------------------------------
# Manually assess classification for reasonableness

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

#!/usr/bin/Rscript
#Creator: Daniel Wooten
#Class STATS243
#PS3

#The first line makes this file callable from the command 
#line,assuming you've given it executable permissions
#Comments will explain how this code works

#This line loads the required libraries and packages that
# we want to use
library(scrapeR)

#This line uses scrapeR's retreival function to store
#the websites html file in an R object
orginal_html <- scrape(url='www.presidency.ucb.edu/sou.php#axzz265cEKp1a'+
, 

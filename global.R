# Two-sample t-test, v2016, r1 - global

library(broom)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)
library(shiny)
library(shinydashboard)

# Load in the data set
df.values <- dget("randomValues")

# Query the data set
findValues <- function(n = 10, s = 6, u = 0) {
  df.values[[paste(n,s,sep="_")]] + u
}


# Nice colours to use for the figures:
DarkBlue <- "#005AA5"
MidBlue <- "#7489c0"
LightBlue <- "#b3cce2"

# DarkRed <- "#C42126"
DarkRed <- "#c22227"
MidRed <- "#d67a64"
LightRed <- "#f8b3ae"

# DarkYellow <- "#F5B319"
DarkYellow <- "#f3b21a"
MidYellow <- "#f8cd7b"
LightYellow <- "#fbdfac"

DarkGreen <- "#00824A"
DarkPurple <- "#662D91" 
DarkOrange <- "#F38520"

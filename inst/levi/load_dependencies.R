

# load libs ---------------------------------------------------------------

library(shinydashboard)
library(DT)
library(tidyverse)
library(magrittr)
library(plotly)
library(ggrepel)


# source code which needs to run once -------------------------------------


source("helpers.R")


# load data ---------------------------------------------------------------

#counties <- readRDS("./data/counties.rds")

library(reactlog)
options(shiny.reactlog = TRUE)

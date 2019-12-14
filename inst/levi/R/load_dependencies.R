# load_dependencies.R #

# load libs ---------------------------------------------------------------

library(shinydashboard)
library(DT)
library(tidyverse)
library(magrittr)
library(plotly)
library(vroom)
library(janitor)
library(zeallot)
library(crosstalk)

# source code which needs to run once -------------------------------------

source("R/data_model.R")
source("R/signal_plot.R")
source("R/dashboard.R")
source("R/compare_signals.R")
source("R/helpers.R")

# load data ---------------------------------------------------------------

#counties <- readRDS("./data/counties.rds")



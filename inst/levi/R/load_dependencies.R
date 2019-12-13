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

source("R/tevi_data_model.R")
source("R/module_signal_plot.R")
source("R/module_dashboard.R")
source("R/module_compare_signals.R")


# load data ---------------------------------------------------------------

#counties <- readRDS("./data/counties.rds")



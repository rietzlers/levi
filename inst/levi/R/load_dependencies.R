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
source("R/model_params_vc.R")
source("R/sig_analysis.R")
source("R/dashboard.R")
source("R/compare_signals.R")
source("R/ui_helpers.R")
source("R/signal_vc.R")
source("R/spectrum_vc.R")

# load data ---------------------------------------------------------------

#counties <- readRDS("./data/counties.rds")



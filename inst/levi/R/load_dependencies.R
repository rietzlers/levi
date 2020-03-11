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
library(shinyBS)
library(levi)
library(readxl)
library(seewave)

# source code which needs to run once -------------------------------------
source("R/sample_specs_vc.R")
source("R/sig_analysis.R")
source("R/import_tevi_data.R")
source("R/spectrum_vc.R")
source("R/results_vc.R")
source("R/seewaveplots_vc.R")

# load data ---------------------------------------------------------------

#counties <- readRDS("./data/counties.rds")



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
source("R/surface_tension_analysis_vc.R")
source("R/load_tevi_data_vc.R")
source("R/spectrum_vc.R")
source("R/surface_tension_results_vc.R")
source("R/seewaveplots_vc.R")
source("R/report_notes_vc.R")
source("R/simulate_data_vc.R")
source("R/oscillogramly_vc.R")
# load data ---------------------------------------------------------------





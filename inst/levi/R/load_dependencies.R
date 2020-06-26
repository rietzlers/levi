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
library(broom)

# source code which needs to run once -------------------------------------
source("R/dashboard_vc.R")
source("R/sample_specs_vc.R")
source("R/load_analysis_data_vc.R")
source("R/report_notes_vc.R")


source("R/oscillogramly_vc.R")
source("R/estimate_spectrum_vc.R")
source("R/st_results_vc.R")
source("R/spectrum_vc.R")


source("R/seewaveplots_vc.R")

source("R/simulate_data_vc.R")

# load data ---------------------------------------------------------------





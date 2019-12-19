# import TeVi data files
# Author: stephan
###############################################################################

# to do !
# format of .dat-files has to be considered, e.g. transformed

tevi <- read.table("./data/CuNiSi_PF2014B/CuNiSi_PF2014B_Day4_Pb14_rad.dat",header = TRUE)
tevi$t <- tevi$Seconds - tevi[1,"Seconds"];
# importResults: script to import the viscosity- and surface-tension dataframes as produced
# by the matlab tevi-data evaluation script LevitationExperimentEvaluation
#
# Author: Stephan Rietzler
# Date: 25.03.2015
###############################################################################

setwd("/home/stephan/workspace/Projekt_Metalllegierungsschmelzen/Auswertungsskripts/r_scripts");

#####################Daten einlesen#######################
## Daten der radial-camera

# CuNiSi PF2014A
cunisi.T.melt <- 1066
cunisiA.std <- read.table("./data/CuNiSi_PF2014A/CuNiSiPF2014ADay02_std_rad.txt",header=TRUE)
cunisiA.vd <- read.table("./data/CuNiSi_PF2014A/CuNiSiPF2014ADay02_vd_rad.txt",header=TRUE)

# CuNiSi PF2014B
cunisi.T.melt <- 1066
cunisiB.std.merged <- read.table("./data/CuNiSi_PF2014B/CuNiSiPF2014BDay4_std_merged.txt",header=TRUE)
cunisiB.vd.merged <- read.table("./data/CuNiSi_PF2014B/CuNiSiPF2014BDay4_vd_merged.txt",header=TRUE)

# gTiAlTa PF2014B 
gtialta.T.melt <- 1614
gtialta.std <- read.table("./data/gTiAlTa_PF2014B/gTiAlTaPF2014B_std_rad.txt",header=TRUE)
gitalta.vd <- read.table("./data/gTiAlTa_PF2014B/gTiAlTaPF2014B_vd_rad.txt",header=TRUE)

# Zr600ppmO PF2014B
zr600o.T.melt <- 1740
zr600o.std <- read.table("./data/Zr600ppmOx_PF2014B/Zr600ppmOPF2014BDay5_std_rad.txt",header=TRUE)
zr600o.vd <- read.table("./data/Zr600ppmOx_PF2014B/Zr600ppmOPF2014BDay5_vd_rad.txt",header=TRUE)

# Zr3%atOx PF2014B
zr3ato.T.melt <- 1855;

#####################Daten aufbereiten
std.rad <- cunisiB.std.rad;
std.ax <- cunisiB.std.ax;
std.merged <- cunisiB.std.merged;

vd.rad <- cunisiB.vd.rad;
vd.ax < cunisiB.vd.ax; 
vd.merged <- cunisiB.vd.merged;

std <- std.merged;
vd <- vd.merged;

T.melt <- cunisi.T.melt;
std$Temp <- std$Temp_c - T.melt; 
vd$Temp <- vd$Temp_c - T.melt;

oscVariablesRad = c("RX","RZ","RXrot","RZrot","diffRXZ","diffRXZrot","sumRXZ","sumRXZrot")
oscVariablesAx = c("RX","RY","RXrot","RYrot","diffRXY","diffRXYrot","sumRXY","sumRXYrot")
oscVariablesMerged = c(paste(oscVariablesRad,"camRad",sep=""),paste(oscVariablesAx,"camAx",sep=""))

oscVars <- oscVariablesMerged;

std.l <- reshape(std,direction="long",varying=list(oscVars),v.names="st")
std.l <- transform(std.l,origin=oscVars[time])

vd.l <- reshape(vd,direction="long",varying=list(oscVars),v.names="visc")
vd.l <- transform(vd.l,origin=oscVars[time])
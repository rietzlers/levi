# evaluateResults:
# Author: stephan
# Date: 25.03.2015
###############################################################################

# load data
source("/home/stephan/workspace/Projekt_Metalllegierungsschmelzen/Auswertungsskripts/r_scripts/scripts/importResults.R", echo=FALSE, encoding="UTF-8")

library(lattice);

#####################################
# surface-tension
#########################################
# korrelationen 
x11();
splom(~std[2:5],data=std,groups=PBFID,auto.key=list(columns=4))
x11();
std.w <- reshape(subset(std.l,st>0 & st<5),direction="wide")
splom(~std.w[,c("Temp.1",paste("st",1:4,sep="."))],data=std.w,groups=PBFID.1,auto.key=list(columns=4))

# calc surface-tension 
std.l.sub <- subset(std.l)
std.l.sub$origin <- factor(std.l.sub$origin);

std.lm <- lm(st ~ Temp*PBFID,data=std.l.sub);
summary(std.lm);

x11(); par(mfrow=c(2,2));
plot(std.lm);

x11();
xyplot(st ~ Temp,data=std.l.sub,
		auto.key=list(columns=length(levels(std.l.sub$origin))),
		panel = function(x,y,groups,subscripts,...){
			panel.xyplot(x,y)
			panel.loess(x,y,span=1)
			panel.lmline(x,y)
		})

xyplot(st ~ Temp_c|PBFID,data=std.l.sub,
		auto.key=list(columns=length(levels(std.l.sub$origin))),
		panel = function(x,y,groups,subscripts,...){
			panel.xyplot(x,y)
			panel.loess(x,y,span=1)
			panel.lmline(x,y)
		})


###################################################
# viscosity
###################################################
# korrelationan
x11();
splom(~vd[2:9],data=vd,groups=PBFID,auto.key=list(columns=5))
vd.w <- reshape(subset(vd.l,visc>0),direction="wide")
splom(~vd.w[,c("Temp.1",paste("visc",1:4,sep="."))],data=vd.w,groups=PBFID.1,auto.key=list(columns=5))


# calc viscosity
vd.l.sub <- subset(vd.l,visc>0 & origin %in% c("RX","RZ","RXrot","RZrot"))
vd.lm <- lm(visc ~ Temp,data=vd.l.sub);
summary(vd.lm);
xyplot(visc ~ Temp|PBFID,data=vd.l.sub,
		groups=origin,auto.key=list(columns=5),
		panel = function(x,y,groups,subscripts,...){
			panel.xyplot(x,y)
			panel.lmline(x,y)
		})
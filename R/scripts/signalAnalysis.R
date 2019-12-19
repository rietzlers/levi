# calculate the periodogramm of a given timeseries
# Author: stephan
###############################################################################

sampleFreq = 200; # Hz (Bildfreq. der Radialkamera)
t <- seq(0,6,1/sampleFreq)
signal <- sin(2*pi*30*t);

t <- tevi$t;
signal = tevi$R_RadiusDiff;


s <- ts(signal,start=0,frequency=200)

x11()
par(mfrow=c(1,1))
spec <- spectrum(s)

times <- seq(min(t),max(t)-1,by=1)
f_max <- numeric(length(times))
for(i in 1:length(times)){
	X11() #dev.new()
	par(mfrow=c(2,1))
	s_w <- window(s,start=times[i],end=times[i]+1)
	plot(s_w)
	spec <- spectrum(s_w,plot=FALSE)
	f <- spec$freq#[-(1:50)]
	sp <- spec$spec#[-(1:50)]
	f_max[i] <- f[which.max(sp)]	
	plot(spec,main=paste("Max.Freq",round(f_max[i],2)))
}

graphics.off()
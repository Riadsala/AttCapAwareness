library(ggplot2)
library(dplyr)


fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("response.csv")





rDist = filter(rDat, lookedAtDist)

for (ii in 1:nrow(rDist))
{
	tr = rDist$trial[ii]
	ob = rDist$observer[ii]
	trialFix = filter(fDat, observer==ob, trial==tr)
	distFix = filter(trialFix, aoi2=="distracter")
	rDist$distDwell[ii] = sum(distFix$dur)
}



adat  = (rDist
		%>% group_by(observer, thought, congC) 
		%>% summarise(
			medianDwell=median(distDwell),
			nTrials = length(distDwell)))

write.csv( adat, "distracterDwellTimes.txt", row.names=FALSE)

plt = ggplot(rDist , aes(x=log(distDwell), fill=thought)) + geom_density(alpha=0.5)
plt = plt + scale_x_continuous("log(distracter dwell time (ms))", expand=c(0,0))
plt = plt + scale_y_continuous(expand=c(0,0.01))
plt = plt + theme_bw() + theme(legend.justification=c(1,1), legend.position=c(1,1))

ggsave("../graphs/dwellTime.pdf")
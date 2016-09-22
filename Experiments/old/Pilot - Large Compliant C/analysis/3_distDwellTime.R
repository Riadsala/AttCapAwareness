library(ggplot2)
library(dplyr)


fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("responseCapture.csv")


rDat$lookedAtTarg = FALSE
rDat$lookedAtDist = FALSE


# determine if trial included fixation to target or distracter
for (tr in 1:nrow(rDat))
{
	tfDat = fDat[which(fDat$trial==rDat$trial[tr] & fDat$observer==rDat$observer[tr]),]
	rDat$lookedAtTarg[tr] = sum(tfDat$aoi2 == "target")>0
	rDat$lookedAtDist[tr] = sum(tfDat$aoi2 == "distracter")>0
}

rDat$lookedAtTarg = as.logical(rDat$lookedAtTarg)
rDat$lookedAtDist = as.logical(rDat$lookedAtDist)


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

plt = ggplot(adat, aes(x=thought, y=medianDwell)) + geom_boxplot()
plt = plt + facet_grid(.~congC)

ggsave("dwellTime.pdf")
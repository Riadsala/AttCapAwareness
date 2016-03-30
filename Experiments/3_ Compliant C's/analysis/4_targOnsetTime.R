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


rTarg = filter(rDat, lookedAtTarg)

for (ii in 1:nrow(rTarg))
{
	tr = rTarg$trial[ii]
	ob = rTarg$observer[ii]
	trialFix = filter(fDat, observer==ob, trial==tr)
	targFix = filter(trialFix, aoi2=="target")
	rTarg$targOnset[ii] = min(targFix$onset)
}

rTarg$thoughtNoAttCap = as.factor(rTarg$thoughtNoAttCap)
levels(rTarg$thoughtNoAttCap) = c("captured", "direct")
names(rTarg)[6] = "thought"



adat  = (rTarg
		%>% group_by(observer, thought, captured) 
		%>% summarise(
			medianOnset=median(targOnset),
			nTrials = length(targOnset)))

write.csv( adat, "targetOnsetTimes.txt", row.names=FALSE)

plt = ggplot(adat, aes(x=thought, y=medianOnset, fill=captured)) + geom_boxplot()
ggsave("onsetTime.pdf")
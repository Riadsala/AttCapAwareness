library(ggplot2)
library(dplyr)
library(scales)

fDat = read.csv("aoiFixationData.csv")
rDat = read.csv("responseCapture.csv")

# look at accuracy before removing incorrect trials
accDat = aggregate(targDiscrim ~ observer + congC, rDat, "mean")
print(aggregate(targDiscrim~congC, accDat, "mean"))
print(aggregate(targDiscrim~observer, accDat, "mean"))
# get outlier person 
pAccDat = aggregate(targDiscrim~observer, accDat, "mean")
rDat = filter(rDat, observer!=pAccDat$observer[pAccDat$targDiscrim<0.8])
fDat = filter(fDat, observer!=pAccDat$observer[pAccDat$targDiscrim<0.8])

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

plt = ggplot(rDist , aes(x=distDwell, fill=thought)) + geom_density(alpha=0.5)
plt = plt + scale_x_continuous("log(distracter dwell time (ms))", expand=c(0,0), trans=log2_trans(), breaks=c(12.5, 25, 50,100, 150, 200, 300,400, 500))
plt = plt + scale_y_continuous(expand=c(0,0.01))
plt = plt + theme_bw() 
plt = plt + scale_fill_brewer(palette=3, name="response", direction=-1)

 plt = plt + coord_trans(x="log2")
plt
ggsave("../graphs/dwellTime.pdf")
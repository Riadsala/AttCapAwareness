library(tidyverse)
library(scales)


rDat = read.csv("responses.csv")

rDat$thought = as.factor(rDat$thoughtNoAttCap)
levels(rDat$thought) = c("error", "direct")
rDat$lookedAtTarg = FALSE
rDat$lookedAtDist = FALSE

fDat <- as.tibble(read.csv("fixations2.csv"))

# determine if trial included fixation to target or distracter
for (tr in 1:nrow(rDat))
{
	tfDat = fDat[which(fDat$trial==rDat$trial[tr] & fDat$observer==rDat$observer[tr]),]
	rDat$lookedAtTarg[tr] = sum(tfDat$aoi2 == "target")>0
	rDat$lookedAtDist[tr] = sum(tfDat$aoi2 == "distracter")>0
}

rDat$lookedAtTarg = as.logical(rDat$lookedAtTarg)
rDat$lookedAtDist = as.logical(rDat$lookedAtDist)


rDist = filter(rDat, lookedAtDist==TRUE)

for (ii in 1:nrow(rDist))
{
	tr = rDist$trial[ii]
	ob = rDist$observer[ii]
	trialFix = filter(fDat, observer==ob, trial==tr)
	distFix = filter(trialFix, aoi2=="distracter")
	rDist$distDwell[ii] = sum(distFix$dur)
	first_fix <- filter(trialFix, n == 1)
	rDist$initLat[ii] <- first_fix$dur + first_fix$onset
	# rDist$initDur[ii] <- first_fix$onset
}



adat  = (rDist
		%>% group_by(observer, thought) 
		%>% summarise(
			medianDwell=median(distDwell),
			nTrials = length(distDwell)))

# write.csv( adat, "distracterDwellTimes.txt", row.names=FALSE)

plt = ggplot(rDist , aes(x=distDwell, fill=thought))
plt <- plt + geom_density(alpha=0.5)
plt = plt + scale_x_continuous("distracter dwell time (ms)", expand=c(0,0), trans=log2_trans(), breaks=c(12.5, 25, 50,100, 150, 200, 300,400, 500))
 plt = plt + coord_trans(x="log2")
 plt <- plt + scale_fill_manual(values=c("grey50", "grey15"))
# plt = plt + scale_y_continuous(expand=c(0,0.01))
plt = plt + theme_bw() + theme(legend.justification=c(1,1), legend.position=c(1,1))
plt
ggsave("../graphs/dwellTime.pdf", width = 6, height = 4)
ggsave("../graphs/dwellTime.png", width = 6, height = 4)




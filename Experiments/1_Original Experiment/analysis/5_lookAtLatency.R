library(tidyverse)

fix_dat <- read_csv("fixations.csv")
trl_dat <- read_csv("responses.csv")


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



for (ii in 1:nrow(rDat))
{
	tr = rDat$trial[ii]
	ob = rDat$observer[ii]
	trialFix = filter(fDat, observer==ob, trial==tr)
	distFix = filter(trialFix, aoi2=="distracter")
	rDat$distDwell[ii] = sum(distFix$dur)
	first_fix <- filter(trialFix, n == 1)
	if (is.na(first_fix$onset) || (nrow(first_fix ) == 0)) {
		rDat$initLat[ii] = NA
	} else if (first_fix$onset < 0) {
		rDat$initLat[ii] <- first_fix$dur + first_fix$onset
	} else {
		rDat$initLat[ii] <- first_fix$dur

	}
}

# get mean sacc lat for conditions
rDat = filter(rDat, lookedAtTarg == TRUE)
mean(filter(rDat, distracter == 1, lengthOK == 1, lookedAtDist == FALSE)$initLat, na.rm=T)
mean(filter(rDat, distracter == 1, lengthOK == 0, lookedAtDist == TRUE)$initLat, na.rm=T)
mean(filter(rDat, distracter == 1, lengthOK == 0, lookedAtDist == FALSE)$initLat, na.rm=T)


sd(filter(rDat, distracter == 1, lengthOK == 1, lookedAtDist == FALSE)$initLat, na.rm=T)
sd(filter(rDat, distracter == 1, lengthOK == 0, lookedAtDist == TRUE)$initLat, na.rm=T)
sd(filter(rDat, distracter == 1, lengthOK == 0, lookedAtDist == FALSE)$initLat, na.rm=T)

mean(filter(rDat, distracter == 0, lengthOK == 1)$initLat, na.rm=T)
mean(filter(rDat, distracter == 0, lengthOK == 0)$initLat, na.rm=T)


sd(filter(rDat, distracter == 0, lengthOK == 1)$initLat, na.rm=T)
sd(filter(rDat, distracter == 0, lengthOK == 0)$initLat, na.rm=T)
# look at latency
# rDat <- filter(rDat, initLat < 500)
rDat$distracter <- as.factor(rDat$distracter)
plt = ggplot(rDat, aes(x=initLat, fill=distracter))
plt <- plt + geom_density(alpha=0.5)
plt = plt + scale_x_continuous("initial latency (ms)", expand=c(0,0))
 # plt = plt + coord_trans(x="log2")
 plt <- plt + scale_fill_manual(values=c("blue", "grey15"))
# plt = plt + scale_y_continuous(expand=c(0,0.01))
plt = plt + theme_bw() + theme(legend.justification=c(1,1), legend.position=c(1,1))
plt
library(ggplot2)
library(colorspace)
library(dplyr)
options(digits=3)

fDat = read.csv("fixations.csv")
rDat = read.csv("responses.csv")

fDat$observer = as.factor(fDat$observer)
rDat$observer = as.factor(rDat$observer)
rDat$pathLength = 0
rDat$nFix = 0

rDat$congC = as.numeric(rDat$tc == rDat$dc)
rDat$congC[which(rDat$dc==0)] = 2
rDat$congC = as.factor(rDat$congC)
levels(rDat$congC) = c('incongruent', 'congruent', 'no distracter')

rDat$distLocO = rDat$distLoc
rDat$distLoc = (rDat$distLoc + (rDat$targLoc-1)) %% 6
rDat$distLoc[which(rDat$distLoc==0)] = 6
rDat$distLoc[which(rDat$distracter==0)] = NaN
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# get circle locations - use twice as many to include ALL
# potential distracter locations
R = 256
circLabels=c('1','d1','2','d2','3','d3','4','d4','5','d5','6','d6')
cDat = data.frame(n=factor(1:12, labels=circLabels))
cDat$phi = 2*(as.numeric(cDat$n)+1)*pi/12 
cDat$x = R * -cos(cDat$phi)
cDat$y = R * sin(cDat$phi)
cDat = rbind(cDat, data.frame(n='c', phi=NA, x=0, y=0) )
circLabels=cbind(circLabels, 'c')

# first, transform (x,y) to stimuli coordinates
fDat$x = fDat$x - (1920-1024)/2 - 512
fDat$y = - (fDat$y - (1080-1024)/2 - 512)

dC = array(0, c(nrow(fDat),13))

for (n in 1:13)
{
	dC[,n] =  (fDat$y - cDat$x[n])^2 + (fDat$x - cDat$y[n])^2
}
dC[which(dC>100^2)] = NaN

# code up AOIs for fixations
fDat$aoi = 0
fDat$aoi2 = "blank"
for (f in 1:nrow(fDat))
{
	if (sum(is.finite(dC[f,]))>0) 	{
		fDat$aoi[f] = circLabels[which(dC[f,]== min(dC[f,], na.rm=T))]
	} else {
		
	}
	# now code into centre/target/distracter/na code
	tr = fDat$trial[f]
	obs = fDat$observer[f]
	trialDat = filter(rDat, trial==tr, observer==obs)
	if (nrow(trialDat)>0)
	{
		target = trialDat$targLoc
		distracter = trialDat$distLoc
		if (fDat$aoi[f] == "c")
		{
			fDat$aoi2[f] = "centre"
		} else if (fDat$aoi[f] == as.character(target))
		{
			fDat$aoi2[f] = "target"
		} else if (fDat$aoi[f] == as.character(paste("d", distracter, sep="")))
		{
			fDat$aoi2[f] = "distracter"
		} else {
			fDat$aoi2[f] = "blank"
		}
	}
}
fDat$aoi = as.factor(fDat$aoi)
fDat$aoi2 = as.factor(fDat$aoi2)


#  get total path length for each trials
for (tr in 1:nrow(rDat))
{
	fix = fDat[which((fDat$trial==rDat$trial[tr]) & (fDat$observer==rDat$observer[tr])),]
	rDat$nFix[tr] = nrow(fix)
	if (nrow(fix)>1)
	{
		amp = sqrt((fix$x[1])^2+(fix$y[1])^2)
		for (f in 2:nrow(fix))
		{
			amp = sqrt((fix$x[f]-fix$x[f-1])^2+(fix$y[f]-fix$y[f-1])^2)
			rDat$pathLength[tr] = rDat$pathLength[tr] + amp
		}
		rm(amp)
	}
	else
	{
		rDat$pathLength[tr] = NaN
	}
}

# determine if trial included fixation to target or distracter
for (tr in 1:nrow(rDat))
{
	tfDat = fDat[which(fDat$trial==rDat$trial[tr] & fDat$observer==rDat$observer[tr]),]
	rDat$lookedAtTarg[tr] = sum(tfDat$aoi2 == "target")>0
	rDat$lookedAtDist[tr] = sum(tfDat$aoi2 == "distracter")>0
}
rDat$lookedAtTarg = as.logical(rDat$lookedAtTarg)
rDat$lookedAtDist = as.logical(rDat$lookedAtDist)

#  normalise path length
rDat$pathLength = rDat$pathLength/256
rDat$lengthOK = as.factor(abs(rDat$pathLength - 1) < 0.2)

#  classify trial
rDat$type = 'error'
rDat$type[as.logical(rDat$lengthOK) & (rDat$lookedAtTarg==TRUE)] = "direct"
rDat$type = as.factor(rDat$type)

rDat$thoughtNoAttCap = as.factor(rDat$thoughtNoAttCap)
levels(rDat$thoughtNoAttCap) = c("no", "yes")
names(rDat)[6] = "thought"

# reorder factor levels
rDat$congC = factor(rDat$congC, levels=levels(rDat$congC)[c(3,1,2)])	

# calculate dwell time
for (ii in 1:nrow(rDat))
{
	tr = rDat$trial[ii]
	ob = rDat$observer[ii]
	trialFix = filter(fDat, observer==ob, trial==tr)
	distFix = filter(trialFix, aoi2=="distracter")
	rDat$distDwell[ii] = sum(distFix$dur)
}

write.csv(fDat, "aoiFixationData.csv", row.names=FALSE)
write.csv(rDat, "responseCapture.csv", row.names=FALSE)
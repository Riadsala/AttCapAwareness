library(tidyverse)

options(digits=3)
fDat = read.csv("fixations.csv")
rDat = read.csv("responses.csv")

rDat$distLocO = rDat$distLoc
rDat$distLoc = (rDat$distLoc + (rDat$targLoc-1)) %% 6
rDat$distLoc[which(rDat$distLoc==0)] = 6
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# get circle locations - use twice as many to include ALL
# potential distracter locations

R = 256
circLabels=c('1','d1','2','d2','3','d3','4','d4','5','d5','6','d6')
cDat = data.frame(n=factor(1:12, labels=circLabels))
cDat$phi = 2*(as.numeric(cDat$n)+1)*pi/12 
cDat$x = R * -cos(cDat$phi)
cDat$y = R * sin(cDat$phi)
cDat = rbind(cDat, data.frame(n='c', phi=NA, x=0, y=0))
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


for (f in 1:nrow(fDat))
{
	if (sum(is.finite(dC[f,]))>0)
	{
		fDat$aoi[f] = circLabels[which(dC[f,]== min(dC[f,], na.rm=T))]
	}
	else
	{
		fDat$aoi[f] = 0
	}
	# now code into centre/target/distracter/na code
	tr = fDat$trial[f]
	obs = fDat$observer[f]
	trialDat = filter(rDat, trial==tr, observer==obs)
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
fDat$aoi = as.factor(fDat$aoi)
fDat$aoi2 = as.factor(fDat$aoi2)

rDat$lookedAtTarg = FALSE
rDat$lookedAtDist = FALSE
for (tr in 1:nrow(rDat))
{
	tfDat = fDat[which(fDat$trial==rDat$trial[tr] & fDat$observer==rDat$observer[tr]),]
	rDat$lookedAtTarg[tr] = sum(tfDat$aoi2 == "target")>0
	rDat$lookedAtDist[tr] = sum(tfDat$aoi2 == "distracter")>0
}

rDat$lookedAtTarg = as.factor(rDat$lookedAtTarg)
rDat$lookedAtDist = as.factor(rDat$lookedAtDist)


# for (tr in 1:30)#nrow(rDat)
# {
# 	tfDat = fDat[which(fDat$trial==rDat$trial[tr] & fDat$observer==rDat$observer[tr]),]
#  	cDat$target = 0
#  	cDat$target[which(cDat$n==rDat$targLoc[tr])] = 1
#  	cDat$target[which(cDat$n==paste("d",rDat$distLoc[tr], sep=""))] = 1
#  	cDat$target = as.factor(cDat$target)
# 	plt = ggplot() + geom_path(data=tfDat, aes(x=x, y=y)) + geom_text(data=tfDat, aes(x=x, y=y, label=aoi2))#
# 	plt = plt + geom_text(aes(x=cDat$y,y=cDat$x, label=cDat$n, colour=cDat$target))
# 	ggsave(paste("trial", tr, ".png", sep=""))

# }


#  classify trials based on path length
rDat$type = 'error'
rDat$type[as.logical(rDat$lengthOK) & (rDat$lookedAtTarg==TRUE)] = "direct"
# rDat$type[rDat$pathLength>1.2] = "tooLong"
# rDat$type[rDat$lookedAtDist==TRUE] = "fixatedDistracter"

rDat$type = as.factor(rDat$type)



write.csv(rDat, "responses2.csv")
write.csv(fDat, "fixations2.csv")
library(ggplot2)
options(digits=3)
library(dplyr)
fDat = read.csv("fixations.csv")
rDat = read.csv("responses.csv")

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

# ggplot(fDat, aes(x=x, y=y,colour=trial)) + geom_path()

dC = array(0, c(nrow(fDat),13))

for (n in 1:13)
{
	dC[,n] =  (fDat$y - cDat$x[n])^2 + (fDat$x - cDat$y[n])^2
}

dC[which(dC>100^2)] = NaN


for (f in 1:nrow(fDat))
{
	if (sum(is.finite(dC[f,]))==1)
	{
		fDat$aoi[f] = circLabels[which(dC[f,]== min(dC[f,], na.rm=T))]
		
		# now code into centre/target/distracter/na code
		tr = fDat$trial[f]
		obs = fDat$observer[f]
		trialDat = filter(rDat, trial==tr, observer==obs)
		if (nrow(trialDat)==1)
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
				fDat$aoi2[f] = "nope"
			}
		}
	}
	else
	{
		fDat$aoi[f] = NA
		fDat$aoi2[f] = "na"
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




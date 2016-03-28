library(ggplot2)
library(colorspace)
library(dplyr)
options(digits=3)
fDat = read.csv("fixations.csv")
rDat = read.csv("responses.csv")
fDat$observer = as.factor(fDat$observer)
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

# code up AOIs for fixations
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


# determine if trial included fixation to target or distracter
for (tr in 1:nrow(rDat))
{
	tfDat = fDat[which(fDat$trial==rDat$trial[tr] & fDat$observer==rDat$observer[tr]),]
	rDat$lookedAtTarg[tr] = sum(tfDat$aoi2 == "target")>0
	rDat$lookedAtDist[tr] = sum(tfDat$aoi2 == "distracter")>0
}

rDat$lookedAtTarg = as.logical(rDat$lookedAtTarg)
rDat$lookedAtDist = as.logical(rDat$lookedAtDist)

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


dat = rDat
rm(rDat)

# remove incorrect trials
dat = filter(dat, targDiscrim==1)

#  normalise path length
dat$pathLength = dat$pathLength/256
dat$lengthOK = as.factor(abs(dat$pathLength - 1) < 0.2)

#  classify trial
dat$captured = 'none'
dat$captured[as.logical(dat$lengthOK) & (dat$lookedAtTarg==TRUE)] = "direct"
dat$captured[dat$pathLength>1.2] = "tooLong"
dat$captured[dat$lookedAtDist==TRUE] = "fixatedDistracter"

dat$captured = as.factor(dat$captured)

dat$thoughtNoAttCap = as.factor(dat$thoughtNoAttCap)
levels(dat$thoughtNoAttCap) = c("bad", "good")

plt = ggplot(filter(dat, distracter==1), aes(x=captured, fill=thoughtNoAttCap)) + geom_bar(stat="count") + facet_grid(~observer)
plt = plt + theme_bw() + scale_y_continuous(name="number of trials") + scale_x_discrete(name=" ")
plt = plt + theme(legend.position="top") + scale_fill_discrete(name="responded that the trial was:")
plt = plt + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggsave("../graphs/capturedAndThoughtA.pdf", width=10, height=5)
ggsave("../graphs/capturedAndThoughtA.png", width=10, height=5)

plt = ggplot(filter(dat, distracter==0), aes(x=captured, fill=thoughtNoAttCap)) + geom_bar(stat="count") + facet_grid(~observer)
plt = plt + theme_bw() + scale_y_continuous(name="number of trials") + scale_x_discrete(name=" ")
plt = plt + theme(legend.position="top") + scale_fill_discrete(name="responded that the trial was:")
plt = plt + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggsave("../graphs/capturedAndThoughtAnodist.pdf", width=10, height=5)
ggsave("../graphs/capturedAndThoughtAnodist.png", width=10, height=5)





#  remove outlier RTs!
dat = filter(dat, RT>0.1, RT<4)
#  only include distracter present trials 
dat = filter(dat, distracter==1)


dat$thoughtNoAttCap = as.factor(dat$thoughtNoAttCap)
levels(dat$thoughtNoAttCap) = c("captured", "direct")
names(dat)[6] = "thought"



adat  = (dat
		%>% group_by(observer, captured, thought, congC) 
		%>% summarise(
			medianRT=median(RT),
			nTrials = length(RT)))

adat = aggregate(RT ~observer+captured+congC+thought, filter(dat,congC!="no distracter"), "median")
icdat = select(filter(adat, congC=="incongruent"), observer, captured, thought, RT)
names(icdat)[4] = "incongruent_RT"
ccdat = select(filter(adat, congC=="congruent"), observer, captured, thought, RT)
names(ccdat)[4] = "congruent_RT"
compDat = merge(ccdat, icdat)
rm(icdat, ccdat)
compDat$RT_diff = compDat$incongruent_RT - compDat$congruent_RT 


adat2  = (compDat 
		%>% group_by(captured, thought) 
		%>% summarise(
			meanRTdiff=mean(RT_diff), 
			nPeople=length(RT_diff),
			stder =sd(RT_diff)/sqrt(nPeople),
			lower = meanRTdiff-1.96*stder,
			upper = meanRTdiff+1.96*stder))

agPlt = ggplot(adat2, aes(x=captured, y=meanRTdiff, ymin=lower, ymax=upper, colour=thought))
agPlt = agPlt + geom_point() + geom_errorbar()
agPlt = agPlt + theme_light() + scale_y_continuous("mean incongruent - congruent RT")
agPlt = agPlt + scale_colour_brewer(palette = "Set1")
ggsave("meanRT.pdf", width=5, height=5)


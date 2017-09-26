library(ggplot2)
library(dplyr)
library(colorspace)
options(digits=3)


# get author data
fDatA = read.csv("../authorsData/fixations.csv")
fDatA$observer = factor(fDatA$observer, labels=c('A', 'B'))
fDatA$observer = as.character(fDatA$observer)
rDatA = read.csv("../authorsData/responses.csv")
rDatA$observer = factor(rDatA$observer, labels=c('A', 'B'))
rDatA$observer = as.character(rDatA$observer)

fDatB = read.csv("fixations.csv")
rDatB = read.csv("responses.csv")
fDatB$observer = factor(fDatB$observer, labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
rDatB$observer = factor(rDatB$observer, labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

fDat = rbind(fDatA, fDatB)
rDat = rbind(rDatA, rDatB)
fDat$observer = as.factor(fDat$observer)
rDat$observer = as.factor(rDat$observer)

rDat$pathLength = 0

rDat$nFix = 0

rDat$distLocO = rDat$distLoc
rDat$distLoc = (rDat$distLoc + (rDat$targLoc-1)) %% 6
rDat$distLoc[which(rDat$distLoc==0)] = 6

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
	rDat$latToTarg[tr] = min(filter(trDat, aoi2 = target)$)
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


#  normalise path length
dat$pathLength = dat$pathLength/256
dat$lengthOK = as.factor(abs(dat$pathLength - 1) < 0.2)

#  classify trial
dat$type = 'error'
dat$type[as.logical(dat$lengthOK) & (dat$lookedAtTarg==TRUE)] = "direct"
# dat$type[dat$pathLength>1.2] = "tooLong"
# dat$type[dat$lookedAtDist==TRUE] = "fixatedDistracter"

dat$type = as.factor(dat$type)

dat$thoughtNoAttCap = as.factor(dat$thoughtNoAttCap)
levels(dat$thoughtNoAttCap) = c("no", "yes")
dat$thoughtNoAttCap = factor(dat$thoughtNoAttCap, levels(dat$thoughtNoAttCap)[c(2,1)])
# reorder factor level
 # dat$thoughtNoAttCap= factor(dat$thoughtNoAttCap,levels(dat$thoughtNoAttCap)[c()])
levels(dat$observer) = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "A", "B")

# plot basic results for distracter trials. 
dat = filter(dat, distracter==1)

plt = ggplot(dat, aes(x=type, fill=thoughtNoAttCap)) + geom_bar(stat="count") + facet_grid(~observer)
plt = plt + theme_bw() + scale_y_continuous(name="number of trials") + scale_x_discrete(name=" ")
plt = plt + theme(legend.position="top") 
plt = plt + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plt = plt + scale_fill_brewer(type="seq", palette=3, name="response:", direction=1)
ggsave("../graphs/capturedAndThoughtA.pdf", width=10, height=4)
ggsave("../graphs/capturedAndThoughtA.png", width=10, height=4)

# calculate prec and recall for each person
dat_pr = data.frame(person=character(), stat=factor(levels=c('accuracy', 'precision', 'rec', "F1")), val=numeric())
dat_error_recall =data.frame(person=character(), errorRate=numeric(), recall=numeric())

for (person in levels(dat$observer))
{
	pdat = dat[which(dat$observer==person),]
	prec = sum(pdat$type=="error" & pdat$thoughtNoAttCap=="no")/sum(pdat$thoughtNoAttCap=="no")
	recall = sum(pdat$type=="error" & pdat$thoughtNoAttCap=="no")/sum(pdat$type=="error")
	acc = mean((pdat$type=="error") == (pdat$thoughtNoAttCap==FALSE))
	
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="precision", val=prec))
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="recall", val=recall))
	# dat_pr = rbind(dat_pr, data.frame(person=person, stat="F1", val=2*prec*recall/(prec+recall)))
	dat_pr= rbind(dat_pr, data.frame(person=person, stat="accuracy", val=acc))

	dat_error_recall = rbind(dat_error_recall, data.frame(person=person, errorRate = mean(filter(pdat, distracter==1)$type=="error"), recall=recall))
}

dat_pr$stat = factor(dat_pr$stat, levels=c('accuracy', 'precision', 'recall', "F1"))

plt = ggplot(dat_pr, aes(x=stat, y=val)) + geom_boxplot(fill="grey")
plt = plt + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
ggsave("../graphs/f1score.pdf", height=5, width=5)
ggsave("../graphs/f1score.png", height=5, width=5)

plt = ggplot(dat_error_recall, aes(x=1-errorRate, y=recall))
plt = plt + geom_point() #+ geom_smooth(method=lm, colour="black")
plt = plt + theme_bw() + scale_x_continuous(name="proportion of distracter trials with direct eye movements")
ggsave("../graphs/recall_corr.png", height=5, width=5)
ggsave("../graphs/recall_corr.pdf", height=5, width=5)
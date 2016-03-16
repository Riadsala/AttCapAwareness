library(ggplot2)
library(colorspace)
library(BayesFactor)
options(digits=3)
fDat = read.csv("fixations.csv")
rDat = read.csv("responses.csv")
fDat$observer = as.factor(fDat$observer)
rDat$pathLength = 0
rDat$nFix = 0

colours <- rainbow_hcl(4, start = 90, end = -30)

for (tr in 1:nrow(rDat))
{
	fix = fDat[which((fDat$trial==rDat$trial[tr]) & (fDat$observer==rDat$observer[tr])),]
	rDat$nFix[tr] = nrow(fix)
	if (nrow(fix)>1)
	{
		rDat$pathLength[tr] = 0
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
	rm(fix)
}

rDat$congC = as.numeric(rDat$tc == rDat$dc)
rDat$congC[which(rDat$dc==0)] = 2
rDat$congC = as.factor(rDat$congC)
levels(rDat$congC) = c('incongruent', 'congruent', 'no distracter')


aggregate(data=rDat, targDiscrim ~ observer + distracter + congC, FUN=mean)
aggregate(data=rDat, thoughtNoAttCap ~ observer + distracter + congC, FUN=mean)
aggregate(data=rDat, nFix ~ observer + distracter, FUN=mean)

dat = rDat[-which(rDat$nFix<2),]
#dat = dat[-which(dat$distracter==0 & dat$thoughtNoAttCap==0),]

# ggplot(dat, aes(x=(pathLength/256), fill=thoughtNoAttCap)) + geom_histogram(binwidth = 0.25) + facet_grid(observer~distracter)
# ggsave("../graphs/pathLengthComp.pdf")

# now classify by pathlength
dat$pathLength = dat$pathLength/256
dat$captured = (abs(dat$pathLength - 1) > 0.2)

 pltPL = ggplot(dat, aes(x=pathLength)) + geom_density()
 pltPL = pltPL + facet_grid(.~observer)
 pltPL


aggregate(data=dat, thoughtNoAttCap ~ captured + observer + congC, FUN=mean)

levels(dat$observer)=as.character(seq(1,length(levels(dat$observer))))

dat$captured = as.factor(dat$captured)
levels(dat$captured) = c("good", "bad")


levels(dat$observer)=as.character(seq(1,10))
 dat$thoughtNoAttCap = as.factor(dat$thoughtNoAttCap)
 levels(dat$thoughtNoAttCap) = c("bad", "good")

plt = ggplot(dat, aes(x=captured, fill=thoughtNoAttCap)) 
plt = plt + geom_bar(stat="count") + facet_grid(congC~observer)
plt = plt + theme_bw() 
plt = plt + scale_y_continuous(name="number of trials", limits=c(0,230), breaks=c(0,115, 230), labels=c("0%", "20%", "40%")) + scale_x_discrete(name=" ")
plt = plt + theme(legend.position="top") + scale_fill_discrete(name="responded that the trial was:")
# plt = plt + scale_fill_manual(values=colours)
ggsave("../graphs/capturedAndThoughtA.pdf", width=12, height=5)
ggsave("../graphs/capturedAndThoughtA.png", width=12, height=4)


aggregate(data=rDat, targDiscrim ~ observer, FUN=mean)



library(Hmisc)
pointEst = binconf(aggregate(data=dat, as.numeric(thoughtNoAttCap)-1 ~ captured + observer, FUN=sum)[,3], aggregate(data=dat, as.numeric(thoughtNoAttCap)-1 ~ captured + observer, FUN=length)[,3])
a = aggregate(data=dat, as.numeric(thoughtNoAttCap)-1 ~ captured + observer, FUN=mean)
# dat2 = data.frame(observer=a[,2], captured=a[,1], thoughtGoodTrial=a[,3], lower=pointEst[,2], upper=pointEst[,3])


# plt = ggplot(dat2, aes(x=captured, y=thoughtGoodTrial)) + geom_point(stat="identity") + facet_grid(~observer)
# plt = plt + geom_errorbar(aes(ymin=lower, ymax=upper)) + scale_fill_manual(values=colours)Ï
# plt = plt + theme_bw() + scale_y_continuous(name="responded that the trial was good")
# ggsave("../graphs/capturedAndThoughtB.pdf", width=10, height=5)
# ggsave("../graphs/capturedAndThoughtB.png", width=10, height=5)


# calculate prec and recall for each person
dat_pr = data.frame(person=character(), stat=factor(levels=c('accuracy', 'precision', 'rec')), val=numeric())

for (person in levels(dat$observer))
{
	pdat = dat[which(dat$observer==person),]
	prec = sum(pdat$captured=="bad" & pdat$thoughtNoAttCap=="bad")/sum(pdat$thoughtNoAttCap=="bad")
	recall = sum(pdat$captured=="bad" & pdat$thoughtNoAttCap=="bad")/sum(pdat$captured=="bad")
	acc = mean((pdat$captured=="bad") == (pdat$thoughtNoAttCap==FALSE))
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="precision", val=prec))
	dat_pr = rbind(dat_pr, data.frame(person=person, stat="recall", val=recall))
	# dat_pr = rbind(dat_pr, data.frame(person=person, stat="f1", val=2*prec*recall/(prec+recall)))
	dat_pr= rbind(dat_pr, data.frame(person=person, stat="accuracy", val=acc))
}

dat_pr$stat = factor(dat_pr$stat, levels=c('accuracy', 'precision', 'recall'))

plt = ggplot(dat_pr, aes(x=stat, y=val, fill=stat)) + geom_boxplot() + scale_fill_manual(values=colours)
plt = plt + theme_bw() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position="none")
ggsave("../graphs/f1score.pdf", height=5, width=5)
ggsave("../graphs/f1score.png", height=5, width=5)


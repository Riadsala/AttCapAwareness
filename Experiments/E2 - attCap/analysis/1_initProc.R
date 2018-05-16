
library(dplyr)
options(digits=3)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

source("../../ProcessASC.R")

people = as.character()
for (e in 1:6)
{
	for (p in 1:5)
	{
		people = c(people, paste(e, "_", p, sep=""))
	}
}
# add the data from the level three students
people = c(people, "99_1", "99_2", "99_3", "99_4", "99_5", "99_6")


rDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), targDiscrim=numeric(), thoughtNoAttCap=numeric())
fDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), x=numeric(), y=numeric(), n=numeric())
for (person in people)
{
	print(person)

	dat = read.csv(paste("../data/txt_files/obs", person, ".txt", sep=""))
	# get count of practise trial and remove
	if (person !="99_1")
	{
		pTrial = dat$trial[20]
		dat = dat[21:nrow(dat),]
	}
	dat$observer = person
	rDat = rbind(rDat, dat)
	rm(dat)

	asc = readLines(paste("../data//asc_files/acap", person, ".asc", sep=""))
	asc = strsplit(asc, "\t")
	dat = ProcessASC(asc)
	# remove practice trials 
	if (person !="99_1")
	{
		dat$trial = dat$trial-pTrial
		dat = filter(dat, trial>0)
	}
	fDat = rbind(fDat, dat)
	rm(dat)
}


rDat$observer = as.factor(rDat$observer)
# sort out missing targDiscrim data
rDat$targDiscrim[which(rDat$targDiscrim==-1)] = NaN
rDat$thoughtNoAttCap[which(rDat$thoughtNoAttCap==-1)]=0
rDat$thoughtNoAttCap = as.logical(rDat$thoughtNoAttCap)
rDat$distracter = 1
rDat$distracter[which(rDat$distLoc==0)] = 0

format(aggregate(data=rDat, thoughtNoAttCap~observer+distracter, FUN=mean),ndigt=3)

# sort out distLoc
rDat$distLocRel = (as.numeric(rDat$distLoc) - (as.numeric(rDat$targLoc)-1)) %% 6
rDat$distLocRel[which(rDat$distLoc==0)]=1
rDat$distLocRel[which(rDat$distLoc==5)]=2
rDat$distLocRel[which(rDat$distLoc==4)]=3
rDat$distLocRel[which(rDat$distracter==0)]=0

aggregate(data=rDat, thoughtNoAttCap~observer+distLoc, FUN="mean")


write.csv(fDat, "fixations.csv", row.names=F, quote=F)
write.csv(rDat, "responses.csv", row.names=F, quote=F)
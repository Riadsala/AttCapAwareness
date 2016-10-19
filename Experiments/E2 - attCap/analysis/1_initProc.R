
library(dplyr)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

ProcessASC <- function(asc)
{
	fixDat =  data.frame(observer=numeric(), trial=numeric(), n=numeric(), x=numeric(), y=numeric(), dur=numeric())

	trialStarts = grep("TRIAL_START[0-9]*", asc)
	trialEnds   = grep("TRIAL_END[0-9]*", asc)
	nTrials = length(trialStarts)

	for (t in 1:nTrials)
	{
		if (is.finite(trialEnds[t]))
		{	
			trial = asc[trialStarts[t]:trialEnds[t]]
			fixationLines = grep("EFIX", trial)
			
			stimOffLine = trial[grep("STIM_OFF", trial)]
			stimOffTime = as.numeric(strsplit(unlist(stimOffLine)[2], " ")[[1]][1])
			if (length(fixationLines)>0)
			{
				fixations = as.data.frame(matrix(unlist(trial[fixationLines]), byrow=T, ncol=6))
				
				#  get onset times (annoying)
				idx =grep("[0-9]+", unlist(strsplit(as.character(fixations$V1), " ")))
				onsetTimes = as.numeric(unlist(strsplit(as.character(fixations$V1), " "))[idx])


				trialDat = data.frame(
					observer=person, 
					trial=t, 
					x=as.numeric.factor(fixations$V4), 
					y=as.numeric.factor(fixations$V5), 
					dur=as.numeric.factor(fixations$V3),
					onset = onsetTimes, 
					offset=as.numeric.factor(fixations$V2))

						
				# convert to stimulus coordinates				 		
				trialDat$n = 1:length(trialDat$x)
				# remove fixations with onset post stim_off
				trialDat = filter(trialDat, onset<stimOffTime)
			    # set onset times relative to first fixations
				trialDat$onset = trialDat$onset - trialDat$onset[1] 

				 fixDat = rbind(fixDat, trialDat)
				 #rm(trialDat)
			}
		}
	}
	return(fixDat)
} 

people = as.character()
for (e in 1:6)
{
	for (p in 1:5)
	{
		people = c(people, paste(e, "_", p, sep=""))
	}
}


options(digits=3)
rDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), targDiscrim=numeric(), thoughtNoAttCap=numeric())
fDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), x=numeric(), y=numeric(), n=numeric())
for (person in people)
{
	print(person)

	dat = read.csv(paste("../data/txt_files/obs", person, ".txt", sep=""))
	# get count of practise trial
	if (person !="99_1")
	{
		pTrial = dat$trial[20]
	}
	dat$observer = person
	rDat = rbind(rDat, dat)
	rm(dat)

	asc = readLines(paste("../data//asc_files/acap", person, ".asc", sep=""))
	asc = strsplit(asc, "\t")
	dat = ProcessASC(asc)
	# remove practice trials from observer 2-6
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
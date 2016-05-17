as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

ProcessASC <- function(asc)
{
	fixDat =  data.frame(observer=numeric(), trial=numeric(), n=numeric(), x=numeric(), y=numeric(), dur=numeric())

	trialStarts = grep("TRIAL_START[0-9]*", asc)
	trialEnds   = grep("STIM_OFF[0-9]*", asc)
	nTrials = length(trialStarts)

	for (t in 1:nTrials)
	{
		if (is.finite(trialEnds[t]))
		{	
			trial = asc[trialStarts[t]:trialEnds[t]]
			fixationLines = grep("EFIX", trial)
			
			if (length(fixationLines)>0)
			{
				fixations = as.data.frame(matrix(unlist(trial[fixationLines]), byrow=T, ncol=6))

				trialDat = data.frame(
					observer=person, 
					trial=t, 
					x=as.numeric.factor(fixations$V4), y=as.numeric.factor(fixations$V5), dur=as.numeric.factor(fixations$V3), onset=as.numeric.factor(fixations$V2))

				# convert to stimulus coordinates				 		
				 trialDat$n = 1:length(trialDat$x)
			
			    # set onset times relative to first fixations
				trialDat$onset = trialDat$onset - trialDat$onset[1] 

				 fixDat = rbind(fixDat, trialDat)
				 #rm(trialDat)
			}
		}
	}
	return(fixDat)
} 

people = c(1,2,3,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,21)

options(digits=3)
rDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), targDiscrim=numeric(), thoughtNoAttCap=numeric())
fDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), x=numeric(), y=numeric(), n=numeric())
for (person in people)
{
	print(person)

	dat = read.csv(paste("../results/obs", person, ".txt", sep=""))
	rDat = rbind(rDat, dat)
	rm(dat)

	asc = readLines(paste("../results/attcap", person, ".asc", sep=""))
	asc = strsplit(asc, "\t")
	dat = ProcessASC(asc)
	fDat = rbind(fDat, dat)
	rm(dat)
}

# sort out missing targDiscrim data
rDat$targDiscrim[which(rDat$targDiscrim==-1)] = NaN
rDat$thoughtNoAttCap[which(rDat$thoughtNoAttCap==-1)]=0
rDat$thoughtNoAttCap = as.logical(rDat$thoughtNoAttCap)
rDat$distracter = 1
rDat$distracter[which(rDat$distLoc==0)] = 0

format(aggregate(data=rDat, thoughtNoAttCap~observer+distracter, FUN=mean),ndigt=3)

# sort out distLoc
rDat$distLocRel = (rDat$distLoc - (rDat$targLoc-1)) %% 6
rDat$distLocRel[which(rDat$distLoc==0)]=1
rDat$distLocRel[which(rDat$distLoc==5)]=2
rDat$distLocRel[which(rDat$distLoc==4)]=3
rDat$distLocRel[which(rDat$distracter==0)]=0

aggregate(data=rDat, thoughtNoAttCap~observer+distLoc, FUN=mean)

write.csv(fDat, "fixations.csv", row.names=F, quote=F)
write.csv(rDat, "responses.csv", row.names=F, quote=F)
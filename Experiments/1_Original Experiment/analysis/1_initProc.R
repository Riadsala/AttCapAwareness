
library(dplyr)
options(digits=3)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
source("../../ProcessASC.R")

people = c(19,20,21,22,23,25,26,27,30, 34)

rDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), targDiscrim=numeric(), thoughtNoAttCap=numeric())
fDat = data.frame(observer=numeric(), trial=numeric(), targLoc=numeric(), distLoc=numeric(), onset=numeric(), x=numeric(), y=numeric(), n=numeric())
for (person in people)
{
	dat = read.csv(paste("../results/obs", person, ".txt", sep=""))
	rDat = rbind(rDat, dat)
	rm(dat)

	asc = readLines(paste("../results/attcap", person, ".asc", sep=""))
	asc = strsplit(asc, "\t")
	dat = ProcessASC(asc)
	fDat = rbind(fDat, dat)
	rm(dat)
}

# now process author's data
people = c(1, 2)
authors = c("A", "B")
for (person in people)
{
	dat = read.csv(paste("../authorsData/obs", person, ".txt", sep=""))
	dat$observer = authors[person]
	rDat = rbind(rDat, dat)
	rm(dat)

	asc = readLines(paste("../authorsData/attcap", person, ".asc", sep=""))
	asc = strsplit(asc, "\t")
	dat = ProcessASC(asc)
	dat$observer = authors[person]
	fDat = rbind(fDat, dat)
	rm(dat)
}


# sort out missing targDiscrim data
rDat$targDiscrim[which(rDat$targDiscrim==-1)] = NaN
rDat$thoughtNoAttCap[which(rDat$thoughtNoAttCap==-1)]=0
rDat$thoughtNoAttCap = as.logical(rDat$thoughtNoAttCap)
rDat$distracter = 1
rDat$distracter[which(rDat$distLoc==0)] = 0

# format(aggregate(data=rDat, thoughtNoAttCap~observer+distracter, FUN=mean),ndigt=3)

# sort out distLoc relative! doing long way to decrase chance of bugs
# can't be bothered being clever :(

rDat$distdist = NaN

targ_loc = 1
dist_dist = 1
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(6, 1))
rDat$distdist[idx] = dist_dist

dist_dist = 2
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(5, 2))
rDat$distdist[idx] = dist_dist

dist_dist = 3
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(4, 3))
rDat$distdist[idx] = dist_dist

targ_loc = 2
dist_dist = 1
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(1, 2))
rDat$distdist[idx] = dist_dist

dist_dist = 2
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(6, 3))
rDat$distdist[idx] = dist_dist

dist_dist = 3
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(5, 4))
rDat$distdist[idx] = dist_dist

targ_loc = 3
dist_dist = 1
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(2, 3))
rDat$distdist[idx] = dist_dist

dist_dist = 2
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(1, 4))
rDat$distdist[idx] = dist_dist

dist_dist = 3
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(6, 5))
rDat$distdist[idx] = dist_dist

targ_loc = 4
dist_dist = 1
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(3, 4))
rDat$distdist[idx] = dist_dist

dist_dist = 2
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(2, 5))
rDat$distdist[idx] = dist_dist

dist_dist = 3
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(1, 6))
rDat$distdist[idx] = dist_dist


targ_loc = 5
dist_dist = 1
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(4, 5))
rDat$distdist[idx] = dist_dist

dist_dist = 2
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(3, 6))
rDat$distdist[idx] = dist_dist

dist_dist = 3
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(2, 1))
rDat$distdist[idx] = dist_dist

targ_loc = 6
dist_dist = 1
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(5, 6))
rDat$distdist[idx] = dist_dist

dist_dist = 2
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(4, 1))
rDat$distdist[idx] = dist_dist

dist_dist = 3
idx <- which(rDat$distracter == 1 & rDat$targLoc == targ_loc & rDat$distLoc %in% c(3, 2))
rDat$distdist[idx] = dist_dist



aggregate(data=rDat, thoughtNoAttCap~observer+distLoc, FUN=mean)



#  get total path length for each trials
rDat$nFix = NA
rDat$pathLength = 0
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




#  normalise path length
rDat$pathLength = rDat$pathLength/256
rDat$lengthOK = as.factor(abs(rDat$pathLength - 1) < 0.2)


rDat$thoughtNoAttCap = as.factor(rDat$thoughtNoAttCap)
levels(rDat$thoughtNoAttCap) = c("no", "yes")
rDat$thoughtNoAttCap = factor(rDat$thoughtNoAttCap, levels(rDat$thoughtNoAttCap)[c(2,1)])
# reorder factor level
 # dat$thoughtNoAttCap= factor(dat$thoughtNoAttCap,levels(dat$thoughtNoAttCap)[c()])

fDat$observer = as.factor(fDat$observer)
rDat$observer = as.factor(rDat$observer)

levels(fDat$observer) = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "A", "B")
levels(rDat$observer) = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "A", "B")



write.csv(fDat, "fixations.csv", row.names=F, quote=F)
write.csv(rDat, "responses.csv", row.names=F, quote=F)
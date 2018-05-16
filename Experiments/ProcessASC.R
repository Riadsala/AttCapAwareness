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

			t0 = as.numeric(unlist(strsplit(unlist(trial[2])[2], " "))[1])
			
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
					onset = as.numeric(unlist(regmatches(fixations$V1, gregexpr("[0-9]+", fixations$V1)))) - t0,

					offset=as.numeric.factor(fixations$V2))

						
				# convert to stimulus coordinates				 		
				trialDat$n = 1:length(trialDat$x)
				# remove fixations with onset post stim_off
				trialDat = filter(trialDat, onset<stimOffTime)
			    # set onset times relative to first fixations
				# trialDat$onset = trialDat$onset - trialDat$onset[1] 

				 fixDat = rbind(fixDat, trialDat)
				 #rm(trialDat)
			}
		}
	}
	return(fixDat)
} 
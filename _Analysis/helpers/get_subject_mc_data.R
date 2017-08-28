#define a function to get the manipulation check data for a given subject
get_subject_mc_data = function(folder,.pb=NULL){
	if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()

	#get samples
	sample_file = list.files(folder,full.names=T,recursive = T,pattern='samples')
	samples  = read.table(sample_file,stringsAsFactors = F,na.strings = '.')[,1:4]
	names(samples) = c('time','x','y','pupil')
	samples$x = samples$x-1024/2
	samples$y = samples$y-768/2
	samples$dist = sqrt( (samples$x^2) + (samples$y^2) )
	samples$out_of_box = (abs(samples$x)>100) | (abs(samples$y)>100)
	# mean(samples$out_of_box,na.rm=T)
	# mean(samples$dist>100,na.rm=T)

	#get blinks
	blink_file = list.files(folder,full.names=T,recursive = T,pattern='blinks')
	blink_times = read.table(blink_file)[,3]

	# #get saccades
	# saccade_file = list.files(file,full.names=T,recursive = T,pattern='saccades')
	# saccades = read.table(saccade_file)[,c(3,8,9)]
	# names(saccades) = c('start','x','y')

	#get msgs
	msg_file = list.files(folder,full.names=T,recursive = T,pattern='msgs')
	msgs  = readLines(msg_file)
	msgs = gsub('MSG\t','',msgs)
	msgs = gsub('!V TRIAL_VAR ','',msgs)

	subset = grepl('TRIALID',msgs)
	trialData = msgs[subset]
	trialData = gsub('TRIALID ','',trialData)
	trialData = tibble::as_tibble(trialData)
	trialData %>%
		tidyr::separate(
			value
			, into = c('trial_time','trial')
			, convert = T
		) %>%
		dplyr::filter(
			trial<60
		)-> trialData

	# subset = substr(msgs,1,5)=='START'
	# temp = msgs[subset][1:60]
	# temp = gsub('START\t','',temp)
	# temp = tibble::as_tibble(temp)
	# temp %>%
	# 	tidyr::separate(
	# 		value
	# 		, into = c('start_time')
	# 		, convert = T
	# 		, extra = 'drop'
	# 	) -> temp
	# trialData = bind_cols(trialData,temp)
	# table(trialData$trial_time-trialData$start_time)
	#
	#
	# subset = grep('DRAW_LIST',msgs)
	# temp = msgs[subset]
	# temp = temp[1:60]
	# temp = tibble::as_tibble(temp)
	# temp %>%
	# 	tidyr::separate(
	# 		value
	# 		, into = 'draw_time'
	# 		, convert = T
	# 		, extra = 'drop'
	# 	) -> temp
	# trialData = bind_cols(trialData,temp)
	# table(trialData$trial_time-trialData$draw_time)
	# table(trialData$start_time-trialData$draw_time)

	subset = which(grepl('DRAW_LIST',msgs))
	temp = msgs[subset+1]
	temp = temp[1:60]
	temp = tibble::as_tibble(temp)
	temp %>%
		tidyr::separate(
			value
			, into = c('string_time','string')
			, convert = T
			, extra = 'drop'
		) -> temp
	table(temp$string)
	trialData = bind_cols(trialData,temp)
	# table(trialData$trial_time-trialData$string_time)
	# table(trialData$start_time-trialData$string_time)
	# table(trialData$draw_time-trialData$string_time) #more consistent,  767ms

	# subset = which(grepl('DRAW_LIST',msgs))
	# temp = msgs[subset+2]
	# temp = temp[1:60]
	# all(grep('Performance_Recall_Practice',temp)) #double check that all are draw messages
	# temp = tibble::as_tibble(temp)
	# temp %>%
	# 	tidyr::separate(
	# 		value
	# 		, into = c('var_time')
	# 		, convert = T
	# 		, extra = 'drop'
	# 	) -> temp
	# trialData = bind_cols(trialData,temp)
	# table(trialData$trial_time-trialData$var_time)
	# table(trialData$draw_time-trialData$var_time)
	# table(trialData$string_time-trialData$var_time)
	#
	# subset = which(grepl('TRIAL_RESULT',msgs))
	# temp = msgs[subset]
	# temp = temp[1:60]
	# temp = tibble::as_tibble(temp)
	# temp %>%
	# 	tidyr::separate(
	# 		value
	# 		, into = 'result_time'
	# 		, convert = T
	# 		, extra = 'drop'
	# 	) -> temp
	# trialData = bind_cols(trialData,temp)
	# table(trialData$trial_time-trialData$result_time)
	# table(trialData$draw_time-trialData$result_time)
	# table(trialData$string_time-trialData$result_time)
	# table(trialData$var_time-trialData$result_time) #most consistent,  156ms



	out = list()
	for(i in 1:nrow(trialData)){
		t0 = trialData$string_time[i]
		critical_blink = any( ((blink_times-t0)<3000) & ((blink_times-t0)>(-500)) )
		#	if(!critical_blink){ #no blinks during this period
		subset = ((samples$time-t0)<3000) & ((samples$time-t0)>(-500))
		samps = samples[subset,]
		samps = samps[!is.na(samps$dist),]
		# samps = samps[samps$dist<100,]
		if(nrow(samps)>0){
			samps$time = samps$time-t0
			#samps$time = round0(samps$time,1)
			samps %>%
				group_by(time) %>%
				summarise(
					pupil = mean(pupil)
					, x = mean(x)
					, y = mean(y)
					, dist = mean(dist)
				) -> samps
			samps$pupil = samps$pupil-mean(samps$pupil[samps$time<0])
			out[[length(out)+1]] = tibble::as_tibble(data.frame(
				trial = i
				, critical_blink = critical_blink
				, condition = trialData$string[i]
				, time = samps$time
				, pupil = samps$pupil
				, x = samps$x
				, y = samps$y
				, dist = samps$dist
			))
		}
		#	}
	}
	out2 = dplyr::bind_rows(out)
	rm(out)
	gc()
	out2$id = strsplit(folder,'/')[[1]][3]
	return(out2)
}

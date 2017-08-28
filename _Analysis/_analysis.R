# load useful packages ----
library(tidyverse)

# Convert EDFs ----

#get the function to convert a given edf
source('helpers/convert_edf.R')

#get list of files to convert
files = list.files(
	path = '../_Data'
	, pattern = '.edf'
	, full.names = T
	, recursive = T
)

#run walk through files, with progress bar
pb = dplyr::progress_estimated(length(files))
purrr::walk(
	.x = files
	, .f = convert_edf
	, .pb = pb
)

# Extract info from ASCs ----

#get function to process a given asc
source('helpers/pre_process_eye_data.R')

#get list of files to process
files = list.files(
	path = '../_Data'
	, pattern = '.asc.gz'
	, full.names = T
	, recursive = T
)

#walk through files, with progress bar
pb = dplyr::progress_estimated(length(files))
purrr::walk(
	.x = files
	, .f = pre_process_eye_data
	, .pb = pb
)

# Get mc data ----

#get function to get the manipulation check data for a given subject
source('helpers/get_subject_mc_data.R')

#get list of folders to process
folders = list.files(
	path = '../_Data'
	, full.names = T
)

#remember to exclude two subjects
folders = folders[!grepl("102tnt",tolower(folders))]
folders = folders[!grepl("105tnt",tolower(folders))]

#collect data from each folder, with progress bar
pb = dplyr::progress_estimated(length(folders))
mc = purrr::map_df(
	.x = folders
	, .f = get_subject_mc_data
	, .pb = pb
)

hist(mc$dist,br=100)
mean(mc$dist>100)
save(mc,file='mc.rdata')

# add any_off_center per trial
mc %>%
	dplyr::group_by(
		condition
		, id
		, trial
	) %>%
	dplyr::mutate(
		any_off_center = any(dist>200)
	) -> mc

mc %>%
	dplyr::group_by(
		condition
		, id
	) %>%
	dplyr::summarise(
		value = mean(critical_blink,na.rm=T)
		, count = n()
	) %>%
	ggplot(
		mapping = aes(
			x = condition
			,  y = value
		)
	)+
	geom_boxplot()+
	geom_point(alpha=.5)+
	labs(
		y = 'proportion of trials\nwith critical blinks'
	)+
	theme(
		aspect.ratio = 1
	)

mc %>%
	dplyr::filter(
		!critical_blink
	) %>%
	dplyr::group_by(
		condition
		, id
	) %>%
	dplyr::summarise(
		value = mean(dist>200,na.rm=T)
		, count = n()
	) %>%
	ggplot(
		mapping = aes(
			x = condition
			,  y = value
		)
	)+
	geom_boxplot()+
	geom_point(alpha=.5)+
	labs(
		y = 'proportion of trials\nwith off-center gaze'
	)+
	theme(
		aspect.ratio = 1
	)

mc %>%
	dplyr::filter(
		!critical_blink
		, !any_off_center
	) %>%
	ggplot()+
	geom_smooth(
		# geom_line(
		mapping = aes(
			x = time
			, y = dist
			, colour = condition
		)
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	)+
	labs(
		y = 'gaze distance from center'
	)

mc %>%
	dplyr::filter(
		!critical_blink
		, !any_off_center
	) %>%
	ggplot()+
	geom_line(
		mapping = aes(
			x = time
			, y = pupil
			, colour = condition
			, group = trial
		)
		, alpha = .5
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	) +
	facet_wrap(~id)+
	labs(
		y = 'pupil size'
	)

mc %>%
	filter(
		!critical_blink
		, !any_off_center
	) %>%
	ggplot()+
	geom_smooth(
		mapping = aes(
			x = time
			, y = pupil
			, colour = condition
		)
		, alpha = .5
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	) +
	facet_wrap(~id)+
	labs(
		y = 'pupil size'
	)

# Get tnt data ----

#get function to get the manipulation check data for a given subject
source('helpers/get_subject_tnt_data.R')

#get list of folders to process
folders = list.files(
	path = '../_Data'
	, full.names = T
)

#exclude some Ss
folders = folders[!grepl("102tnt",tolower(folders))]
folders = folders[!grepl("105tnt",tolower(folders))]

#collect data from each folder, with progress bar
pb = dplyr::progress_estimated(length(folders))
tnt = purrr::map_df(
	.x = folders
	, .f = get_subject_tnt_data
	, .pb = pb
)
save(tnt,file='tnt.rdata')
load(file='tnt.rdata')

# add any_off_center per trial
tnt %>%
	dplyr::group_by(
		condition
		, id
		, trial
	) %>%
	dplyr::mutate(
		any_off_center = any(dist>200)
	) -> tnt

tnt %>%
	dplyr::group_by(
		condition
		, id
	) %>%
	dplyr::summarise(
		value = mean(critical_blink,na.rm=T)
		, count = n()
	) %>%
	ggplot(
		mapping = aes(
			x = condition
			,  y = value
		)
	)+
	geom_boxplot()+
	geom_point(alpha=.5)+
	labs(
		y = 'proportion of trials\nwith critical blinks'
	)+
	theme(
		aspect.ratio = 1
	)

tnt %>%
	dplyr::filter(
		!critical_blink
	) %>%
	dplyr::group_by(
		condition
		, id
	) %>%
	dplyr::summarise(
		value = mean(dist>200,na.rm=T)
		, count = n()
	) %>%
	ggplot(
		mapping = aes(
			x = condition
			,  y = value
		)
	)+
	geom_boxplot()+
	geom_point(alpha=.5)+
	labs(
		y = 'proportion of trials\nwith off-center gaze'
	)+
	theme(
		aspect.ratio = 1
	)

tnt %>%
	dplyr::filter(
		!critical_blink
		, !any_off_center
	) %>%
	ggplot()+
	geom_smooth(
		# geom_line(
		mapping = aes(
			x = time
			, y = dist
			, colour = condition
		)
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	)+
	labs(
		y = 'gaze distance from center'
	)

tnt %>%
	dplyr::filter(
		!critical_blink
		, !any_off_center
	) %>%
	ggplot()+
	geom_line(
		mapping = aes(
			x = time
			, y = pupil
			, colour = condition
			, group = trial
		)
		, alpha = .5
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	) +
	facet_wrap(~id)+
	labs(
		y = 'pupil size'
	)

tnt %>%
	filter(
		!critical_blink
		, !any_off_center
	) %>%
	ggplot()+
	geom_smooth(
		mapping = aes(
			x = time
			, y = pupil
			, colour = condition
		)
		, alpha = .5
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	) +
	facet_wrap(~id)+
	labs(
		y = 'pupil size'
	)

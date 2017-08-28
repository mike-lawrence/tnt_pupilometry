#preliminaries ----

#load useful packages
library(tidyverse)
library(ez)

#load data
load('tnt.rdata')

tnt %>%
	dplyr::group_by(
		trial
		, id
	) %>%
	dplyr::mutate(
		any_moves = max(dist,na.rm=T)>200
		, any_pupil_na = any(is.na(pupil))
	) -> tnt

tnt %>% filter(
	!any_moves
	, !critical_blink
	, !any_pupil_na
) -> tnt


mix_out = ezMixed(
	data = tnt
	, dv = pupil
	, random = id
	, fixed = .(condition,time)
	, gam_k = 10
	, use_bam = TRUE
	, term_labels = c('condition:time')
)
save(mix_out,file='mix.rdata')


preds = ezPredict(
	fit = mix_out$models$`condition:time`$unrestricted
)


ezPlot2(
	preds = preds
	, x = time
	, split = condition
	, ribbon = T
)
ezPlot2(
	preds = preds
	, x = time
	, diff = condition
	, ribbon = T
)

#preliminaries ----

#load useful packages
library(tidyverse)
library(rstan)
library(ezStan)
rstan_options(auto_write = TRUE)

#function to round to a given bin size
round0 = function(x,z){
	round(x/z,0)*z
}

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


dat = tnt


#use half-sum contrasts
# dat = dat[dat$condition!='PB',]
# dat$condition = factor(dat$condition)
# contrasts(dat$condition) = ezStan::halfsum_contrasts

# Prep the data for stan ----
print(length(unique(dat$time))) #complete set is too big
dat$x = round0(dat$time,50) #round to 50ms bins
print(length(unique(dat$x))) #smaller set

#average within time
dat %>%
	group_by(
		id
		, condition
		, trial
		, x
	) %>%
	summarize(
		pupil = mean(pupil,na.rt=T)
		, dist = mean(dist,na.rt=T)
	) -> dat

#make sure data is ordered by subject
dat %>%
	dplyr::arrange(
		id
	) ->
	dat


dat %>%
	ggplot()+
	geom_smooth(
		# geom_line(
		mapping = aes(
			x = x
			, y = pupil
			, colour = condition
		)
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	)

# get the sorted unique value for x
x = sort(unique(dat$x))

# for each value in dat$x, get its index x
x_index = match(dat$x,x)

# compute the model matrix
z = model.matrix(
	data = dat
	, object = ~ condition
)

# compute the unique entries in the model matrix
temp = as.data.frame(z)
temp = tidyr::unite_(data = temp, col = 'combined', from = names(temp))
temp_unique = unique(temp)
z_unique = z[row.names(z)%in%row.names(temp_unique),]
z_unique[2:nrow(z_unique),1] = 0
# for each row in z, get its index z_unique
z_unique_index = match(temp$combined,temp_unique$combined)

# combine the two index objects to get the index into the flattened z_by_f vector
z_by_f_index = z_unique_index + (x_index-1)*nrow(z_unique)

#get subject indices
subj_inds = ezStan::get_subject_indices(dat$id)

# create the data list for stan
data_for_stan = list(
	n_y = nrow(dat)
	, y = scale(dat$pupil)[,1] #scaled to mean=0,sd=1
	, n_x = length(x)
	, x = (x-min(x))/(max(x)-min(x)) #scaled to min=0,max=1
	, x_index = x_index
	, n_z = ncol(z)
	, rows_z_unique = nrow(z_unique)
	, z_unique = z_unique
	, z_by_f_index = z_by_f_index
	, n_subj = nrow(subj_inds)
	, subj_inds = subj_inds
)

# sample the model ----

#compile
hmod = rstan::stan_model('hierarchical_gp_regression.stan')

# start the parallel chains
ezStan::startBigStan(
	stanMod = hmod
	, stanData = data_for_stan
	, cores = 4 #set this to the # of physical cores on your system
	, iter = 2e3 #2e3 takes about 2min
	, stanArgs = "
		  include = FALSE
		, pars = c(
			  'f_normal01'
			, 'volatility_helper'
			, 'subj_f_normal01'
			, 'subj_volatility_helper'
		)
	"
)

#watch the chains' progress
ezStan::watchBigStan()

#play a sound when done
beepr::beep()

# collect results
post = ezStan::collectBigStan()

# kill just in case
ezStan::killBigStan()

# delete temp folder
ezStan::cleanBigStan()

# save results to file
save(post,file='tnt_post.rdata')

#how long did it take?
sort(rowSums(get_elapsed_time(post)/60))
# chain:1  chain:1  chain:1  chain:1
# 1.792383 1.832595 1.847362 1.856770

#check noise &  GP parameters
ezStan::stan_summary(
	from_stan = post
	, par = c('subj_noise_mean','subj_noise_sd','volatility','amplitude')
	#, par = c('noise','volatility','amplitude')
)
#                 50%  2.5% 97.5% n_eff Rhat
# noise          0.97  0.95  0.98  4000    1
# volatility[1] 10.33  8.06 14.67  1004    1
# volatility[2] 15.04 10.02 24.89  1167    1
# amplitude[1]   0.43  0.24  0.82  1092    1
# amplitude[2]   0.21  0.13  0.43  1084    1

#check the rhats for the latent functions
fstats = ezStan::stan_summary(
	from_stan = post
	, par = 'f'
	, return_array = TRUE
)
summary(fstats[,ncol(fstats)]) #rhats
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.9990  0.9996  0.9999  1.0000  1.0003  1.0019


#visualize latent functions
f = rstan::extract(
	post
	, pars = 'f'
)[[1]]

f2 = tibble::as_tibble(data.frame(matrix(
	f
	, byrow = F
	, nrow = dim(f)[1]
	, ncol = dim(f)[2]*dim(f)[3]
)))
f2$sample = 1:nrow(f2)


f2 %>%
	tidyr::gather(
		key = 'key'
		, value = 'value'
		, -sample
	) %>% #View()
	dplyr::mutate(
		key = as.numeric(gsub('X','',key))
	) %>%  #-> temp
	dplyr::mutate(
		key = as.numeric(gsub('X','',key))
		, parameter = rep(
			1:dim(f)[3]
			, each = dim(f)[1]*dim(f)[2]
		)
		, x = rep(x,each=dim(f)[1],times=dim(f)[3])
	) %>%
	dplyr::select(
		-key
	) ->
	fdat
fdat %>%
	mutate(
		condition = case_when(
			.$parameter==1 ~ 'NT'
			, .$parameter==2 ~ 'PB'
			, .$parameter==3 ~ 'T'
		)
	) ->
	fdat

fdat %>%
	dplyr::group_by(
		x
		, condition
	) %>%
	dplyr::summarise(
		med = median(value)
		, lo95 = quantile(value,.025)
		, hi95 = quantile(value,.975)
		, lo50 = quantile(value,.25)
		, hi50 = quantile(value,.75)
	) %>%
	ggplot(
		mapping=aes(
			group = condition
			, colour = condition
			, fill = condition
		)
	)+
	geom_ribbon(
		mapping = aes(
			x = x
			, ymin = lo50
			, ymax = hi50
		)
		, alpha = .5
		, colour = 'transparent'
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	geom_hline(
		yintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	)+
	labs(
		x = 'Time (ms)'
		, y = 'Pupil Size\n(proportion of baseline)'
		, fill = 'Condition'
	)


fdat %>%
	dplyr::group_by(
		x
		, sample
	) %>%
	dplyr::summarise(
		value = value[condition=='T'] - value[condition=='NT']
	) %>%
	dplyr::summarise(
		med = median(value)
		, lo95 = quantile(value,.025)
		, hi95 = quantile(value,.975)
		, lo50 = quantile(value,.25)
		, hi50 = quantile(value,.75)
	) %>%
	ggplot()+
	geom_ribbon(
		mapping = aes(
			x = x
			, ymin = lo95
			, ymax = hi95
		)
		, alpha = .5
		, colour = 'transparent'
	)+
	geom_vline(
		xintercept = 0
		, linetype = 3
	)+
	geom_hline(
		yintercept = 0
		, linetype = 3
	)+
	scale_x_continuous(
		expand = c(0,0)
	)+
	labs(
		x = 'Time (ms)'
		, y = 'Pupil Size\n(proportion of baseline)'
		, fill = 'Condition'
	)

#+
	# geom_ribbon(
	# 	mapping = aes(
	# 		x = x
	# 		, ymin = lo50
	# 		, ymax = hi50
	# 	)
	# 	, alpha = .5
	# )+
	# geom_line(
	# 	mapping = aes(
	# 		x = x
	# 		, y = med
	# 	)
	# 	, alpha = .5
	# )
	# +
	# facet_grid(
	# 	parameter ~ .
	# 	, scale = 'free_y'
	# )



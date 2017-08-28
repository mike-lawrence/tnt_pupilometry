functions{
	# GP: computes noiseless Gaussian Process
	vector GP(real volatility, real amplitude, vector normal01, int n_x, real[] x ) {
		matrix[n_x,n_x] cov_mat ;
		real amplitude_sq_plus_jitter ;
		amplitude_sq_plus_jitter = amplitude^2 + 1e-6 ;
		cov_mat = cov_exp_quad(x, amplitude, 1/volatility) ;
		for(i in 1:n_x){
			cov_mat[i,i] = amplitude_sq_plus_jitter ;
		}
		return(cholesky_decompose(cov_mat) * normal01 ) ;
	}
}

data {

	# n_y: number of observations in y
	int n_y ;

	# y: vector of observations for y
	#	 should be scaled to mean=0,sd=1
	vector[n_y] y ;

	# n_x: number of unique x values
	int n_x ;

	# x: unique values of x
	#	 should be scaled to min=0,max=1
	real x[n_x] ;

	# x_index: vector indicating which x is associated with each y
	int x_index[n_y] ;

	# n_z: number of columns in predictor matrix z
	int n_z ;

	# rows_z_unique: number of rows in predictor matrix z
	int rows_z_unique ;

	# z_unique: predictor matrix (each column gets its own GP)
	matrix[rows_z_unique,n_z] z_unique ;

	# z_by_f_index:
	int z_by_f_index[n_y] ;

}

parameters {


	# noise: noise of the GP
	real<lower=0> noise ;

	# volatility_helper: helper for cauchy-distributed volatility (see transformed parameters)
	vector<lower=0,upper=pi()/2>[n_z] volatility_helper ;

	# amplitude: amplitude of population GPs
	vector<lower=0>[n_z] amplitude ;

	# f_normal01: helper variable for GPs (see transformed parameters)
	matrix[n_x, n_z] f_normal01 ;


}

transformed parameters{

	# volatility: volatility of population GPs
	vector[n_z] volatility ;

	# f: Population GPs
	matrix[n_x,n_z] f ;


	#next line implies volatility ~ cauchy(0,10)
	volatility = 10*tan(volatility_helper) ;

	# loop over predictors, computing population GP
	for(zi in 1:n_z){

		# population GP
		f[,zi] = GP(
			  volatility[zi]
			, amplitude[zi]
			, f_normal01[,zi]
			, n_x , x
		) ;

	}

}

model {

	# noise priors
	noise ~ weibull(2,1) ; #peaked at .8ish

	# volatility priors:
	# - population GPs have volatility ~ cauchy(0,10)

	# amplitude priors
	# - population GPs have amplitude as weibull peaked at .8
	amplitude ~ weibull(2,1) ; #peaked at .8ish

	# normal(0,1) priors on GP helpers
	to_vector(f_normal01) ~ normal(0,1);

	# loop over observations
	{
		matrix[rows_z_unique,n_x] z_by_f ;

		for(i in 1:rows_z_unique){
			for(j in 1:n_x){
				z_by_f[i,j] = sum(z_unique[i].*f[j,]) ;
			}
		}
  		y ~ normal(
  			  to_vector(z_by_f)[z_by_f_index]
  			, noise
  		);
	}

}


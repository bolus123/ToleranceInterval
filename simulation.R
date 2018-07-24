#Calculating gamma with parallel technique by simulation
#author: Yuhui Yao
#version: 0.1
#update: 7.24.2018
#########################################################################################################################################################################

require(parallel)                                                                              

#########################################################################################################################################################################



gamma.sim <- function(L, U, alpha, m = 20, n = 5, tau = 1, sim.alpha = 1000, sim.gamma = 1000, core = detectCores() - 1) {

	gamma.f <- function(L, U, alpha, m = 20, n = 5, tau = 1, sim.alpha = 1000){	
																			#Main computational function of calculating gamma with parallel technique
																			#L and U are lower and upper tolerance factors, respectively
																			#m and n are the number of subgroups and the subgroup size
																			#tau is the factor of variance
																			#sim.alpha is the number of simulations of non-signal events
	                                                                        #
		gamma <- sum(unlist(lapply(                                         #simulate gamma
				1:sim.alpha,                                                #
				function(x) {                                               #
																			#
					Y <- matrix(rnorm(m * n), nrow = m, ncol = n) * tau     #simulate X following the standard normal distribution
					S2 <- diag(var(t(Y)))                                   #calculate the subgroups' variances
					S2p <- mean(S2)                                         #calculate the mean of variances
																			#
					sum(L * S2p <= S2 & S2 <= U * S2p) / m >= 1 - alpha     #calcualte the probability of non-signal event
																			#and compare it with 1 - alpha
				}                                                           #
			))) / sim.alpha                                                 #
                                                                            #
		return(gamma)                                                       #
                                                                            #
	}                                                                       #
                                                                            #
	cl <- makeCluster(core)                                                 #make a cluster for parallel
																			#
	clusterExport(cl, c('L', 'U', 'alpha', 'm', 'n', 'tau', 'sim.alpha', 'gamma.f'), envir = environment())
																			#
																			#load variables into the cluster
																			#
	gamma.vec <- unlist(parLapplyLB(										#parallelly calcualte gamma
					cl,                                                     #specify cluster
					1:sim.gamma,                                            #sequentially input the computational process
					function(X) {                                           #
						gamma.f(L, U, alpha, m, n, tau, sim.alpha)          #main function to calculate gamma
					}                                                       #
				))                                                          #
	                                                                        #
	stopCluster(cl)                                                         #shut down cluster when the computation finished
	                                                                        #
	result <- list(                                                         #return result including mean, standard deviation
			gamma.mean = mean(gamma.vec), 									#and 0%, 1%, 5%, 10%, 20%, 25%, 50%, 75%, 80%, 90%,
			gamma.sd = sd(gamma.vec), 										#95%, 99%, 100% percentiles
			gamma.percentile = quantile(gamma.vec, c(0, 0.01, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 1))
		)
	
	return(result)

}

#Exact, gamma = 0.9, alpha = 0.1, L = 0.4226, U = 1.7983

Ex.gamma1 <- gamma.sim(L = 0.4226, U = 1.7983, alpha = 0.1, m = 20, n = 14)
Ex.gamma1

#$gamma.mean
#[1] 0.849568
#
#$gamma.sd
#[1] 0.01152277
#
#$gamma.percentile
#     0%      1%      5%     10%     20%     25%     50%     75%     80%     90%     95%     99%    100% 
#0.81500 0.82300 0.83000 0.83500 0.84000 0.84200 0.84950 0.85700 0.85900 0.86410 0.86900 0.87501 0.88600 

#Exact, gamma = 0.9, alpha = 0.05, L = 0.3533, U = 2.0014

Ex.gamma2 <- gamma.sim(L = 0.3533, U = 2.0014, alpha = 0.05, m = 20, n = 14)
Ex.gamma2

#$gamma.mean
#[1] 0.882606
#
#$gamma.sd
#[1] 0.01027026
#
#$gamma.percentile
#    0%     1%     5%    10%    20%    25%    50%    75%    80%    90%    95%    99%   100% 
#0.8560 0.8600 0.8650 0.8689 0.8740 0.8760 0.8830 0.8900 0.8910 0.8960 0.8990 0.9060 0.9160 

#Wilson-Hilferty, gamma = 0.9, alpha = 0.1, L = 0.4236, U = 1.7958

WH.gamma1 <- gamma.sim(L = 0.4236, U = 1.7958, alpha = 0.1, m = 20, n = 14)
WH.gamma1

#$gamma.mean
#[1] 0.845274
#
#$gamma.sd
#[1] 0.01116779
#
#$gamma.percentile
#     0%      1%      5%     10%     20%     25%     50%     75%     80%     90%     95%     99%    100% 
#0.81300 0.81999 0.82700 0.83100 0.83600 0.83800 0.84500 0.85300 0.85500 0.85900 0.86300 0.87100 0.88300 

#Wilson-Hilferty, gamma = 0.9, alpha = 0.05, L = 0.3536, U = 2.0003

WH.gamma2 <- gamma.sim(L = 0.3536, U = 2.0003, alpha = 0.05, m = 20, n = 14)
WH.gamma2

#$gamma.mean
#[1] 0.881458
#
#$gamma.sd
#[1] 0.0103514
#
#$gamma.percentile
#    0%     1%     5%    10%    20%    25%    50%    75%    80%    90%    95%    99%   100% 
#0.8480 0.8580 0.8640 0.8680 0.8730 0.8750 0.8820 0.8890 0.8902 0.8950 0.8980 0.9050 0.9160 

#Normal-Based, gamma = 0.9, alpha = 0.1, L = 0.4174, U = 1.9108

NB.gamma1 <- gamma.sim(L = 0.4174, U = 1.9108, alpha = 0.1, m = 20, n = 14)
NB.gamma1

#$gamma.mean
#[1] 0.90897
#
#$gamma.sd
#[1] 0.008733234
#
#$gamma.percentile
#   0%    1%    5%   10%   20%   25%   50%   75%   80%   90%   95%   99%  100% 
#0.879 0.889 0.895 0.898 0.902 0.903 0.909 0.915 0.917 0.921 0.923 0.930 0.935 

#Normal-Based, gamma = 0.9, alpha = 0.05, L = 0.3433, U = 2.1368

NB.gamma2 <- gamma.sim(L = 0.3433, U = 2.1368, alpha = 0.05, m = 20, n = 14)
NB.gamma2

#$gamma.mean
#[1] 0.931631
#
#$gamma.sd
#[1] 0.007682699
#
#$gamma.percentile
#   0%    1%    5%   10%   20%   25%   50%   75%   80%   90%   95%   99%  100% 
#0.909 0.914 0.919 0.922 0.925 0.926 0.932 0.937 0.938 0.942 0.944 0.949 0.954 

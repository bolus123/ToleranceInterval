#Calculating gamma with parallel technique by simulation
#author: Yuhui Yao
#version: 0.1
#update: 7.24.2018
#########################################################################################################################################################################

require(parallel)                                                                              

#########################################################################################################################################################################



gamma.sim <- function(L, U, alpha, m = 20, n = 5, tau = 1, sim = 1000, sim.alpha = 1000, sim.gamma = 1000, core = detectCores() - 1) {

	gamma.f <- function(L, U, alpha, m = 20, n = 5, tau = 1, sim.alpha = 1000, sim.gamma = 1000){	
																			#Main computational function of calculating gamma with parallel technique
																			#L and U are lower and upper tolerance factors, respectively
																			#m and n are the number of subgroups and the subgroup size
																			#tau is the factor of variance
																			#sim.alpha is the number of simulations of non-signal events
	                                                                        #							
		X <- matrix(rnorm(m * n), nrow = m, ncol = n)   					#simulate X following the standard normal distribution
		S2 <- diag(var(t(X)))                                   			#calculate the subgroups' variances
		S2p <- mean(S2)                                         			#calculate the mean of variances																			
																			#
		gamma <- mean(unlist(lapply(
					1:sim.gamma,
					function(x) {
			
						Y <- matrix(rnorm(sim.alpha * n), nrow = sim.alpha, ncol = n) * tau													
						S2.samp <- 	diag(var(t(Y)))  														
						gamma <- mean(L * S2p <= S2.samp & S2.samp <= U * S2p) >= 1 - alpha
						cat(gamma, '\n')
						return(gamma)
					}
				)))
																			#calcualte the probability of non-signal event
																			#and compare it with 1 - alpha
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
					1:sim,                                            #sequentially input the computational process
					function(X) {                                           #
						gamma.f(L, U, alpha, m, n, tau, sim.alpha, sim.gamma)
																			#main function to calculate gamma
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

#Exact, gamma = 0.9, alpha = 0.1, L = 0.1581, U = 2.4973, m = 50, n = 5
Ex.gamma1 <- gamma.sim(L = 0.1581, U = 2.4973, alpha = 0.1, m = 50, n = 5, sim = 100, sim.alpha = 100, sim.gamma = 100)
Ex.gamma1

#$gamma.mean
#[1] 0.7707

#$gamma.sd
#[1] 0.1401605

#$gamma.percentile
#    0%     1%     5%    10%    20%    25%    50%    75%    80%    90%    95%    99%   100% 
#0.2800 0.3097 0.4695 0.5980 0.6980 0.7075 0.8100 0.8700 0.8820 0.9000 0.9200 0.9302 0.9500 

#WH, gamma = 0.9, alpha = 0.1, L = 0.1562, U = 2.5102, m = 50, n = 5
WH.gamma1 <- gamma.sim(L = 0.1562, U = 2.5102, alpha = 0.1, m = 50, n = 5, sim = 100, sim.alpha = 100, sim.gamma = 100)
WH.gamma1

#$gamma.mean
#[1] 0.7597
#
#$gamma.sd
#[1] 0.1291969
#
#$gamma.percentile
#    0%     1%     5%    10%    20%    25%    50%    75%    80%    90%    95%    99%   100% 
#0.2700 0.3690 0.5170 0.6190 0.6780 0.6975 0.7900 0.8600 0.8700 0.8900 0.9100 0.9301 0.9400 


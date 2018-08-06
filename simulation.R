#Calculating gamma with parallel technique by simulation
#author: Yuhui Yao
#version: 0.1
#update: 7.24.2018
#########################################################################################################################################################################

require(parallel)                                                                              

#########################################################################################################################################################################



gamma.sim <- function(L = NULL, U = NULL, alpha = 0.1, target.gamma = 0.9, m = 20, n = 5, tau = 1, sim = 1000, sim.alpha = 1000, sim.gamma = 1000, option = 'WH', core = 2) {

	gamma.f <- function(L = NULL, U = NULL, alpha = 0.1, target.gamma = 0.9, m = 20, n = 5, tau = 1, sim.alpha = 1000, sim.gamma = 1000, option = 'WH'){	
																			#Main computational function of calculating gamma with parallel technique
																			#L and U are lower and upper tolerance factors, respectively
																			#m and n are the number of subgroups and the subgroup size
																			#tau is the factor of variance
																			#sim.alpha is the number of simulations of non-signal events
	                                                                        #							
		X <- matrix(rnorm(m * n), nrow = m, ncol = n)   					#simulate X following the standard normal distribution
		S2 <- diag(var(t(X)))                                   			#calculate the subgroups' variances
		S2p <- mean(S2)                                         			#calculate the mean of  variances																			
																			#
																			
		if (option == 'NB') {
			cc <- sqrt((m - 1) * qchisq(1 - alpha, 1, 1 / m) / qchisq(1 - target.gamma, m - 1))
		
			S2.LNB <- (mean(S2^(1 / 3)) - cc * 1 / sqrt(m - 1) * sqrt( sum((S2^(1 / 3) - mean(S2^(1 / 3)))^2 ))  ) ^ 3
			L <- S2.LNB / S2p
			
			S2.UNB <- (mean(S2^(1 / 3)) + cc * 1 / sqrt(m - 1) * sqrt( sum((S2^(1 / 3) - mean(S2^(1 / 3)))^2 ))  ) ^ 3
			U <- S2.UNB / S2p
		}
																			
		gamma <- mean(unlist(lapply(
					1:sim.gamma,
					function(x) {
			
						Y <- matrix(rnorm(sim.alpha * n), nrow = sim.alpha, ncol = n) * tau													
						S2.samp <- 	diag(var(t(Y)))  														
						gamma <- mean(L * S2p <= S2.samp & S2.samp <= U * S2p) >= 1 - alpha
						#cat(gamma, '\n')
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
	clusterExport(cl, c('L', 'U', 'alpha', 'target.gamma', 'm', 'n', 'tau', 'option', 'sim.alpha', 'gamma.f'), envir = environment())
																			#
																			#load variables into the cluster
																			#
	gamma.vec <- unlist(parLapplyLB(										#parallelly calcualte gamma
					cl,                                                     #specify cluster
					1:sim,                                            #sequentially input the computational process
					function(X) {                                           #
						gamma.f(L, U, alpha, target.gamma, m, n, tau, sim.alpha, sim.gamma, option)
																			#main function to calculate gamma
					}                                                       #
				))                                                          #
	                                                                        #
	stopCluster(cl)                                                         #shut down cluster when the computation finished
	                                                                        #
	result <- list(                                                         #return result including mean, standard deviation
			gamma.mean = mean(gamma.vec), 									#and 0%, 1%, 5%, 10%, 20%, 25%, 50%, 75%, 80%, 90%,
			gamma.sd = sd(gamma.vec), 										#95%, 99%, 100% percentiles
			gamma.percentile = quantile(gamma.vec, c(0, 0.01, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 1)),
            gamma.vec = gamma.vec
		)
	
	return(result)

}

#debug(gamma.sim)

gamma.sim.vec <- function(L = NULL, U = NULL, alpha = 0.1, target.gamma = 0.9, m = 20, n = 5, tau = 1, sim = 1000, sim.alpha = 1000, sim.gamma = 1000, option = 'WH', core = 2) {
    result <- gamma.sim(L, U, alpha, target.gamma, m, n, tau, sim, sim.alpha, sim.gamma, option, core)$gamma.mean
    return(result)
}

gamma.sim.vec <- Vectorize(gamma.sim.vec, vectorize.args = c('L', 'U', 'alpha', 'target.gamma', 'm', 'n', 'tau', 'sim', 'sim.alpha', 'sim.gamma'))
                                                                            #Vectorizing the simulation of gamma
                                                                            #result only contains the mean of simulation of gamma

#########################################################################################################################################################################

#Example

#########################################################################################################################################################################

#Ex.gamma1 <- gamma.sim(L = 0.4226, U = 1.7983, alpha = 0.1, m = 20, n = 14, sim = 100, sim.alpha = 100, sim.gamma = 100, core = 3)
#Ex.gamma1

#Vecotorized Example
#gamma.sim.vec(
#    L = c(.1100, .1056), 
#    U = c(2.8904, 2.9351), 
#    alpha = 0.05, 
#	target.gamma = 0.9,
#    m = 100, 
#    n = 5, 
#    tau = 1, 
#    sim = 100, 
#    sim.alpha = 100, 
#    sim.gamma = 100,
#	core = 3
#) 
#
#gamma.sim.vec(
#    L = 0, 
#    U = 0, 
#    alpha = 0.05, 
#	target.gamma = 0.9,
#    m = 100, 
#    n = 5, 
#    tau = 1, 
#    sim = 100, 
#    sim.alpha = 100, 
#    sim.gamma = 100,
#	option = 'NB',
#	core = 3
#) 

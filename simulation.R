#Calculating gamma with parallel technique by simulation
#author: Yuhui Yao
#version: 0.1
#update: 7.24.2018
#########################################################################################################################################################################

require(parallel)                                                                              

#########################################################################################################################################################################



gamma.sim <- function(L = NULL, U = NULL, alpha = 0.1, target.gamma = 0.9, m = 20, n = 5, tau = 1, sim = 1000, sim.alpha = 1000, sim.gamma = 1000, method.option = 'WH', PH2.rgamma.shape = (n - 1) / 2, PH2.rgamma.scale = 2, core = 2) {

	gamma.f <- function(L = NULL, U = NULL, alpha = 0.1, target.gamma = 0.9, m = 20, n = 5, tau = 1, sim.alpha = 1000, sim.gamma = 1000, method.option = 'WH', PH2.rgamma.shape = (n - 1) / 2, PH2.rgamma.scale = 2){	
	#Main computational function of calculating gamma with parallel technique
	#L and U are lower and upper tolerance factors, respectively
	#m and n are the number of subgroups and the subgroup size
	#tau is the factor of variance
	#sim.alpha is the number of simulations of non-signal events
	#							
	#simulate X following the standard normal distribution
	#calculate the subgroups' variances
	#calculate the mean of  variances																			
	#
		
		#X <- matrix(rnorm(m * n), nrow = m, ncol = n)
		#S2 <- diag(var(t(X)))                        
		#S2p <- mean(S2)     
        S2 <- rchisq(m, n - 1) / (n - 1)
		S2p <- sum(S2) / m #rchisq(1, m * (n - 1)) / m / (n - 1)
																			
		if (method.option == 'NB') {
			cc <- sqrt((m - 1) * qchisq(1 - alpha, 1, 1 / m) / qchisq(1 - target.gamma, m - 1))
		
			S2.LNB <- (mean(S2^(1 / 3)) - cc * 1 / sqrt(m - 1) * sqrt( sum((S2^(1 / 3) - mean(S2^(1 / 3)))^2 ))  ) ^ 3
			L <- S2.LNB / S2p
			
			S2.UNB <- (mean(S2^(1 / 3)) + cc * 1 / sqrt(m - 1) * sqrt( sum((S2^(1 / 3) - mean(S2^(1 / 3)))^2 ))  ) ^ 3
			U <- S2.UNB / S2p
		}
        
		#Special part for the Normal-based method. The methodology is written in Appendix B
																			
		gamma <- mean(unlist(lapply(
					1:sim.gamma,
					function(x) {
			
						#Y <- matrix(rnorm(sim.alpha * n), nrow = sim.alpha, ncol = n) * tau													
						#S2.samp <- 	diag(var(t(Y)))  
                        S2.samp <- rgamma(sim.alpha, shape = PH2.rgamma.shape, scale = PH2.rgamma.scale) / (n - 1) #/ PH2.rgamma.shape / PH2.rgamma.scale
                        cat(S2.samp)

						gamma <- mean(L * S2p <= S2.samp & S2.samp <= U * S2p) >= 1 - alpha
						#cat(gamma, '\n')
						return(gamma)
					}
				)))
				
		#calcualte the probability of non-signal event
		#and compare it with 1 - alpha
                                                                           
		return(gamma)                                                      
                                                                           
	}                                                                      
                                                                           
	cl <- makeCluster(core)                                                 
	#make a cluster for parallel
																			
	clusterExport(cl, c('L', 'U', 'alpha', 'target.gamma', 'm', 'n', 'tau', 'method.option', 'sim.alpha', 
        'sim.gamma', 'gamma.f', 'PH2.rgamma.shape', 'PH2.rgamma.scale'), envir = environment())
	#load variables into the cluster
																			
	gamma.vec <- unlist(parLapplyLB(										
					cl,                                                     
					1:sim,                                            		
					function(X) {                                           
						gamma.f(L, U, alpha, target.gamma, m, n, tau, sim.alpha, sim.gamma, method.option, PH2.rgamma.shape, PH2.rgamma.scale)
						#main function to calculate gamma
					}                                                       
				))                                                          
				
	#parallelly calcualte gamma
	#specify cluster
	#sequentially input the computational process
				
	                                                                        
	stopCluster(cl)
	#shut down cluster when the computation finished
	                                                                        
	result <- list(                                                         
			gamma.mean = mean(gamma.vec), 									
			gamma.sd = sd(gamma.vec), 										
			gamma.percentile = quantile(gamma.vec, c(0, 0.01, 0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95, 0.99, 1)),
            gamma.vec = gamma.vec
		)
	#return result including mean, standard deviation	
	#and 0%, 1%, 5%, 10%, 20%, 25%, 50%, 75%, 80%, 90%,
	#95%, 99%, 100% percentiles
	
	
	return(result)

}

#debug(gamma.sim)

gamma.sim.vec <- function(L = NULL, U = NULL, alpha = 0.1, target.gamma = 0.9, m = 20, n = 5, tau = 1, sim = 1000, sim.alpha = 1000, sim.gamma = 1000, method.option = 'WH', PH2.rgamma.shape = (n - 1) / 2, PH2.rgamma.scale = 2, core = 2) {
    result <- gamma.sim(L, U, alpha, target.gamma, m, n, tau, sim, sim.alpha, sim.gamma, method.option, PH2.rgamma.shape, PH2.rgamma.scale, core)$gamma.mean
    return(result)
}

gamma.sim.vec <- Vectorize(gamma.sim.vec, vectorize.args = c('L', 'U', 'alpha', 'target.gamma', 'm', 'n', 'tau', 'sim', 'sim.alpha', 
        'sim.gamma', 'PH2.rgamma.shape', 'PH2.rgamma.scale'))
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




#for (sim.alpha in sim.alpha.vec){
#
#    a <- gamma.sim.vec(
#        L = c(
#             0.1193，
#             0.1467，
#             0.1581，
#             0.1626,
#             0.1650，
#             0.1704
#        ), 
#        U = c(
#            2.8018,
#            2.5780,
#            2.4973,
#            2.4674,
#            2.4512,
#            2.4171
#        ), 
#        alpha = 0.1, 
#        target.gamma = 0.9,
#        m = c(10, 25, 50, 75, 100, 250), 
#        n = 5, 
#        tau = 1, 
#        sim = 1000, 
#        sim.alpha = sim.alpha, 
#        sim.gamma = 250,
#        core = 3
#    ) 
#    cat(a, '\n')
#
#}

#sim.alpha.vec <- c(100, 250)

#sim.alpha.vec <- c(100, 250, 500, 1000, 5000, 10000)
sim.alpha.vec <- c(5000, 10000)
shape.vec <- c(1.5, 1.6, 1.7, 1.8, 1.9, 2.1, 2.2, 2.3, 2.4, 2.5)
scale.vec <- c(2)

result <- matrix(NA, nrow = length(sim.alpha.vec) * length(shape.vec) * length(scale.vec), ncol = 9)

i <- 0
for (shape in shape.vec){

    for (scale in scale.vec){

        for (sim.alpha in sim.alpha.vec){

            i <- i + 1
        
            b <- gamma.sim.vec(
                L = c(
                     0.1196,
                     0.1453,
                     0.1562,
                     0.1606,
                     0.1630,
                     0.1683
                ), 
                U = c(
                    2.7990,
                    2.5886,
                    2.5102,
                    2.4807,
                    2.4646,
                    2.4304
                ), 
                alpha = 0.1, 
                target.gamma = 0.9,
                m = c(10, 25, 50, 75, 100, 250), 
                n = 5, 
                tau = 1, 
                sim = 1000, 
                sim.alpha = sim.alpha, 
                sim.gamma = 250,
                PH2.rgamma.shape = shape,
                PH2.rgamma.scale = scale,
                core = 6
            ) 
            cat(b, '\n')
            
            result[i, 1] <- shape
            result[i, 2] <- scale
            result[i, 3] <- sim.alpha
            result[i, 4:9] <- b
            
            save(result, file = '/home/yyao17/Documents/ToleranceInterval/simResult1.Rdata')

        }

    }

}


sim.alpha.vec <- c(100, 250, 500, 1000)
shape.vec <- c(2)
scale.vec <- c(1.5, 1.6, 1.7, 1.8, 1.9, 2.1, 2.2, 2.3, 2.4, 2.5)

result <- matrix(NA, nrow = length(sim.alpha.vec) * length(shape.vec) * length(scale.vec), ncol = 9)

i <- 0
for (shape in shape.vec){

    for (scale in scale.vec){

        for (sim.alpha in sim.alpha.vec){

            i <- i + 1
        
            b <- gamma.sim.vec(
                L = c(
                     0.1196,
                     0.1453,
                     0.1562,
                     0.1606,
                     0.1630,
                     0.1683
                ), 
                U = c(
                    2.7990,
                    2.5886,
                    2.5102,
                    2.4807,
                    2.4646,
                    2.4304
                ), 
                alpha = 0.1, 
                target.gamma = 0.9,
                m = c(10, 25, 50, 75, 100, 250), 
                n = 5, 
                tau = 1, 
                sim = 1000, 
                sim.alpha = sim.alpha, 
                sim.gamma = 250,
                PH2.rgamma.shape = shape,
                PH2.rgamma.scale = scale,
                core = 6
            ) 
            cat(b, '\n')
            
            result[i, 1] <- shape
            result[i, 2] <- scale
            result[i, 3] <- sim.alpha
            result[i, 4:9] <- b
            
            save(result, file = '/home/yyao17/Documents/ToleranceInterval/simResult2.Rdata')

        }

    }

}



#for (sim.alpha in sim.alpha.vec){
#
#    cc <- gamma.sim.vec(
#        L = c(
#             0
#        ), 
#        U = c(
#            0
#        ), 
#        alpha = 0.1, 
#        target.gamma = 0.9,
#        m = c(10, 25, 50, 75, 100, 250), 
#        n = 5, 
#        tau = 1, 
#        sim = 1000, 
#        sim.alpha = sim.alpha, 
#        sim.gamma = 250,
#        option = 'NB',
#        core = 3
#    ) 
#    cat(cc, '\n')
#
#}


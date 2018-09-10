#Calculating gamma with parallel technique by simulation
#author: Yuhui Yao
#version: 0.9
#update: 9.6.2018
#########################################################################################################################################################################

src.code.addr <- 'https://raw.githubusercontent.com/bolus123/ToleranceInterval/master/'

#########################################################################################################################################################################

require(parallel)                                                                              

#########################################################################################################################################################################

source(file = paste(src.code.addr, 'Exact_TI_S2_alpha_star.R', sep = ''))
source(file = paste(src.code.addr, 'CE_Method_TI_S2.R', sep = ''))

EX.find_alpha_star <- find_alpha_star
CE.find_alpha_star <- find1_alpha_star

#########################################################################################################################################################################

gamma.sim <- function(nom.alpha = 0.1, nom.gamma = 0.9, m = 20, n = 5, sim = 1000, sim.alpha = 1000, sim.gamma = 1000, method.option = 'KMM', 
					  PH1.rgamma.shape = (n - 1) / 2, PH1.rgamma.scale = 2, PH2.rgamma.shape = PH1.rgamma.shape, PH2.rgamma.scale = PH1.rgamma.scale, core = 2) {
	
	inner.loop.f <- function(L = NULL, U = NULL, x, S2, S2p, m, n, PH2.rgamma.shape, PH2.rgamma.scale, method.option = 'KMM') {
						
			if (method.option == 'KMM') {
				cc <- sqrt((m - 1) * qchisq(1 - nom.alpha, 1, 1 / m) / qchisq(1 - nom.gamma, m - 1))

				S2.LNB <- (mean(S2[, x]^(1 / 3)) - cc * 1 / sqrt(m - 1) * sqrt( sum((S2[, x]^(1 / 3) - mean(S2[, x]^(1 / 3)))^2 ))  ) ^ 3
				L <- S2.LNB / S2p[x]

				S2.UNB <- (mean(S2[, x]^(1 / 3)) + cc * 1 / sqrt(m - 1) * sqrt( sum((S2[, x]^(1 / 3) - mean(S2[, x]^(1 / 3)))^2 ))  ) ^ 3
				U <- S2.UNB / S2p[x]
			}
			
	
        	S2.samp <- rgamma(sim.alpha, shape = PH2.rgamma.shape, scale = PH2.rgamma.scale) / (n - 1)

			gamma <- mean(L * S2p[x] <= S2.samp & S2.samp <= U * S2p[x]) >= 1 - nom.alpha
			return(gamma)
		}
		
	inner.loop.f <- Vectorize(inner.loop.f, vectorize.args = 'x')
		
	gamma.f <- function(L = NULL, U = NULL, nom.alpha = 0.1, nom.gamma = 0.9, m = 20, n = 5, sim.alpha = 1000, sim.gamma = 1000, method.option = 'WH', 
						PH1.rgamma.shape = (n - 1) / 2, PH1.rgamma.scale = 2, PH2.rgamma.shape = PH1.rgamma.shape, PH2.rgamma.scale = PH1.rgamma.scale){	
	#Main computational function of calculating gamma with parallel technique
	#L and U are lower and upper tolerance factors, respectively
	#m and n are the number of subgroups and the subgroup size
	#sim.alpha is the number of simulations of non-signal events
	#							
	#simulate X following the standard normal distribution
	#calculate the subgroups' variances
	#calculate the mean of  variances																			
	#
 
		S2 <- matrix(rgamma(m * sim.gamma, shape = PH1.rgamma.shape, scale = PH1.rgamma.scale), nrow = m, ncol = sim.gamma) / (n - 1)
		
		S2p <- colMeans(S2)

		#Special part for the Normal-based method. The methodology is written in Appendix B

		gamma.vec <- inner.loop.f(L = L, U = U, x = 1:sim.gamma, S2 = S2, S2p = S2p, m = m, n = n, PH2.rgamma.shape = PH2.rgamma.shape, PH2.rgamma.scale = PH2.rgamma.scale, method.option = method.option)
		
		gamma <- mean(gamma.vec)
				
		#calcualte the probability of non-signal event
		#and compare it with 1 - nom.alpha
                                                                           
		return(gamma)                                                      
                                                                           
	}                                                                      
	
	cl <- makeCluster(core)                                                 
	#make a cluster for parallel
		
	L <- NULL
	U <- NULL
											
	if (method.option == 'EXACT'){
			
		limits <- EX.find_alpha_star(m = m,n = n,alpha = nom.alpha,nom_gamma = nom.gamma)
		
		L <- limits[2]
		U <- limits[3]
				
	} else if (method.option == 'CE'){
				
		limits <- CE.find_alpha_star(m = m,n = n,alpha = nom.alpha, gamma = nom.gamma)
		
		L <- limits[2]
		U <- limits[3]
			
	}
	
	
	
	clusterExport(cl, c('L', 'U', 'nom.alpha', 'nom.gamma', 'm', 'n', 'method.option', 'sim.alpha', 
        'sim.gamma', 'gamma.f', 'inner.loop.f', 'PH1.rgamma.shape', 'PH1.rgamma.scale', 'PH2.rgamma.shape', 'PH2.rgamma.scale'), envir = environment())
	
	clusterExport(cl, c('secantc', 'secant', 'gamma_val', 'EX.find_alpha_star', 'secant.method', 'WH_gamma_approx', 'CE.find_alpha_star'))
	
	#load variables into the cluster
	
	gamma.vec <- unlist(parLapplyLB(										
					cl,                                                     
					1:sim,                                            		
					function(X) {                                           
						gamma.f(L, U, nom.alpha, nom.gamma, m, n, sim.alpha, sim.gamma, method.option, PH1.rgamma.shape, PH1.rgamma.scale, PH2.rgamma.shape, PH2.rgamma.scale)
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

gamma.sim.vec <- function(nom.alpha = 0.1, nom.gamma = 0.9, m = 20, n = 5, sim = 1000, sim.alpha = 1000, sim.gamma = 1000, method.option = 'KMM',
					  PH1.rgamma.shape = (n - 1) / 2, PH1.rgamma.scale = 2, PH2.rgamma.shape = PH1.rgamma.shape, PH2.rgamma.scale = PH1.rgamma.scale, core = 2) {
    result <- gamma.sim(nom.alpha, nom.gamma, m, n, sim, sim.alpha, sim.gamma, method.option, PH1.rgamma.shape, PH1.rgamma.scale, PH2.rgamma.shape, PH2.rgamma.scale, core)$gamma.mean
    return(result)
}

gamma.sim.vec <- Vectorize(gamma.sim.vec, vectorize.args = c('nom.alpha', 'nom.gamma', 'm', 'n', 'sim', 'sim.alpha', 'sim.gamma',
					  'PH1.rgamma.shape', 'PH1.rgamma.scale', 'PH2.rgamma.shape', 'PH2.rgamma.scale'))
#Vectorizing the simulation of gamma
#result only contains the mean of simulation of gamma

#########################################################################################################################################################################

#simulation for KMM

#########################################################################################################################################################################
#
#sim.vec <- 1000
#sim.alpha.vec <- c(100, 250, 500, 1000)
##sim.alpha.vec <- c(100)
#sim.gamma.vec <- 250
#
#alpha.vec <- c(0.05, 0.1)
#gamma.vec <- c(0.9, 0.95)
#
#m.vec <- c(10, 25, 50, 75, 100, 250)
##m.vec <- c(10, 25)
#n.vec <- c(5, 10)
#
#shift.vec <- seq(-0.25, 0.25, 0.05)
##shift.vec <- 0
#
#pars.mat <- expand.grid(sim.vec, sim.gamma.vec, sim.alpha.vec, alpha.vec, gamma.vec, m.vec, n.vec, shift.vec)
#
#pars.mat1 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2, (pars.mat[, 7] - 1) / 2 * (1 + pars.mat[, 8]), 2)
#pars.mat2 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2, (pars.mat[, 7] - 1) / 2, 2 * (1 + pars.mat[, 8]))
#
#pars.mat3 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2 * (1 + pars.mat[, 8]), 2, (pars.mat[, 7] - 1) / 2, 2)
#pars.mat4 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2 * (1 + pars.mat[, 8]), (pars.mat[, 7] - 1) / 2, 2)
#
#nn <- dim(pars.mat1)[1]
#mm <- dim(pars.mat1)[2]
#
#result1 <- cbind(pars.mat1, NA)
#result2 <- cbind(pars.mat2, NA)
#result3 <- cbind(pars.mat3, NA)
#result4 <- cbind(pars.mat4, NA)
#
#result1[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat1[, 4], 
#				nom.gamma = pars.mat1[, 5], 
#				m = pars.mat1[, 6], 
#				n = pars.mat1[, 7], 
#				sim = pars.mat1[, 1], 
#				sim.alpha = pars.mat1[, 2], 
#				sim.gamma = pars.mat1[, 3], 
#			  	PH1.rgamma.shape = pars.mat1[, 9], 
#				PH1.rgamma.scale = pars.mat1[, 10], 
#				PH2.rgamma.shape = pars.mat1[, 11], 
#				PH2.rgamma.scale = pars.mat1[, 12], 
#				method.option = 'KMM',
#				core = 6
#) 
#
#save(result1, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result1.KMM.Rdata')
#
#result2[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat2[, 4], 
#				nom.gamma = pars.mat2[, 5], 
#				m = pars.mat2[, 6], 
#				n = pars.mat2[, 7], 
#				sim = pars.mat2[, 1], 
#				sim.alpha = pars.mat2[, 2], 
#				sim.gamma = pars.mat2[, 3], 
#			  	PH1.rgamma.shape = pars.mat2[, 9], 
#				PH1.rgamma.scale = pars.mat2[, 10], 
#				PH2.rgamma.shape = pars.mat2[, 11], 
#				PH2.rgamma.scale = pars.mat2[, 12], 
#				method.option = 'KMM',
#				core = 6
#) 
#
#save(result2, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result2.KMM.Rdata')
#
#result3[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat3[, 4], 
#				nom.gamma = pars.mat3[, 5], 
#				m = pars.mat3[, 6], 
#				n = pars.mat3[, 7], 
#				sim = pars.mat3[, 1], 
#				sim.alpha = pars.mat3[, 2], 
#				sim.gamma = pars.mat3[, 3], 
#			  	PH1.rgamma.shape = pars.mat3[, 9], 
#				PH1.rgamma.scale = pars.mat3[, 10], 
#				PH2.rgamma.shape = pars.mat3[, 11], 
#				PH2.rgamma.scale = pars.mat3[, 12], 
#				method.option = 'KMM',
#				core = 6
#) 
#
#save(result3, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result3.KMM.Rdata')
#
#result4[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat4[, 4], 
#				nom.gamma = pars.mat4[, 5], 
#				m = pars.mat4[, 6], 
#				n = pars.mat4[, 7], 
#				sim = pars.mat4[, 1], 
#				sim.alpha = pars.mat4[, 2], 
#				sim.gamma = pars.mat4[, 3], 
#			  	PH1.rgamma.shape = pars.mat4[, 9], 
#				PH1.rgamma.scale = pars.mat4[, 10], 
#				PH2.rgamma.shape = pars.mat4[, 11], 
#				PH2.rgamma.scale = pars.mat4[, 12], 
#				method.option = 'KMM',
#				core = 6
#) 
#
#save(result4, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result4.KMM.Rdata')

#########################################################################################################################################################################

#simulation for CE

#########################################################################################################################################################################

#
#sim.vec <- 1000
#sim.alpha.vec <- c(100, 250, 500, 1000)
##sim.alpha.vec <- c(100)
#sim.gamma.vec <- 250
#
#alpha.vec <- c(0.05, 0.1)
#gamma.vec <- c(0.9, 0.95)
#
#m.vec <- c(10, 25, 50, 75, 100, 250)
##m.vec <- c(10, 25)
#n.vec <- c(5, 10)
#
#shift.vec <- seq(-0.25, 0.25, 0.25)
##shift.vec <- 0
#
#pars.mat <- expand.grid(sim.vec, sim.gamma.vec, sim.alpha.vec, alpha.vec, gamma.vec, m.vec, n.vec, shift.vec)
#
#pars.mat1 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2, (pars.mat[, 7] - 1) / 2 * (1 + pars.mat[, 8]), 2)
#pars.mat2 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2, (pars.mat[, 7] - 1) / 2, 2 * (1 + pars.mat[, 8]))
#
#pars.mat3 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2 * (1 + pars.mat[, 8]), 2, (pars.mat[, 7] - 1) / 2, 2)
#pars.mat4 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2 * (1 + pars.mat[, 8]), (pars.mat[, 7] - 1) / 2, 2)
#
#nn <- dim(pars.mat1)[1]
#mm <- dim(pars.mat1)[2]
#
#result1 <- cbind(pars.mat1, NA)
#result2 <- cbind(pars.mat2, NA)
#result3 <- cbind(pars.mat3, NA)
#result4 <- cbind(pars.mat4, NA)
#
#result1[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat1[, 4], 
#				nom.gamma = pars.mat1[, 5], 
#				m = pars.mat1[, 6], 
#				n = pars.mat1[, 7], 
#				sim = pars.mat1[, 1], 
#				sim.alpha = pars.mat1[, 2], 
#				sim.gamma = pars.mat1[, 3], 
#			  	PH1.rgamma.shape = pars.mat1[, 9], 
#				PH1.rgamma.scale = pars.mat1[, 10], 
#				PH2.rgamma.shape = pars.mat1[, 11], 
#				PH2.rgamma.scale = pars.mat1[, 12], 
#				method.option = 'CE',
#				core = 6
#) 
#
#save(result1, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result1.CE.Rdata')
#
#result2[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat2[, 4], 
#				nom.gamma = pars.mat2[, 5], 
#				m = pars.mat2[, 6], 
#				n = pars.mat2[, 7], 
#				sim = pars.mat2[, 1], 
#				sim.alpha = pars.mat2[, 2], 
#				sim.gamma = pars.mat2[, 3], 
#			  	PH1.rgamma.shape = pars.mat2[, 9], 
#				PH1.rgamma.scale = pars.mat2[, 10], 
#				PH2.rgamma.shape = pars.mat2[, 11], 
#				PH2.rgamma.scale = pars.mat2[, 12], 
#				method.option = 'CE',
#				core = 6
#) 
#
#save(result2, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result2.CE.Rdata')
#
#result3[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat3[, 4], 
#				nom.gamma = pars.mat3[, 5], 
#				m = pars.mat3[, 6], 
#				n = pars.mat3[, 7], 
#				sim = pars.mat3[, 1], 
#				sim.alpha = pars.mat3[, 2], 
#				sim.gamma = pars.mat3[, 3], 
#			  	PH1.rgamma.shape = pars.mat3[, 9], 
#				PH1.rgamma.scale = pars.mat3[, 10], 
#				PH2.rgamma.shape = pars.mat3[, 11], 
#				PH2.rgamma.scale = pars.mat3[, 12], 
#				method.option = 'CE',
#				core = 6
#) 
#
#save(result3, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result3.CE.Rdata')
#
#result4[, mm + 1] <- gamma.sim.vec(
#				nom.alpha = pars.mat4[, 4], 
#				nom.gamma = pars.mat4[, 5], 
#				m = pars.mat4[, 6], 
#				n = pars.mat4[, 7], 
#				sim = pars.mat4[, 1], 
#				sim.alpha = pars.mat4[, 2], 
#				sim.gamma = pars.mat4[, 3], 
#			  	PH1.rgamma.shape = pars.mat4[, 9], 
#				PH1.rgamma.scale = pars.mat4[, 10], 
#				PH2.rgamma.shape = pars.mat4[, 11], 
#				PH2.rgamma.scale = pars.mat4[, 12], 
#				method.option = 'CE',
#				core = 6
#) 
#
#save(result4, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result4.CE.Rdata')

#########################################################################################################################################################################

#simulation for EXACT

#########################################################################################################################################################################


sim.vec <- 1000
sim.alpha.vec <- c(100, 250, 500, 1000)
#sim.alpha.vec <- c(100)
sim.gamma.vec <- 250

alpha.vec <- c(0.05, 0.1)
gamma.vec <- c(0.9, 0.95)

m.vec <- c(10, 25, 50, 75, 100, 250)
#m.vec <- c(250)
n.vec <- c(5, 10)

shift.vec <- seq(-0.25, 0.25, 0.25)
#shift.vec <- 0

pars.mat <- expand.grid(sim.vec, sim.gamma.vec, sim.alpha.vec, alpha.vec, gamma.vec, m.vec, n.vec, shift.vec)

pars.mat1 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2, (pars.mat[, 7] - 1) / 2 * (1 + pars.mat[, 8]), 2)
pars.mat2 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2, (pars.mat[, 7] - 1) / 2, 2 * (1 + pars.mat[, 8]))

pars.mat3 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2 * (1 + pars.mat[, 8]), 2, (pars.mat[, 7] - 1) / 2, 2)
pars.mat4 <- cbind(pars.mat, (pars.mat[, 7] - 1) / 2, 2 * (1 + pars.mat[, 8]), (pars.mat[, 7] - 1) / 2, 2)

nn <- dim(pars.mat1)[1]
mm <- dim(pars.mat1)[2]

result1 <- cbind(pars.mat1, NA)
result2 <- cbind(pars.mat2, NA)
result3 <- cbind(pars.mat3, NA)
result4 <- cbind(pars.mat4, NA)

result1[, mm + 1] <- gamma.sim.vec(
				nom.alpha = pars.mat1[, 4], 
				nom.gamma = pars.mat1[, 5], 
				m = pars.mat1[, 6], 
				n = pars.mat1[, 7], 
				sim = pars.mat1[, 1], 
				sim.alpha = pars.mat1[, 2], 
				sim.gamma = pars.mat1[, 3], 
			  	PH1.rgamma.shape = pars.mat1[, 9], 
				PH1.rgamma.scale = pars.mat1[, 10], 
				PH2.rgamma.shape = pars.mat1[, 11], 
				PH2.rgamma.scale = pars.mat1[, 12], 
				method.option = 'EXACT',
				core = 6
) 

save(result1, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result1.EXACT.Rdata')

result2[, mm + 1] <- gamma.sim.vec(
				nom.alpha = pars.mat2[, 4], 
				nom.gamma = pars.mat2[, 5], 
				m = pars.mat2[, 6], 
				n = pars.mat2[, 7], 
				sim = pars.mat2[, 1], 
				sim.alpha = pars.mat2[, 2], 
				sim.gamma = pars.mat2[, 3], 
			  	PH1.rgamma.shape = pars.mat2[, 9], 
				PH1.rgamma.scale = pars.mat2[, 10], 
				PH2.rgamma.shape = pars.mat2[, 11], 
				PH2.rgamma.scale = pars.mat2[, 12], 
				method.option = 'EXACT',
				core = 6
) 

save(result2, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result2.EXACT.Rdata')

result3[, mm + 1] <- gamma.sim.vec(
				nom.alpha = pars.mat3[, 4], 
				nom.gamma = pars.mat3[, 5], 
				m = pars.mat3[, 6], 
				n = pars.mat3[, 7], 
				sim = pars.mat3[, 1], 
				sim.alpha = pars.mat3[, 2], 
				sim.gamma = pars.mat3[, 3], 
			  	PH1.rgamma.shape = pars.mat3[, 9], 
				PH1.rgamma.scale = pars.mat3[, 10], 
				PH2.rgamma.shape = pars.mat3[, 11], 
				PH2.rgamma.scale = pars.mat3[, 12], 
				method.option = 'EXACT',
				core = 6
) 

save(result3, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result3.EXACT.Rdata')

result4[, mm + 1] <- gamma.sim.vec(
				nom.alpha = pars.mat4[, 4], 
				nom.gamma = pars.mat4[, 5], 
				m = pars.mat4[, 6], 
				n = pars.mat4[, 7], 
				sim = pars.mat4[, 1], 
				sim.alpha = pars.mat4[, 2], 
				sim.gamma = pars.mat4[, 3], 
			  	PH1.rgamma.shape = pars.mat4[, 9], 
				PH1.rgamma.scale = pars.mat4[, 10], 
				PH2.rgamma.shape = pars.mat4[, 11], 
				PH2.rgamma.scale = pars.mat4[, 12], 
				method.option = 'EXACT',
				core = 6
) 

save(result4, file = '/home/yyao17/Documents/ToleranceInterval/simulation.result4.EXACT.Rdata')
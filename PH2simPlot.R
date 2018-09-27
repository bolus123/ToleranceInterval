par(mfrow=c(2,2))

col.vec <- rep(NA, 9)

k <- 0

for (i in 1:3){

		for (j in 1:3){
		
			k <- k + 1
			col.vec[k] <- rainbow(3)[i]
			
		}
	
}


###########################################################
#result1
###########################################################

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of W shape')


##################################################nominal gamma and alpha for size #########################################

###  1 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.EXACT.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[1], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[2], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[3], lty = 3)

}

########################################################################################################################


##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  2 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.CE.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[4], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[5], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[6], lty = 3)

}

##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  3 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.KMM.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[7], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[8], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[9], lty = 3)

}

########################################################################################################################


###########################################################
#result2
###########################################################

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of W scale')


##################################################nominal gamma and alpha for size #########################################

###  1 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result2.EXACT.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[1], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[2], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[3], lty = 3)

}

########################################################################################################################


##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  2 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result2.CE.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[4], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[5], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[6], lty = 3)

}

##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  3 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result2.KMM.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[7], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[8], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n & result2[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[9], lty = 3)

}

########################################################################################################################

###########################################################
#result3
###########################################################

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of Y shape')


##################################################nominal gamma and alpha for size #########################################

###  1 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result3.EXACT.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[1], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[2], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[3], lty = 3)

}

########################################################################################################################


##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  2 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result3.CE.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[4], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[5], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[6], lty = 3)

}

##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  3 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result3.KMM.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[7], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[8], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n & result3[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[9], lty = 3)

}

########################################################################################################################


###########################################################
#result4
###########################################################

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of Y scale')


##################################################nominal gamma and alpha for size #########################################

###  1 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result4.EXACT.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[1], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[2], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[3], lty = 3)

}

########################################################################################################################


##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  2 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result4.CE.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[4], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[5], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[6], lty = 3)

}

##################################################nominal gamma and alpha for size #########################################

#plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'nominal gamma = 0.90 and n = 5')

###  3 ###

nom.gamma <- 0.9

n <- 5

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result4.KMM.Rdata')

shift <- 0
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[7], lty = 1)

}

shift <- 0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[8], lty = 2)

}

shift <- -0.25
alpha <- 0.05

for (sim.alpha in c(1000)){

	a <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 6]
	b <- result4[which(result4[, 3] == sim.alpha & result4[, 4] == alpha & result4[, 5] == nom.gamma & result4[, 7] == n & result4[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[9], lty = 3)

}

########################################################################################################################

#weibull and lognormal

########################################################################################################################

nom.gamma <- 0.9

n <- 5

shift <- 0
alpha <- 0.05

par(mfrow=c(2,2))

col.vec <- rep(NA, 9)

k <- 0

for (i in 1:3){

		for (j in 1:2){
		
			k <- k + 1
			col.vec[k] <- rainbow(3)[i]
			
		}
	
}

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Weibull')

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.EXACT.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[1], lty = 1)

}

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.EXACT.WEIBULL.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n ), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n ), 8]
	
	points(a, b, type = 'l', col = col.vec[2], lty = 2)

}


load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.CE.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[3], lty = 1)

}

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result2.CE.WEIBULL.Rdata')

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n ), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n ), 8]
	
	points(a, b, type = 'l', col = col.vec[4], lty = 2)

}


load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.KMM.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[5], lty = 1)

}

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.KMM.WEIBULL.Rdata')

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n ), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n ), 8]
	
	points(a, b, type = 'l', col = col.vec[6], lty = 2)

}

legend.vec <- c('EX Gamma', 'EX Weibull', 'CE Gamma', 'CE Weibull', 'KMM Gamma', 'KMM Weibull')

legend('bottomleft', legend = legend.vec, lty = rep(c(1, 2), 3), col = col.vec, ncol = 2)


########################################################################################################################


weibull.pars.f <- function(n, interval = c(0.1, 10)) {

    kappa.solution.f <- function(k) {1 / beta(1 + 1/k, 1 + 1/k) / (1 + 2/k)}
    root.finding.f <- function(k, n) {(n + 1) / (n - 1) - kappa.solution.f(k)}
    
    kappa <- uniroot(root.finding.f, interval = interval, n = n)$root

	lambda <- (n - 1) / gamma(1 + 1/kappa)
	
	c(kappa, lambda)
	
}


lognormal.pars.f <- function(n, interval = c(0.1, 10)) {

    logmu.f <- function(logsigma2, n) {
        log(n - 1) - logsigma2 / 2
    }
    
    root.finding.f <- function(logsigma2, n) {
    
        2 * (n - 1) - (exp(logsigma2) - 1) * exp(2 * logmu.f(logsigma2, n) + logsigma2)
    
    }
    
    logsigma2 <- uniroot(root.finding.f, interval = interval, n = n)$root
    
    c(logmu.f(logsigma2, n), logsigma2)

}

x <- seq(0, 20, 0.1)

pars <- weibull.pars.f(5, interval = c(0.1, 10))

y1 <- dgamma(x, shape = 5 / 2, scale = 2)
y2 <- dweibull(x, shape = pars[1], scale = pars[2])

plot(c(0, 20), c(0, 0.25), type = 'n', main = 'Pdf of Gamma and Weibull', xlab = 'x', ylab = 'density')
points(x, y1, type = 'l')
points(x, y2, type = 'l', lty = 2)

legend('topright', legend = c('Gamma', 'Weibull'), lty = c(1, 2))

########################################################################################################################

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Lognormal')

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.EXACT.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[1], lty = 1)

}

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.EXACT.LOGNORMAL.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n ), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n ), 8]
	
	points(a, b, type = 'l', col = col.vec[2], lty = 2)

}


load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.CE.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[3], lty = 1)

}

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result2.CE.LOGNORMAL.Rdata')

for (sim.alpha in c(1000)){

	a <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n ), 6]
	b <- result2[which(result2[, 3] == sim.alpha & result2[, 4] == alpha & result2[, 5] == nom.gamma & result2[, 7] == n ), 8]
	
	points(a, b, type = 'l', col = col.vec[4], lty = 2)

}


load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result1.KMM.Rdata')

for (sim.alpha in c(1000)){

	a <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 6]
	b <- result1[which(result1[, 3] == sim.alpha & result1[, 4] == alpha & result1[, 5] == nom.gamma & result1[, 7] == n & result1[, 8] == shift), 13]
	
	points(a, b, type = 'l', col = col.vec[5], lty = 1)

}

load(file = '/home/yuhuiyao/Documents/Github/ToleranceInterval/simulation/simulation.result3.KMM.LOGNORMAL.Rdata')

for (sim.alpha in c(1000)){

	a <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n ), 6]
	b <- result3[which(result3[, 3] == sim.alpha & result3[, 4] == alpha & result3[, 5] == nom.gamma & result3[, 7] == n ), 8]
	
	points(a, b, type = 'l', col = col.vec[6], lty = 2)

}

legend.vec <- c('EX Gamma', 'EX Lognormal', 'CE Gamma', 'CE Lognormal', 'KMM Gamma', 'KMM Lognormal')

legend('bottomleft', legend = legend.vec, lty = rep(c(1, 2), 3), col = col.vec, ncol = 2)


########################################################################################################################

x <- seq(0, 20, 0.1)

pars <- lognormal.pars.f(5, interval = c(0.1, 10))

y1 <- dgamma(x, shape = 5 / 2, scale = 2)
y2 <- dlnorm(x, meanlog = pars[1], sdlog = sqrt(pars[2]))

plot(c(0, 20), c(0, 0.25), type = 'n', main = 'Pdf of Gamma and Lognormal', xlab = 'x', ylab = 'density')
points(x, y1, type = 'l')
points(x, y2, type = 'l', lty = 2)

legend('topright', legend = c('Gamma', 'Lognormal'), lty = c(1, 2))

########################################################################################################################
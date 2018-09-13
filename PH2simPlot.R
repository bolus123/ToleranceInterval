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

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of Phase II shape')


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

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of Phase II scale')


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

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of Phase I shape')


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

plot(c(10, 250), c(0, 1), type = 'n', ylab = 'simulated gamma', xlab = 'm', main = 'Change of Phase I scale')


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

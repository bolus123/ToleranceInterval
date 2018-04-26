prob.beta <- function(m, n, k, V.a, V.b, Y.a, Y.b, pow, cons, alternative = '2-sided') {

	#V <- matrix(rchisq(m * n, V.df), nrow = m, ncol = n) 

	V <- matrix(rgamma(m * n, shape = V.a, scale = V.b), nrow = m, ncol = n)

	X <- V^(pow)
	
	X.bar <- rowMeans(X)
	X.sd <- sqrt(diag(var(t(X))))
	
	L <- (X.bar - cons * X.sd)
	U <- (X.bar + cons * X.sd)

	UL <- cbind(L, U)

	p <- apply(
	  UL, 
	  1, 
	  function(x) {
	    Y <- rgamma(k, shape = Y.a, scale = Y.b)
	    Y <- Y^(pow)
	    
	    if (alternative == '2-sided') {
	    	mean(x[1] < Y & Y < x[2])
	    } else if (alternative == 'upper') {
	    	mean(Y < x[2])
	    } else if (alternative == 'lower') {
	    	mean(x[1] < Y)
	    }

	  }
	)
	
	p
  
}

#a <- prob.beta(m1, n1, k, V.df1, Y.df1, pow1, 14.87) 


prob.alpha <- function(h, m, n, k, V.a, V.b, Y.a = V.a, Y.b = V.b, pow, cons, beta, alternative = '2-sided') {

	p.vec <- unlist(
				lapply(
					1:h, 
					function(x) prob.beta(m, n, k, V.a, V.b, Y.a, Y.b, pow, cons, alternative = alternative)
				)
			)
	
	vec <- p.vec >= beta
	
	res <- mean(vec)
	
	return(res)
  
}

prob.alpha <- Vectorize(prob.alpha, vectorize.args = c('V.a', 'V.b', 'Y.a', 'Y.b'))

################################################################
# setting
################################################################
# repeatation
rp <- 1000

# Monte Carlo simulation
h <- 1000 # number of simulating beta
m <- 100 # number of simulating blocks
k <- 100 # number of simulating Y

# The first plot
n1 <- 3 #block size
V.a1 <- 1 #shape parameter for gamma distribution for V
V.b1 <- 2 #scale parameter for gamma distribution for V
Y.a1 <- V.a1 #shape parameter for gamma distribution for Y
Y.b1 <- V.b1 #scale parameter for gamma distribution for Y
beta1 <- 0.9
cons1 <- 5.788

alternative1 = '2-sided' # or 'upper' or 'lower'

# The second plot
n2 <- 3 #block size
V.a2 <- 1 #shape parameter for gamma distribution for V 
V.b2 <- 2 #scale parameter for gamma distribution for V
Y.a2 <- V.a2 #shape parameter for gamma distribution for Y
Y.b2 <- V.b2 #scale parameter for gamma distribution for Y
beta2 <- 0.9
cons2 <- 5.788

alternative2 = '2-sided' # or 'upper' or 'lower'

#define the range of y-axis the plots (default is between 0 and 100)
y.min = 0
y.max = 100

#define the range of bins for x-axis (default is between 0.85 and 1 with 0.01 step)
bins <- seq(0.85, 1, 0.01)

################################################################
#run two empirical distributions
################################################################

result <- matrix(NA, nrow = rp, ncol = 2)
V.a <- c(V.a1, V.a2)
V.b <- c(V.b1, V.b2)
Y.a <- c(Y.a1, Y.a2)
Y.b <- c(Y.b1, Y.b2)

start <- Sys.time()

for (ii in 1:rp){

	result[ii, ] <- prob.alpha(h = h, m = m, n = n1, k = k, 
		V.a = V.a, V.b = V.b, Y.a = Y.a, Y.b = Y.b, pow = 1/3, cons = cons1, 
		beta = beta1, alternative = alternative1)

}

end <- Sys.time()

end - start

#################################################################
##run histograms
#################################################################

#statistics for the first empirical distribution
mean(result1)
sd(result1)

#statistics for the second empirical distribution
mean(result2)
sd(result2)

#make the side-by-side plots 
par(mfrow = c(1, 2))
hist(result1, breaks = bins, freq = FALSE, ylim = c(y.min, y.max))
text(median(bins), 100, paste('mean = ', round(mean(result1), 4), sep = ''))
text(median(bins), 90, paste('sd = ', round(sd(result1), 4), sep = ''))
text(median(bins), 80, paste('n = ', n1, sep = ''))
text(median(bins), 70, paste('df = ', V.a1, sep = ''))
text(median(bins), 60, paste('df = ', V.b1, sep = ''))

hist(result2, breaks = bins, freq = FALSE, ylim = c(y.min, y.max))
text(median(bins), 100, paste('mean = ', round(mean(result2), 4), sep = ''))
text(median(bins), 90, paste('sd = ', round(sd(result2), 4), sep = ''))
text(median(bins), 80, paste('n = ', n2, sep = ''))
text(median(bins), 70, paste('df = ', Y.a2, sep = ''))
text(median(bins), 60, paste('df = ', Y.b2, sep = ''))


#make the boxplot 
par(mfrow = c(1, 1))
result1 <- cbind(result1, 1)
result2 <- cbind(result2, 2)
result <- as.data.frame(rbind(result1, result2))
names(result) <- c('result', 'group')
boxplot(result ~ group, data = result, names=c("res1","result2"))

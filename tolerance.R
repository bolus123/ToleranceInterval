library(parallel)

prob.beta <- function(m, n, k, V.df, Y.df, pow, cons) {

	#V <- matrix(rchisq(m * n, V.df), nrow = m, ncol = n) 

	V <- matrix(rchisq(m * n, V.df), nrow = m, ncol = n) / 2

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
	    Y <- rchisq(k, Y.df) / 2
	    Y <- Y^(pow)
	    mean(x[1] < Y & Y < x[2])
	    
	  }
	)
	
	p
  
}

#a <- prob.beta(m1, n1, k, V.df1, Y.df1, pow1, 14.87) 


prob.alpha <- function(h, m, n, k, V.df, Y.df = V.df, pow, cons, beta) {

	p.vec <- unlist(
				lapply(
					1:h, 
					function(x) prob.beta(m, n, k, V.df, Y.df, pow, cons)
				)
			)
	
	vec <- p.vec >= beta
	
	res <- mean(vec)
	
	return(res)
  
}



################################################################
# setting
################################################################

h <- 1000000
k <- 100
m <- 100
n <- 3
df.seq <- c(1, 3, 6, 18)
beta <- 0.9
cons <- 5.788

prob.alpha <- Vectorize(prob.alpha, vectorize.args = 'V.df')

prob.alpha(h = h, m = m, n = n, k = k, V.df = df.seq, pow = 1/3, cons = cons, beta = beta)








################################################################
# setting
################################################################
# setting cores
#cores <- detectCores() - 1

# sample size for Y
#k <- 100

# The second plot
#h1 <- 100
#m1 <- 100
#n1 <- 3
#V.df1 <- 3
#Y.df1 <- V.df1
#pow1 <- 1/3
#cons1 <- 14.87
#beta1 <- 0.8

# The second plot
#h2 <- 100
#m2 <- 100
#n2 <- 3
#V.df2 <- 10
#Y.df2 <- V.df2
#pow2 <- 1/3
#cons2 <- 14.87
#beta2 <- 0.8

# sample sizes of empirical distributions for histograms
#rp <- 1000

#define the range of y-axis the plots (default is between 0 and 100)
#y.min = 0
#y.max = 100

#define the range of bins for x-axis (default is between 0.85 and 1 with 0.01 step)
#bins <- seq(0.85, 1, 0.01)


################################################################
#run two empirical distributions
################################################################
#
#start.time <- Sys.time()
#
#cl <- makeCluster(cores)
#
#clusterExport(cl, c('prob.beta', 'prob.alpha', 'k', 'h1', 'm1', 'n1',
#	'V.df1', 'Y.df1', 'pow1', 'cons1', 'beta1', 'h2', 'm2', 'n2',
#	'V.df2', 'Y.df2', 'pow2', 'cons2', 'beta2'
#))
#
#
#res1 <- unlist(
#			parLapply(
#				cl, 
#				1:rp,
#				function(x) {
#					prob.alpha(h = h1, m = m1, n = n1, k = k, 
#						V.df = V.df1, Y.df = Y.df1, pow = pow1, cons = cons1, beta = beta1)
#				}
#			)
#		)
#
##res1 <- rep(NA, rp)
##
##for (i in 1:rp){
##  
##  res1[i] <- prob.alpha(h = h1, m = m1, n = n1, k = k, 
##  		V.df = V.df1, Y.df = Y.df1, pow = pow1, cons = cons1, beta = beta1)
##  
##}
#
#res2 <- unlist(
#			parLapply(
#				cl, 
#				1:rp,
#				function(x) {
#					prob.alpha(h = h2, m = m2, n = n2, k = k, 
#						V.df = V.df2, Y.df = Y.df2, pow = pow2, cons = cons2, beta = beta2)
#				}
#			)
#		)
#
#stopCluster(cl)
#
#end.time <- Sys.time()
#
#proc.time <- end.time - start.time
#
#################################################################
##run histograms
#################################################################
#
##statistics for the first empirical distribution
#mean(res1)
#sd(res1)
#
##statistics for the second empirical distribution
#mean(res2)
#sd(res2)
#
##make the side-by-side plots 
#par(mfrow = c(1, 2))
#hist(res1, breaks = bins, freq = FALSE, ylim = c(y.min, y.max))
#text(median(bins), 80, paste('mean = ', round(mean(res1), 4), sep = ''))
#text(median(bins), 70, paste('sd = ', round(sd(res1), 4), sep = ''))
#text(median(bins), 60, paste('n = ', n1, sep = ''))
#text(median(bins), 60, paste('df = ', V.df1, sep = ''))
#
#hist(res2, breaks = bins, freq = FALSE, ylim = c(y.min, y.max))
#text(median(bins), 80, paste('mean = ', round(mean(res2), 4), sep = ''))
#text(median(bins), 70, paste('sd = ', round(sd(res2), 4), sep = ''))
#text(median(bins), 60, paste('n = ', n2, sep = ''))
#text(median(bins), 60, paste('df = ', Y.df1, sep = ''))
#
#
##make the boxplot 
#par(mfrow = c(1, 1))
#res1 <- cbind(res1, 1)
#res2 <- cbind(res2, 2)
#res <- as.data.frame(rbind(res1, res2))
#names(res) <- c('res', 'group')
#boxplot(res ~ group, data = res, names=c("res1","res2"))

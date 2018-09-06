

##########  (1) Secant method  ########## 

### Secant method (increasing) ### 

secantc <- function(fun, x0, x1, tol=1e-10, niter=10000000){   
  for ( i in 1:niter ) {
    funx1 <- fun(x1)
    funx0 <- fun(x0)
    x2 <- ( (x0*funx1) - (x1*funx0) )/( funx1 - funx0 )
    funx2 <- fun(x2)
    if (abs(funx2) < tol) {
      return(x2)  
    }
    if (funx2 < 0) 
      x0 <- x2     ####convex function
    else
      x1 <- x2     ####concave function
  }
  stop("exceeded allowed number of iteractions")
}

### Secant method (decreasing) ### 

secant <- function(fun, x0, x1, tol=1e-10, niter=100000){  
  for ( i in 1:niter ) {
    funx1 <- fun(x1)
    funx0 <- fun(x0)
    x2 <- ( (x0*funx1) - (x1*funx0) )/( funx1 - funx0 )
    funx2 <- fun(x2)
    if (abs(funx2) < tol) {
      return(x2)  
    }
    if (funx2 < 0) 
      x1 <- x2   ####convex function
    else
      x0 <- x2   ####concave function
  }
  stop("exceeded allowed number of iteractions")
}

########## (2) confidence of the TI [gamma_val=(P(G(Y)>=t)] ##########

gamma_val <- function (t,alpha_star,m,n) {
  
  Y0<-m*((n-1)^2)*log((qchisq(1-(alpha_star/2),n-1))/(qchisq((alpha_star/2),n-1)))/(qchisq(1-(alpha_star/2),n-1)-(qchisq((alpha_star/2),n-1)))
  
  max_G<-pchisq((Y0/(m*(n-1)))*qchisq(1-(alpha_star/2),n-1),n-1)-pchisq((Y0/(m*(n-1)))*qchisq(alpha_star/2,n-1),n-1)
  
  G_function <- function (Y) {
    a <-pchisq((Y/(m*(n-1)))*qchisq(1-(alpha_star/2),n-1),n-1)-pchisq((Y/(m*(n-1)))*qchisq(alpha_star/2,n-1),n-1)
    return(a)
  }
  
  G_functionsec <- function (g_func) {
    k <- G_function(g_func)-t
    return(k)
  }
  
  if (t<max_G) {
    y1 <- secantc(G_functionsec,1.0e-50,Y0)    
    y2 <- secant(G_functionsec,Y0,1.0e+50)  
    c <- pchisq(y2,m*(n-1))-pchisq(y1,m*(n-1))
  }
  else {c <- 0}
  return(c)
}

##########  (3) Find the alpha*two (alpha_star) ########## 

find_alpha_star<- function(m,n,alpha,nom_gamma){
  
  
  G_quantile<- function(m,n,alpha_star,nom_gamma){  
    gamma_valsec <- function (s) {
      a <- gamma_val(s,alpha_star,m,n)-nom_gamma
      return (a)
    }
    d<-secantc(gamma_valsec,1-alpha,1) 
    d    
  }                
  
  
  alpha_ref<-2*(1-pchisq(m*(n-1)*qchisq(1-alpha,n-1)/qchisq(1-nom_gamma,m*(n-1)),n-1))
  if(alpha_ref<alpha){
    ref_Uval<-alpha_ref
  }
  if(alpha_ref>=alpha){
    ref_Uval<-alpha
  }
  if(alpha_ref-(alpha/5)>0){
    ref_Lval<-ref_Uval-(alpha/5)
  }
  if(alpha_ref-(alpha/5)<=0){
    ref_Lval<-0.00001
  }
  
  
  alpha_adj <- function (alpha_star) {  
    b <- G_quantile(m,n,alpha_star,nom_gamma)-(1-alpha)  
    return (b)
  }
  resp<-secantc(alpha_adj,ref_Lval,ref_Uval) 
  
  Ltwo<-qchisq(resp/2,n-1)/(n-1)
  Utwo<-qchisq(1-(resp/2),n-1)/(n-1)
  vect_resp<-c(1-resp,Ltwo,Utwo)
  return(vect_resp)
} 



##########  INSTRUCTIONS  ##########

##### First step: Run the Code (1)-(3) provided above ####### 

##### Second step: Use the function "find_alpha_star(m,n,alpha,nom_gamma)" from Code (3) called "Find the alpha*two", insert the values of m, n, alpha and nom_gamma ####### 

##### Third step: The output of the Second step is the vector (1-alpha*two,L*two,U*two), that is, the 1-alpha star, and the lower and the upper tolerance factors  ####### 

##### EXAMPLE: If we consider find_alpha_star(m=25,n=5,alpha=0.05,nom_gamma=0.95), we obtain: (1-alpha*two=0.97444508,L*two=0.08452931,U*two=3.17776208)  ##########

##### NOTE: If we study a distribution of the sample variance (wich comes from a sample of size n) using a single sample of N observations to estimate sigma^2, we should consider m=(N-1)/(n-1) in our code #######



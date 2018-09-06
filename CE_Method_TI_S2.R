


############################ (1) Secant method ########## https://rpubs.com/aaronsc32/secant-method-r


secant.method <- function(f, x0, x1, tol=1e-10, n_int=10000000){   ###100000  ### tol=1e-10
  for ( i in 1:n_int ) {
    fx1 <- f(x1)
    fx0 <- f(x0)
    x2 <- ( (x0*fx1) - (x1*fx0) )/( fx1 - fx0 )
    fx2 <- f(x2)
    if (abs(fx2) < tol) {
      return(x2)  ##### the search value will be the first one that is less than tol value (it can be + or -)
    }
    if (fx2 < 0) 
      x0 <- x2     ####convex function
    else
      x1 <- x2     ####concave function
  }
  stop("out of the specified n_int")
}

####################### (2) using Wilson HIlferton approximation #####################
WH_gamma_approx<-function(alpha,alpha_star,m,n) {
  
  
  EXPRESS<-function(u){
    U_two<-qchisq(1-alpha_star/2,n-1)/(n-1)
    L_two<-qchisq(alpha_star/2,n-1)/(n-1)
    A<-(((U_two*qchisq(u,m*(n-1))/(m*(n-1)))^(1/3))-(1-(2/(9*(n-1)))))/((2/(9*(n-1)))^0.5)
    B<-(((L_two*qchisq(u,m*(n-1))/(m*(n-1)))^(1/3))-(1-(2/(9*(n-1)))))/((2/(9*(n-1)))^0.5)
    a_b<-(((U_two*(n-1))^(1/3))-((L_two*(n-1))^(1/3)))^3
    cte<-(((n-1)^0.5)*m*16*(2^0.5))/27
    invchi<-qchisq(p=1-alpha,df=1,ncp=((A+B)/2)^2)
    expr<-pchisq(cte*(invchi^(3/2))/a_b,(m*(n-1)))
    return(expr)
  }
  integrate_value<-integrate(EXPRESS,lower=0,upper=1,rel.tol = .Machine$double.eps^0.13)
  d<-integrate_value$value
  return(1-d)
}


######################

############################ (3) Finding the alpha_star using the CE method ##########################



find1_alpha_star<- function(m,n,gamma,alpha){
  
  find2_alpha_star <- function (alpha_star) {  ###alpha=alpha*_two
    b <- WH_gamma_approx(alpha,alpha_star,m,n)-(gamma)   ##using Wilson HIlferton approximation 1 ####   #((1+0.2)*0.0027)###alpha_tol=alpha; p=1-gamma
    return (b)
  }
  
  alpha_ref<-2*(1-pchisq(m*(n-1)*qchisq(1-alpha,n-1)/qchisq(1-gamma,m*(n-1)),n-1))
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
    ref_Lval<-0.000001
  }
  
  
  resp<-secant.method(find2_alpha_star,ref_Lval,ref_Uval) ### 0.01,0.03  ###0.0009,0.002 ##0.000003572776
  L_adj<-qchisq(resp/2,n-1)/(n-1)
  U_adj<-qchisq(1-(resp/2),n-1)/(n-1)  
  answ<-c(1-resp,L_adj,U_adj)
  
  return(answ)
  
} 
########################################

##########  INSTRUCTIONS  ##########

##### First step: Run the Code (1)-(3) provided above ####### 

##### Second step: Use the function "find1_alpha_star(m,n,gamma,alpha)" from Code (3) called "Finding the alpha_star using the CE method", insert the values of m, n, gamma and alpha ####### 

##### Third step: The output of the Second step is the vector (1-alpha*two,L*two,U*two), that is, the alpha star, and the lower and the upper tolerance factors based on the CE method  ####### 

##### EXAMPLE: If we consider find1_alpha_star(m=25,n=5,gamma=0.95,alpha=0.05), we obtain: (1-alpha*two=0.97466208,L*two=0.08414861,U*two=3.18269628)  ##########






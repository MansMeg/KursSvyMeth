# Internal functions, simulated data for demonstrations etc.


# Calculate estimates with standard errors with simple random sampling (SRS)
ySRS <- function(n, N, ybar, s2){
  results <- list(est = ybar, 
                  SE = sqrt((s2/n)*(1-n/N)))
                  return(results)
}

tSRS <- function(n, N, ybar, s2){
  results <- list(est = ybar*N, 
                  SE = N*sqrt((s2/n)*(1-n/N)))
                  return(results)
}

pSRS <- function(n, N, p){
  results <- list(est = p, 
                  SE = sqrt(p*(1-p)*(1/(n-1))*(1-n/N)))
                  return(results)
}


# Data for examples in Lectures
# Gastroenteritis example F4
generateGastro <- function(mySeed){
  set.seed(mySeed) 
  # Create example
  N <- c(1687283, 1851959, 1812691, 2935231)
  n <- round(400.1*(N/sum(N)))
  
  e.y<-c(3.5,0.7,1.8,0.4) 
  age<-c(0,15,25,40,65) 
  y.mean<-numeric(length(n)) 
  y.s<-numeric(length(n))
  for(i in 1:length(n)){   
    y.temp<-round(rexp(n[i],1/e.y[i]))   
    y.mean[i]<-round(mean(y.temp),2)  
    y.s[i]<-round(sd(y.temp),2) 
    x.temp<-round(runif(n=n[i],min=age[i],max=age[i+1]))   
    if(i==1){y<-y.temp;x<-x.temp}   
    if(i>1){y<-c(y,y.temp);x<-c(x,x.temp)} 
  }
  gastro <- list()
  gastro$N <- N
  gastro$n <- n
  gastro$y <- y
  gastro$x <- x
  gastro$age <- age
  gastro$y_bar <- y.mean
  gastro$y_bar_all <- round(mean(y),2) 
  gastro$s_y <- y.s
  gastro$s_y_all <- round(sd(y),2) 
  return(gastro) 
}
gastro <- generateGastro(20130206)




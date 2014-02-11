# Skapar ett enkelt OSU med samtliga kombinationer, 
# samt beräknar y_hat, s_hat samt KI

samplingDist<-function(y, n, alpha=0.05){
  # y: vektor av värden (ex. c(5,10,8,7,12))
  # n: urvalsstorlek
  dist<-as.data.frame(t(combn(y,n)))
  names(dist)[1:n]<-paste("Obs.",1:n,sep="")
  dist$P_S<-1/choose(length(y),n)
  
  # Beräknar y_hat och s_hat
  dist$y_hat<-apply(dist[,1:n],MARGIN=1,FUN=mean)
  dist$s_hat<-apply(dist[,1:n],MARGIN=1,FUN=sd)
  
  # Beräknar KI
  dist$SE_hat<-sqrt((dist$s_hat^2/n)*(1-n/length(y)))
  dist$t<-qt(1-alpha/2,df=n-1)
  dist$KI.low<-dist$y_hat-dist$t*dist$SE_hat
  dist$KI.up<-dist$y_hat+dist$t*dist$SE_hat
  dist$in.KI<-ifelse(dist$KI.low< mean(y)& dist$KI.up>mean(y),1,0)
  return(dist)
}

samplingDistRatio<-function(y,x,n,alpha=0.05){
  # y: vektor av värden (ex. c(5,10,8,7,12))
  # x: vektor med hjälpvariabelvärden 
  # n: urvalsstorlek
  # Funktionen kräver följande paket
  
  # Assert
  stopifnot(length(y) == length(x))
  
  N<-length(y)
  dist <- as.data.frame(t(combn(y, n)))
  distX <- as.data.frame(t(combn(x, n)))
  names(dist)[1:n]<-paste("obs.",1:n,sep="")
  
  # Beräknar urvalssannolikheten
  dist$P_S<-1/choose(length(y),n)
  
  # Beräknar y_hat och y_hat_r
  dist$n<-n
  dist$t_x<-sum(x)
  dist$x_bar<-mean(x)
  dist$t_hat_x<-apply(distX[,1:n],MARGIN=1,FUN=mean)*N
  dist$t_hat_y<-apply(dist[,1:n],MARGIN=1,FUN=mean)*N
  dist$B_hat<-dist$t_hat_y/dist$t_hat_x
  dist$t_hat_yr<-dist$B_hat*dist$t_x
  dist$yr_hat <- dist$B_hat * dist$x_bar  

  distE<-dist[,1:n]-dist$B_hat*distX[,1:n]
  dist$s2_e<-apply(distE[,1:n],MARGIN=1,FUN=sd)
  
  return(dist)
}


samplingDistReg<-function(y,x,n,alpha=0.05){
  # y: vektor av värden (ex. c(5,10,8,7,12))
  # x: vektor med hjälpvariabelvärden (måste vara lika lång som y)
  # n: urvalsstorlek n <- 6
  stopifnot(length(x) == length(y)) 
  stopifnot(n > 2 & n < length(y)) 
  
  no.obs<-1:length(y)
  N<-length(y)
  
  dist <- as.data.frame(t(combn(y, n)))
  distX <- as.data.frame(t(combn(x, n)))
  names(dist)[1:n]<-paste("obs.",1:n,sep="")
  
  regFun<-function(vec,...) lm(formula=vec[1:n]~vec[(n+1):(2*n)])$coefficients  
  temp<-t(apply(cbind(dist[,1:n],distX[,1:n]), MARGIN=1, FUN=regFun))
  colnames(temp)<-c("B0","B1")
  temp <- as.data.frame(temp)
  
  # Beräknar urvalssannolikheten
  dist$P_S<-1/choose(length(y),n)
  
  # Beräknar y_hat, y_hat_r
  dist$n<-n
  dist$truemean_x<-mean(x)
  dist$mean_hat_x<-apply(distX[,1:n],MARGIN=1,FUN=mean)
  dist$mean_hat_y<-apply(dist[,1:n],MARGIN=1,FUN=mean)
  dist <- cbind(dist, temp)
  dist$mean_hat_yreg<-dist$mean_hat_y+dist$B1*(dist$truemean_x-dist$mean_hat_x)
  
  return(dist)
}

# test <- samplingDistReg(y=y, x=x, n=5)
# 
# hist(test$mean_hat_yreg)
# hist(test$mean_hat_y)

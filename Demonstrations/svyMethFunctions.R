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



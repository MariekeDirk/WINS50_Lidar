#'Power curve
#'@description simplified version of the power curve
#'@param Ui velocities for which the power is estimated
#'@return relative power production at each Ui
#'Last updated: October 10, 2018
#'modified by Marieke on November 19, 2020 to R-code
#'Following J. King (2014)
#'@export
power_curve<-function(Ui){
U = seq(0,25,by=1)
P = c(0, 0, 0, 0.0043, 0.0323,
      0.0771, 0.1426, 0.2329, 0.3528,
      0.5024, 0.6732, 0.8287, 0.9264,
      0.9774, 0.9946, 0.999, 0.9999, 1, 1, 1, 1, 1, 1, 1, 1, 1)

Pi = approx(U,P,xout=Ui) #,method='spline'
Pi <- data.frame(Pi)
names(Pi)<-c("U","P")

I<-which(Ui < 3 | Ui >= 25)
if(length(I>0)){
Pi$P[I]<-0
}

return(Pi)
}

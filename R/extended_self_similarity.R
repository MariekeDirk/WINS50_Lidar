#'Structures for extended self similarity analysis
#'@description This function calculates the structures for extended self similarity of timeseries.
#'@param x data from a timeseries, for example wind measurements
#'@param DT Number of hours over which the similarity structures are caculated (only holds if the timeinterval equals 10 minutes).
#'@author Marieke Dirksen, written for R from matlab code from Sukanta Basu
#'@export
StructureFunction_NaN<-function(x,DT=6){
x<-scale(x)
N=length(x)
#The following few lines are added to match with the WTMM code
a0      = 1;
nvoie   = 10;
val<-seq(1:(DT*6))

#Get the structures
#r = R[i]
  get_S<-function(r){
    j = seq(1,(N-r));
    y = abs(x[j+r]-x[j]);

    S1 = mean(y^1,na.rm=TRUE);
    S2 = mean(y^2,na.rm=TRUE);
    S3 = mean(y^3,na.rm=TRUE);
    S4 = mean(y^4,na.rm=TRUE);
    S5 = mean(y^5,na.rm=TRUE);
    S6 = mean(y^6,na.rm=TRUE);

    data.frame("S1"=S1,"S2"=S2,"S3"=S3,"S4"=S4,"S5"=S5,"S6"=S6,"R"=r)
    }
df_out<-lapply(val,get_S)
df_out<-do.call("rbind",df_out)
return(df_out)
}

#'Get the mean daily structures
#'@description Applies the function \link{StructureFunction_NaN} to a dataframe with columns
#'time and u. The structures are calculated from normalized u for each complete day.
#'@param df_h dataframe with columns time an u. time as POSIXct and u as numeric in m/s.
#'@param n_obs number of observations per day, set to 144 (complete day with 10 minute intervals
#'which is the standard interval of wind lidar data).
#'@author Marieke Dirksen
#'@export
get_mean_S<-function(df_h,n_obs=144){
df_h<-df_h[complete.cases(df_h),]
df_h$day<-as.Date(df_h$time)
obs_per_day<-df_h %>% group_by(day) %>% count()

days_in<-obs_per_day[which(obs_per_day$n==n_obs),]$day
if(length(days_in)==0){
  message("No complete days found, returning FALSE")
  message("Please check if the measurements time-interval equals the standard of 10 minutes")
  return(FALSE)
}

# #subset daytime/nighttime: to be written
# if(daytime==TRUE){
#   h.start<-9
#   h.stop<-15
#   df_h$hour<-hour(df_h$time)
#   # t.start<-as.POSIXct("2016-03-30 09:00:00",format="%Y-%m-%d %H:%M:%S")
#   # t.stop<-as.POSIXct("2016-03-30 15:00:00",format="%Y-%m-%d %H:%M:%S")
#   # df_sub<-df_h[which(df_h$time>t.start & df_h$time<t.stop),]
# }
#
# if(nighttime==TRUE){
#   h.start<-21
#   h.stop<-3
#   # t.start<-as.POSIXct("2016-03-30 09:00:00",format="%Y-%m-%d %H:%M:%S")
#   # t.stop<-as.POSIXct("2016-03-30 15:00:00",format="%Y-%m-%d %H:%M:%S")
#   # df_sub<-df_h[which(df_h$time>t.start & df_h$time<t.stop),]
# }
# SR<-StructureFunction_NaN(x=df_sub$u)
df_sub<-df_h[df_h$day %in% days_in,]

SR<-by(df_sub,df_sub$day,FUN=function(x){StructureFunction_NaN(x=x$u)})
SR<-do.call("rbind",SR)
SR_mn<-SR %>% group_by(R) %>% summarise(S1=mean(S1,na.rm=TRUE),
                                        S2=mean(S2,na.rm=TRUE),
                                        S3=mean(S3,na.rm=TRUE),
                                        S4=mean(S4,na.rm=TRUE),
                                        S5=mean(S5,na.rm=TRUE),
                                        S6=mean(S6,na.rm=TRUE))
return(SR_mn)
}


#Check how the R works in matlab compared to R!
#noct=8 #put back in function to run this part of the code
#@param noct noct is number of octaves... use a small value like 8 to start with.
#You can then increase it slowly to get the range.Due to the dyadic nature, the computational cost can increase significantly with large noct values.
# vec = seq(log2(a0),1)
# mat = as.matrix(seq(nvoie,(noct+log2(a0)-1/nvoie)));
# scales = sweep(mat, 2, vec, `/`)
# scales[is.infinite(scales)]<-NA
# scales<-scales[complete.cases(scales),]
# R       = unique(round(2.^scales));

#Just to have more points
#R       = 1:max(R);
# R=N-1
# val<-seq(1:max(R))




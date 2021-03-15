#'Calculate the richardson gradient from the model output
#'@description calculates the Richardson gradient number around hub height (>50 and <145).
#'@param df_harm model meteo parameteres
#'@export
calc_Rig_grad<-function(df_harm){
  df_hub<-df_harm[which(df_harm$height>50 & df_harm$height<145),]
  df_hub$Tv<-thetav(Ta=df_hub$Ta,p=df_hub$P/1000,reh=df_hub$reh*100)

  times_in<-unique(df_hub$time)
  times_in<-data.table(times_in)
  names(times_in)<-"time"
  times_in$Rig<-NA

  for(i in 1:length(times_in$time)){
    df_sub<-df_hub[which(df_hub$time==times_in$time[i]),]
    df_sub<-data.table(df_sub)
    setkey(df_sub,"height") #order the height levels
    # ggplot(df_sub,aes(Tv,height))+geom_point()+geom_smooth()
    # ggplot(df_sub,aes(ws,height))+geom_point()+geom_smooth()
    Rig<-Ri_grad(thetav = df_sub$Tv,u=df_sub$u,
                 v=df_sub$v,z=df_sub$height)
    times_in$Rig[i]<-Rig
  }

  df_hub<-merge(times_in,df_hub,by="time")
  #Subset the turbulent, transition and laminar flow cases
  I_turbulent <- df_hub$time[which(df_hub$Rig<0)]
  I_transition <- df_hub$time[which(df_hub$Rig>=0 & df_hub$Rig<=0.25)]
  I_laminar <-df_hub$time[which(df_hub$Rig>0.25)]
  return(list(I_turbulent=I_turbulent,I_transition=I_transition,I_laminar=I_laminar,Rig=df_hub))
}
#'Subset turbulent and laminar flow
#'@description Subset turbulent and laminar flow based on the time indices I. The model input will be interpolated to
#'the model levels and for each timestamp the errors will be calculated.
#'@param I times with tubulent/laminar flow in POSIXct
#'@param df_level observations from the lidar
#'@param df_harm model input
#'@export
subset_flow<-function(I,df_level,df_harm){
  df_sub<-df_level[df_level$time %in% I,]
  df_harm_sub<-df_harm[df_harm$time %in% unique(df_sub$time),]


  get_h_pred<-function(df,h_level){
    #interpolate u
    model<-lm(ws~poly(height,5),data=df)
    pred <- model %>% predict(data.frame(height=h_level))
    return(data.frame("pred"=pred,"h"=h_level))
  }

  df_h<-df_harm %>% group_by(time) %>% group_modify(~get_h_pred(.x,unique(df_sub$h))) %>% ungroup()

  #calculate the statitics from the overlapping heights
  df_com<-merge(df_sub,df_h,by=c("time","h"))

  bias<-mean(df_com$u-df_com$pred)
  RMSE<-caret::RMSE(df_com$pred, df_com$u)
  R2<-caret::R2(df_com$pred, df_com$u)
  n<-length(unique(df_com$time))
  val<-data.frame("bias"=bias,"RMSE"=RMSE,"R2"=R2,"nr"=n)
  #df_h<-df_sub[which(df_sub$h==112),] #hub height level measurements
  #df_harm_hub<-df_harm[which(df_harm$height==100),]

  df_harm_sub<-df_com %>% group_by(h) %>% summarise(u=mean(pred,na.rm=TRUE))
  df_sub<-df_com %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))
  #diurnal cycle
  df_com$hour<-hour(df_com$time)
  df_diurnal<-df_com %>% group_by(hour,h) %>% summarise(ws=mean(u,na.rm=TRUE),pred=mean(pred,na.rm=TRUE))


  return(list(df_mod=df_harm_sub,df_obs=df_sub,nr=n,df_com=df_com,validation=val,df_diurnal=df_diurnal))
}

#'Directions correlation at different height levels
#'@description calculates the correlation coefficient between the modeled and observed directions based on the cor.circular
#'function for the circular package. The Hlev are interpolated to the observed height levels.
#'@param df_harm
#'@param df_dir
#'@export
direction_cor<-function(df_harm,df_dir){
  df_harm<-data.table(df_harm)
  df_dir<-data.table(df_dir)
  setkey(df_harm,"height")
  setkey(df_dir,"h")

  get_h_pred<-function(df,h_level){
  #interpolate u
  model1<-lm(u~poly(height,5),data=df)
  pred1 <- model1 %>% predict(data.frame(height=h_level))

  #interpolate v
  model2<-lm(v~poly(height,5),data=df)
  pred2 <- model2 %>% predict(data.frame(height=h_level))

  #calculate new direction from u and v component
  dirs<-base::atan2(pred1,pred2)*(180/pi)+180
  ws<-sqrt(pred1^2+pred2^2)
  data.frame("dir_ha"=dirs,"ws"=ws,"h"=h_level)
  }

  get_diff_circular<-function(dir_ha,dir_obs){
    dir_harm<-circular(dir_ha,units = "degrees")
    dir_obs<-circular(dir_obs,units = "degrees")

    dir_diff<-abs(dir_harm-dir_obs)
    dir_diff[which(dir_diff>180)]<-360-dir_diff[which(dir_diff>180)]
    return(dir_diff)
  }

  get_cor_circular<-function(dir_ha,dir_obs){
    dir_harm<-circular(dir_ha,units = "degrees")
    dir_obs<-circular(dir_obs,units = "degrees")
    R2<-cor.circular(dir_harm,dir_obs)
    return(R2)
  }

  df_h<-df_harm %>% group_by(time) %>% group_modify(~get_h_pred(.x,unique(df_dir$h))) %>% ungroup

  #calculate the statitics from the overlapping heights
  df_com<-merge(df_dir,df_h,by=c("time","h"))

  #fix the 2 functions for correlation and difference group_by time and h
  # R2_t<- df_com %>% group_by(time) %>% group_map(~get_cor_circular(.x$dir_ha,.x$Direction))
  # R2_h<- df_com %>% group_by(h) %>% group_map(~get_cor_circular(.x$dir_ha,.x$Direction))
  R2 <- get_cor_circular(df_com$dir_ha,df_com$dir)
  df_com$dir_dirr<-get_diff_circular(df_com$dir_ha,df_com$dir)

return(list(R2=R2,dir_diff=df_com))

}

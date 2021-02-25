read_Cabauw<-function(dir="D:/data/Lidar/Cabauw/",h=50,what="Speed"){


  h_opt<-c(11,20,39,80,140,200,252)#heights for Daarle
  h_exists<-h %in% h_opt
  if(h_exists==FALSE){
    message("Height level not included returning FALSE, try 50,65,80,100,126,135,145,155,180,200,220 or 235")
    return(FALSE)
  }
  df<-fread(paste0(dir,"ZephIRdata_Cabauw_20180215_20200229.csv"))
  t.vec<-df$`Time and Date`
  t.vec<-gsub("[A-Z]"," ",t.vec)
  t.vec<-as.POSIXct(t.vec,format="%Y-%m-%d %H:%M:%S") #



  if(what=="Speed"){
    I<-df[,grep(paste0("u",h,"$"), colnames(df))]
    u<-subset(df,select=I)
    df_h<-cbind(t.vec,u)
    df_h$h<-h
    names(df_h)<-c("time","u","h")
    df_h$u<-as.numeric(df_h$u)
    # heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))

    # I.h<-which(df_height$height==h)
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)
  }

  if(what=="Direction"){
    I<-df[,grep(paste0("d",h,"$"), colnames(df))]
    u<-subset(df,select=I)
    df_h<-cbind(t.vec,u)
    df_h$h<-h
    names(df_h)<-c("time","dir","h")
    df_h$dir<-as.numeric(df_h$dir)
    # heights<-as.numeric(gsub("[^0-9.-]", "", names(u)))
    # I.h<-which(df_height$height==h)
    df_h<-df_h[stats::complete.cases(df_h),]
    return(df_h)

  }
}

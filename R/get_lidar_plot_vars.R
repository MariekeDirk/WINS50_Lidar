get_lidar_plot_vars<-function(lidar="BWFZ",Location=1){
  if(lidar=="TNW"){
    if(Location==1){
    main_dir<-cnf$tnw_dir
    fname<-"D:/Afbeeldingen/Lidar/TNWA_data_snapshot.png"
    loc="TNWA"
    }
    if(Location==2){
    main_dir<-cnf$tnw_dir
    fname<-"D:/Afbeeldingen/Lidar/TNWB_data_snapshot.png"
    loc="TNWB"
    }

  height_levels<-cnf$h_TNW
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_TNW,h=height_levels,MoreArgs = list(dir=main_dir,stn=loc),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2

  df_h<-read_TNW(dir=main_dir,h=100,what="Speed",stn=loc)
  df_spd<-df_h
  df_dir<-read_TNW(dir=main_dir,h=100,what = "Direction",stn=loc)

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h")) #,by=c("time","h")
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2020-03-06")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }
  if(lidar=="FINO2"){
    if(Location==1){
    main_dir<-cnf$fino2_dir
    fname<-"D:/Afbeeldingen/Lidar/FINO2_data_snapshot.png"
    }

  height_levels<-cnf$h_FINO2
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_FINO2,h=height_levels,MoreArgs = list(dir=main_dir),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_FINO2(dir=main_dir,h=107,what="Speed")
  df_spd<-df_h
  df_dir<-read_FINO2(dir=main_dir,h=107,what = "Direction")

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2013-03-30")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  I_out<-grep(x=strftime(df_diurnal$HM_labs,format="%M"),pattern="2")
  df_diurnal<-df_diurnal[-I_out,]
  }
  if(lidar=="EPL"){
    if(Location==1){
    main_dir<-cnf$epl_dir
    fname<-"D:/Afbeeldingen/Lidar/EPL_data_snapshot.png"
    }

  height_levels<-cnf$h_EPL
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_EPL,h=height_levels,MoreArgs = list(dir=main_dir),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_EPL(dir=main_dir,h=116,what="Speed")
  df_spd<-df_h
  df_dir<-read_EPL(dir=main_dir,h=116,what = "Direction")

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2016-11-20")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }
  if(lidar=="BWFZ"){

    if(Location==1){
    main_dir<-cnf$bwfz1_dir
    fname<-"D:/Afbeeldingen/Lidar/Furgo1_data_snapshot.png"
    }
    if(Location==2){
    main_dir<-cnf$bwfz2_dir
    fname<-"D:/Afbeeldingen/Lidar/Furgo2_data_snapshot.png"
    }
  height_levels<-cnf$h_BWFZ
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_BWFZ,h=height_levels,MoreArgs = list(dir=main_dir),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_BWFZ(dir=main_dir,h=100,what="Speed")
  df_spd<-df_h
  df_dir<-read_BWFZ(dir=main_dir,h=100,what = "Direction")
  # df_spd<-read_BWFZ(dir=main_dir,h=100,what = "Speed")

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2016-03-30")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }
  if(lidar=="LEG"){
    if(Location==1){
    main_dir<-cnf$leg_dir
    fname<-"D:/Afbeeldingen/Lidar/LEG_data_snapshot.png"
    }

  height_levels<-cnf$h_LEG
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_LEG,h=height_levels,MoreArgs = list(dir=main_dir),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_LEG(dir=main_dir,h=116,what="Speed")
  df_spd<-df_h
  df_dir<-read_LEG(dir=main_dir,h=116,what = "Direction")

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2016-09-09")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }
  if(lidar=="ANH"){
    if(Location==1){
    main_dir<-cnf$anh_dir
    fname<-"D:/Afbeeldingen/Lidar/ANH_data_snapshot.png"
    }

  height_levels<-cnf$h_ANH
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_ANH,h=height_levels,MoreArgs = list(dir=main_dir),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_ANH(dir=main_dir,h=100,what="Speed")
  df_spd<-df_h
  df_dir<-read_ANH(dir=main_dir,h=100,what = "Direction")

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2013-03-30")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }

  if(lidar=="HKN"){
    if(Location==1){
    main_dir<-cnf$hkn_dir
    fname<-"D:/Afbeeldingen/Lidar/HKN1_data_snapshot.png"
    loc<-"HKNA"
    }
    if(Location==2){
    main_dir<-cnf$hkn_dir
    fname<-"D:/Afbeeldingen/Lidar/HKN2_data_snapshot.png"
    loc<-"HKNB"
    }

  height_levels<-cnf$h_HKN
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_HKN,h=height_levels,MoreArgs = list(dir=main_dir,stn=loc),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_HKN(dir=main_dir,h=100,what="Speed")
  df_spd<-df_h
  df_dir<-read_HKN(dir=main_dir,h=100,what = "Direction")

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2019-04-04")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }
  if(lidar=="HKZ"){
    if(Location==1){
    main_dir<-cnf$hkz_dir
    fname<-"D:/Afbeeldingen/Lidar/HKZA_data_snapshot.png"
    loc<-"HKZA"
    }
    if(Location==2){
    main_dir<-cnf$hkz_dir
    fname<-"D:/Afbeeldingen/Lidar/HKZB_data_snapshot.png"
    loc<-"HKZB"
    }

  height_levels<-cnf$h_HKZ
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_HKZ,h=height_levels,MoreArgs = list(dir=main_dir,stn=loc),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_HKZ(dir=main_dir,h=100,what="Speed",stn=loc)
  df_spd<-df_h
  df_dir<-read_HKZ(dir=main_dir,h=100,what = "Direction",stn=loc)

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2016-10-19")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }

  if(lidar=="WMR"){
    if(Location==1){
    main_dir<-cnf$wmr_dir
    fname<-"D:/Afbeeldingen/Lidar/WMR_data_snapshot.png"
    }


  height_levels<-cnf$h_WMR
  height_colors<-viridis(n=length(height_levels))
  df_hc<-data.frame(height_levels,height_colors)
  names(df_hc)<-c("h","clr")

  #wind speed from different height levels p3,p4
  df_level<-mapply(FUN=read_WMR,h=height_levels,MoreArgs = list(dir=main_dir),SIMPLIFY = FALSE)
  df_level<-do.call("rbind",df_level)
  df_sub<-df_level %>% group_by(h) %>% summarise(u=mean(u,na.rm=TRUE))

  #for the plotting routines p1,p2
  df_h<-read_WMR(dir=main_dir,h=100,what="Speed")
  df_spd<-df_h
  df_dir<-read_WMR(dir=main_dir,h=100,what = "Direction")

  #windrose plotting routine p5
  df_in<-base::merge(df_dir,df_spd,by=c("time","h"))
  df_in<-df_in[stats::complete.cases(df_in),]

  #Day for the diurnal profile plot
  # t.day<-as.Date("2016-10-19")
  # df_diurnal<-df_level[which(as.Date(df_level$time)==t.day),]
  df_level$HM<-strftime(df_level$time, format="%H:%M")
  df_diurnal<-df_level %>% group_by(h,HM) %>% summarise(u=mean(u,na.rm=TRUE))
  df_diurnal$HM_labs<-as.POSIXct(paste0("2010-01-01 ",df_diurnal$HM))
  }

return(list(df_level=df_level,
            df_diurnal=df_diurnal,
            df_dir=df_dir,
            df_spd=df_spd,
            df_sub=df_sub,
            df_h=df_h,
            df_in=df_in,
            height_colors=height_colors))
}

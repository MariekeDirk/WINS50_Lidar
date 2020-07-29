#'Plot similarity's
#'@description Plots S3 vs S2 and S2 vs S4.
#'@param SR similarities as output from \link{get_mean_S}.
#'@param df data input from \link{get_mean_S} to add statistics to the plot.
#'@param main header of the plot
#'@author Marieke Dirksen
#'@export
plot_ESS<-function(SR,df,main){
  #subset only the days which are used in the ESS analysis
  df$day<-as.Date(df$time)
  obs_per_day<-df %>% group_by(day) %>% count()
  days_in<-obs_per_day[which(obs_per_day$n==144),]$day
  df<-df[df$day %in% days_in,]

  #Some numbers for the plotting routine
  n.obs<-length(unique(df$day))
  t.min<-min(as.Date(df$time),na.rm=TRUE)
  t.max<-max(as.Date(df$time),na.rm=TRUE)

  #Label for the plotting routine
  lab <- SR %>%
    summarise(
      S3 = 0.1,
      S2 = mean(S2),
      lab = paste0("nr.= ",n.obs,"\nmin= ",t.min,"\nmax= ",t.max)
    )

  p1<-ggplot(SR,aes(S3,S2))+  #ggtitle(main)+
    geom_point()+
    theme_bw()+
    theme(aspect.ratio = 1)+
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    geom_text(data = lab,aes(label=lab), vjust = "top", hjust = "left")+
    coord_fixed()
  p2<-ggplot(SR,aes(S2,S4))+  #ggtitle(main)+
    geom_point()+
    theme_bw()+
    theme(aspect.ratio = 1)+
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)))


  P<-grid.arrange(p1,p2,top=main,ncol=2)

  return(P)
}

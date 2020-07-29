#'Plot structure
#'@description Plots R versus S.
#'@param SR similarities as output from \link{get_mean_S}.
#'@param df data input from \link{get_mean_S} to add statistics to the plot.
#'@param main header of the plot
#'@author Marieke Dirksen
#'@importFrom magrittr %>%
#'@importFrom rlang .data
#'@export
plot_RS<-function(SR,df,main){
  #subset only the days which are used in the ESS analysis
  df$day<-as.Date(df$time)
  obs_per_day<-df %>% dplyr::group_by(.data$day) %>% dplyr::count()
  days_in<-obs_per_day[which(obs_per_day$n==144),]$day
  df<-df[df$day %in% days_in,]

  #Some numbers for the plotting routine
  n.obs<-length(unique(df$day))
  t.min<-min(as.Date(df$time),na.rm=TRUE)
  t.max<-max(as.Date(df$time),na.rm=TRUE)

  #Label for the plotting routine
  lab <- SR %>%
    dplyr::summarise(
      R = 0.7,
      S1=mean(.data$S1),
      S3 = 0.1,
      S2 = mean(.data$S2),
      S4=mean(.data$S4),
      S5=mean(.data$S5),
      S6=mean(.data$S6),
      lab = paste0("nr.= ",n.obs,"\nmin= ",t.min,"\nmax= ",t.max)
    )

  p1<-ggplot2::ggplot(SR,ggplot2::aes(.data$R/6,.data$S1))+  #ggtitle(main)+
    ggplot2::geom_point()+
    ggplot2::theme_bw()+
    ggplot2::theme(aspect.ratio = 1)+
    ggplot2::xlab(expression(Delta~t))+
    ggplot2::geom_vline(xintercept = 3,color="blue",linetype="dashed",lwd=1)+
    ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))+
    ggplot2::scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))+
    ggplot2::geom_text(data = lab,ggplot2::aes(label=lab), vjust = "top", hjust = "left")+
    ggplot2::coord_fixed()
  p2<-ggplot2::ggplot(SR,ggplot2::aes(.data$R/6,.data$S2))+  #ggtitle(main)+
    ggplot2::geom_point()+
    ggplot2::theme_bw()+
    ggplot2::theme(aspect.ratio = 1)+
    ggplot2::xlab(expression(Delta~t))+
    ggplot2::geom_vline(xintercept = 3,color="blue",linetype="dashed",lwd=1)+
    ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))+
    ggplot2::scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))

  p3<-ggplot2::ggplot(SR,ggplot2::aes(.data$R/6,.data$S3))+  #ggtitle(main)+
    ggplot2::geom_point()+
    ggplot2::theme_bw()+
    ggplot2::theme(aspect.ratio = 1)+
    ggplot2::xlab(expression(Delta~t))+
    ggplot2::geom_vline(xintercept = 3,color="blue",linetype="dashed",lwd=1)+
    ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))+
    ggplot2::scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))

  p4<-ggplot2::ggplot(SR,ggplot2::aes(.data$R/6,.data$S4))+  #ggtitle(main)+
    ggplot2::geom_point()+
    ggplot2::theme_bw()+
    ggplot2::theme(aspect.ratio = 1)+
    ggplot2::xlab(expression(Delta~t))+
    ggplot2::geom_vline(xintercept = 3,color="blue",linetype="dashed",lwd=1)+
    ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))+
    ggplot2::scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))

  p5<-ggplot2::ggplot(SR,ggplot2::aes(.data$R/6,.data$S5))+  #ggtitle(main)+
    ggplot2::geom_point()+
    ggplot2::theme_bw()+
    ggplot2::theme(aspect.ratio = 1)+
    ggplot2::xlab(expression(Delta~t))+
    ggplot2::geom_vline(xintercept = 3,color="blue",linetype="dashed",lwd=1)+
    ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))+
    ggplot2::scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))

  p6<-ggplot2::ggplot(SR,ggplot2::aes(.data$R/6,.data$S6))+  #ggtitle(main)+
    ggplot2::geom_point()+
    ggplot2::theme_bw()+
    ggplot2::theme(aspect.ratio = 1)+
    ggplot2::xlab(expression(Delta~t))+
    ggplot2::geom_vline(xintercept = 3,color="blue",linetype="dashed",lwd=1)+
    ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))+
    ggplot2::scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^.data$x),
                  labels = scales::trans_format("log10", scales::math_format(10^.data$x)))


  P<-gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,top=main,ncol=2)

  return(P)
}

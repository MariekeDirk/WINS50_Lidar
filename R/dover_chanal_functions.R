#18 & 20 WF parameterization BWFZ 1&2

load_ha<-function(ha_file){
  t0<-as.POSIXct("2015-12-21")
  nc_data<-nc_open(ha_file)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  h<-ncvar_get(nc_data,"height")
  t <- ncvar_get(nc_data, "time") #days since 2015-01-01
  t <- t0+(t*24*60*60)

  ua<-ncvar_get(nc_data,"ua") #eastward wind
  va<-ncvar_get(nc_data,"va") #northward wind
  uv<-sqrt(ua^2+va^2)


  uv<-data.table(uv)
  row.names(uv)<-h

  df<-gather_matrix(uv)
  return(df)
}

###Functions for profiles
gather_matrix<-function(m){

  df <- data.frame(
    height=rep(h, each=ncol(m)),
    time=rep(t, times=nrow(m)),
    value=as.numeric(Matrix::t(m)),
    stringsAsFactors=FALSE
  )
  return(df)
}

approxData <- function(dat,u,h){
  df<-data.frame(
    with(dat,
         approx(h, u, xout = seq(10, 200, by = 10), method = "linear")
    ),
    method = "approx()")
  return(df)
}

#' Plot the Wind Meels for the research domain
#' @description The boundaries of the domain are set to -5W, 50S, 10E, 57N.
#' Additional data from natural earth was added for the plotting routine.
#' @param update_farms TRUE or FALSE. If TRUE a new dataset from OSM is downloaded, if FALSE
#' the rds file in the data folder is loaded.
#' @return Returns a ggplot2 object which can be modified and cropped for a smaller domain.
#' @author Marieke Dirksen
#' @export
plot_OSM_wind_meels<-function(update_farms=FALSE){
work_dir<-"D:/data/WindFarms_shapes/EMODnet_HA_WindFarms_20200305/"
farms<-st_read(paste0(work_dir,"EMODnet_HA_WindFarms_pg_20200305.shp"))

countries<-rgdal::readOGR(dsn="D:/natural_earth/ne_10m_admin_0_countries",layer="ne_10m_admin_0_countries")
countries<-sp::spTransform(countries,sp::CRS("+init=epsg:4326"))
europe<-countries[countries$CONTINENT == "Europe",]
europe_sf<-sf::st_as_sf(europe)

bord<-rgdal::readOGR(dsn="D:/natural_earth/ne_10m_lakes",layer="ne_10m_lakes")
bord<-sp::spTransform(bord,sp::CRS("+init=epsg:4326"))
nl_sf<-sf::st_as_sf(bord)

if(update_farms==TRUE){
m <- c(-5, 50, 10, 57) #West, South, East, North

q <- m %>%
  osmdata::opq(timeout = 25*1000) %>%
  osmdata::add_osm_feature("generator:source", "wind")

wind_meels <- osmdata::osmdata_sf(q)
usethis::use_data(wind_meels)
}

if(update_farms==FALSE){
  utils::data("wind_meels")
}

p<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=farms,aes(color=STATUS),size = 1.5)+
  ggplot2::geom_sf(data=wind_meels$osm_points,
                 colour = "#08519c",
                 fill = "#08306b",
                 alpha = .5,
                 size = 1,
                 shape = 21)+
  ggplot2::geom_sf(data=europe_sf,fill="transparent")+
  ggplot2::geom_sf(data=nl_sf,fill="transparent")+
  ggplot2::theme_bw()+
  ggplot2::theme(legend.position = "none")+
  ggplot2::coord_sf()+
  ggplot2::scale_x_continuous(limits=c(-5,10), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits=c(50,57), expand = c(0, 0))
return(p)
}

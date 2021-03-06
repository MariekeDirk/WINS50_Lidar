% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_windmasts.R
\name{read_windmasts}
\alias{read_windmasts}
\title{Read wind mast data from the North Sea}
\usage{
read_windmasts(dir = "D:/data/Lidar/windmasts_NS/", stn = "P11-B")
}
\arguments{
\item{dir}{directory of the files (allowed to be stored in subfolders).}

\item{stn}{station name (P11-B,AWG-1,IJmond,BG-OHVS2,F3N,K13,Huibertgat,Oosterschelde,Vlakte_van_der_Raan,Lichteiland-Goeree,Europlatform)}

\item{h}{height of the measurement.}
}
\description{
Function to read the wind speed and direction from the wind masts. Data can be
downloaded from KDP platform. The files include wind - windspeed, direction,
standarddeviation at a 10 minute interval.

The original data structure of the files:
\itemize{
\item{FF_10M_10: }{is wind snelheid gem. zee herleid -> 10 m land hoogte sensor 10' eenheid m/s}
\item{DD_10: }{is wind richting gem. hoogte sensor 10' eenheid graad}
\item{DDN_10: }{is wind richting min. gem. hoogte sensor 10' eenheid graad}
\item{DD_STD_10: }{is wind richting std. dev. hoogte sensor 10' eenheid graad}
\item{DDX_10: }{is wind richting max. gem. hoogte sensor 10' eenheid graad}
\item{FF_SENSOR_10: }{is wind snelheid gemiddelde hoogte sensor 10' eenheid m/s}
\item{FF_10M_STD_10: }{is wind snelh. std. dev. herleid 10 m 10' eenheid m/s}
\item{FX_10M_10: }{is wind snelh. werkelijk max. zee herleid -> 10 m land hoogte sensor 10' eenheid m/s}
\item{FX_10M_MD_10: }{is wind snelh. zee : werkelijk max. land : marked discontinuity max. 10' eenheid m/s}
\item{FX_SENSOR_10: }{is wind snelheid werkelijk max. hoogte sensor 10' eenheid m/s}
\item{FX_SENSOR_MD_10: }{is wind snelh. luchtvaart max. hoogte sensor 10' eenheid m/s}
\item{SQUALL_10: }{is squall indicator eenheid boolean}
}
}
\author{
Marieke Dirksen
}

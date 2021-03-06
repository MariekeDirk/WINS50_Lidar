% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_HKN.R
\name{read_HKN}
\alias{read_HKN}
\title{Read the wind data from HKN}
\usage{
read_HKN(dir = "D:/data/Lidar/HKN", h = 60, what = "Speed", stn = "HKNA")
}
\arguments{
\item{dir}{directory of the files (allowed to be stored in subfolders).}

\item{h}{height of the measurement (4m,30m,40m,60m,80m,100m,120m,140m,160m,180m,200m).}

\item{what}{choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).}

\item{stn}{choose between HKNA and HKNB lidar locations.}
}
\description{
Function to read the wind speed and direction from the HKN lidar. Data can be
requested from \url{https://offshorewind.rvo.nl/windwaternh}.
}
\author{
Marieke Dirksen
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_FINO.R, R/read_FINO3.R
\name{read_FINO}
\alias{read_FINO}
\title{Read the wind data from FINO towers}
\usage{
read_FINO(dir = "D:/data/Lidar/", stn = "FINO1", h = 69, what = "Speed")

read_FINO(dir = "D:/data/Lidar/", stn = "FINO1", h = 69, what = "Speed")
}
\arguments{
\item{dir}{directory of the files (allowed to be stored in subfolders).}

\item{stn}{station name FINO1, FINO2 or FINO3}

\item{h}{height of the measurement (63m,91m,116m,141m,166m,191m,216m,241m,266m,291m).}

\item{what}{choose between Speed and Direction. Wind speed as sqrt(u^2+v^2).}
}
\description{
Function to read the wind speed and direction from the FINO2 lidar. Data can be
requested from \url{https://orsted.com/en/our-business/offshore-wind/wind-data}.

Function to read the wind speed and direction from the FINO2 lidar. Data can be
requested from \url{https://orsted.com/en/our-business/offshore-wind/wind-data}.
}
\author{
Marieke Dirksen

Marieke Dirksen
}

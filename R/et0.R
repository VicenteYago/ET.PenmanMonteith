#' ET.PenmanMonteith: A package for calculate daily Penman-Monteith Evapotranspiration from hourly data.
#'
#' This package is a shell for the function \code{\link[Evapotranspiration]{ET.PenmanMonteith}} from \code{Evapotranspiration} package written by \href{https://cran.r-project.org/web/packages/Evapotranspiration/index.html}{Danlu Guo}.
#'



#' @docType package
#' @name ET.PenmanMonteith
#' @importFrom magrittr %>%
NULL

data("constants",package = "Evapotranspiration", envir =  environment())

fix.last.day <- function(df){

  is.last.day.complete <- function(dates){
    last <- dates %>% utils::tail(1) %>% lubridate::hour()
    return(last >= 23)
  }

  date <- df[,1]
  if (!is.last.day.complete(date)){
    days <- date %>% lubridate::date()
    day.excluded <- days %>% utils::tail(1)
    mask <- lubridate::date(date) != day.excluded
    return(df[mask,])
  }
  else return(df)
}
#' Get daily Penman-Monteith Evapotranspiration from hourly data
#'
#' @param dates POSIXct dates vector.
#' @param temp numeric vector of temperature in Celsius scale.
#' @param hr numeric vector of relative humidity (\%).
#' @param uz numeric vector of wind speed in m/s.
#' @param rs numeric vector of solar irradiation in W/m2
#' @param lat latitude.
#' @param elev numeric elevation in mts.
#' @param crop character "short" (default) of "tall". See \code{\link[Evapotranspiration]{ET.PenmanMonteith}}.
#'
#' @return dataframe with columns \code{date} and \code{et0} containing the daily Penman-Monteith Reference Evapotranspiration
#'
#' @examples
#' data(meteo)
#' ET.PenmanMonteith::et0(
#'   dates = meteo$dates,
#'   temp  = meteo$temp,
#'   hr    = meteo$higro,
#'   uz    = meteo$anemo,
#'   rs    = meteo$piro,
#'   lat   = 38.04371,
#'   elev  = 313) -> et0.df
#' head(et0.df)
#' @export
et0<-function(dates, temp, hr, uz, rs, lat, elev, crop = "short"){
  input.df <- data.frame(dates = dates,
                         temp  = temp,
                         hr    = hr,
                         uz    = uz,
                         rs    = rs )

  input.df <- fix.last.day(input.df)

  dates<- input.df$dates
  temp <- input.df$temp
  hr   <- input.df$hr
  uz   <- input.df$uz
  rs   <- input.df$rs

  mi.constants <- constants
  mi.constants$lat <- lat
  mi.constants$lat_rad <- mi.constants$lat  * pi / 180
  mi.constants$Elev <- elev
  mi.constants$z  <- 2

  mi.constants$a_0 <- NULL
  mi.constants$b_0 <- NULL
  mi.constants$c_0 <- NULL
  mi.constants$d_0 <- NULL
  mi.constants$PA  <- NULL
  mi.constants$as  <- NULL
  mi.constants$bs  <- NULL

  tDEW <- weathermetrics::humidity.to.dewpoint(rh = hr,  t =  temp, temperature.metric = "celsius")

  data.frame(Year = dates %>% lubridate::year(),
             Month= dates %>% lubridate::month(),
             Day  = dates %>% lubridate::day(),
             Hour = dates %>% lubridate::hour(),
             Temp = temp,
             Tdew = tDEW,
             RH   = hr,
             uz   = uz,
             Rs   = rs*0.0864) -> climatedata

  mi.data <- Evapotranspiration::ReadInputs(varnames = c("Temp",  # Temperatura
                                                         "Tdew",  # Temperatura de rocio
                                                         "RH",    # Humedad relativa
                                                         "uz",    # Velocidad viento
                                                         "Rs"),   # Radiacion soloar
                        climatedata = climatedata,
                        constants = mi.constants,
                        stopmissing=c(10,10,3), #max % lost, max % duration, max % missing days
                        timestep = "subdaily",
                        interp_missing_days = T,
                        interp_missing_entries = T,
                        interp_abnormal = T,
                        missing_method  = "DoY average",
                        abnormal_method = "Doy average")



  results.PenmanMonteith <-Evapotranspiration::ET.PenmanMonteith(mi.data,
                                              mi.constants,
                                              solar="data",
                                              wind="yes",
                                              save.csv = "No",
                                              crop = crop)

  results.PenmanMonteith$ET.Daily

  data.frame(
            date  = zoo::index(results.PenmanMonteith$ET.Daily ) %>% lubridate::with_tz(lubridate::tz(dates[1])),
            et0   = zoo::coredata(results.PenmanMonteith$ET.Daily )) %>% return()
}

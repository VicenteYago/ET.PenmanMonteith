context("basic test")
library(ET.PenmanMonteith)


test_that("individual vs collective computation correcteness", {
  data(meteo)

  ##################### INDIVIDUAL
  meteo$doy <- as.Date(meteo$dates)
  meteo %>%
    split(f = meteo$doy) %>%
    lapply(function(m){
      ET.PenmanMonteith::et0(
        dates = m$dates,
        temp  = m$temp,
        hr    = m$higro,
        uz    = m$anemo,
        rs    = m$piro,
        lat   = 38.04371,
        elev  = 313)
    }) -> et0s

  data.frame(day = names(et0s),
             et0 = lapply(et0s, "[",2) %>% unlist()) -> et0.split
  rownames(et0.split) <- NULL

  ##################### COLLECTIVE
  ET.PenmanMonteith::et0(
    dates = meteo$dates,
    temp  = meteo$temp,
    hr    = meteo$higro,
    uz    = meteo$anemo,
    rs    = meteo$piro,
    lat   = 38.04371,
    elev  = 313) -> et0.df

  expect_equal(et0.df$et0, et0.split$et0)
})

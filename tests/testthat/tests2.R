context("basic test")
library(ET.PenmanMonteith)


test_that("hourly data", {
  data(dmeteo)
  tiempos <- dmeteo$FechaHora
  intervalos <- difftime(tiempos[-1], tiempos[-length(tiempos)], units = "hours")
  expect_equal(all(intervalos == 1), TRUE)
})

test_that("individual vs collective computation correcteness", {
    library(dplyr)
    data(dmeteo)

  diasenteros <- dmeteo %>% dplyr::count(fecha) %>%
    dplyr::filter(n == 24) %>% dplyr::pull(fecha)

  dmeteo %<>% dplyr::filter(fecha %in% diasenteros)

  ##################### INDIVIDUAL
  dmeteo %>%
    split(f = dmeteo$fecha) %>%
    lapply(function(m){
      ET.PenmanMonteith::et0(
        dates = m$FechaHora,
        temp  = m$tmed,
        hr    = m$hrmed,
        uz    = m$vvmed,
        rs    = m$radmed,
        lat   = 37.94006667,
        elev  = 56)
    }) -> et0s

  et0.split <- dplyr::bind_rows(et0s)

  ##################### COLLECTIVE
  ET.PenmanMonteith::et0(
    dates = dmeteo$FechaHora,
    temp  = dmeteo$tmed,
    hr    = dmeteo$hrmed,
    uz    = dmeteo$vvmed,
    rs    = dmeteo$radmed,
    lat   = 37.94006667,
    elev  = 56) -> et0.df

  testthat::expect_equal(et0.df$et0, et0.split$et0)
})

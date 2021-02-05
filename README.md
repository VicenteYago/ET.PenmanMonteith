# ET.PenmanMonteith


ET.PenmanMonteith: A package for calculate daily Penman-Monteith Evapotranspiration from hourly data.

This package is a shell for the function `Evapotranspiration::ET.PenmanMonteith` from `Evapotranspiration` package written by [Danlu Guo](https://cran.r-project.org/web/packages/Evapotranspiration/index.html)

Use : 

```{r}
data(meteo)
ET.PenmanMonteith::et0(
  dates = meteo$dates,
  temp  = meteo$temp,
  hr    = meteo$higro,
  uz    = meteo$anemo,
  rs    = meteo$piro,
  lat   = 38.04371,
  elev  = 313) -> et0.df
  
head(et0.df)
```

```{r}
        date      et0
1 2019-06-11 4.094914
2 2019-06-12 3.511961
3 2019-06-13 3.201071
4 2019-06-14 3.358169
5 2019-06-15 4.773715
6 2019-06-16 3.558969
```

# Loads the Shiny and leaflet libraries.
library(shiny)
library(leaflet)
library(dplyr)

# Saves the breakfast cereal dataset to the `cereals` variable.

mai <- readRDS("shiny/leaflet/mai.rds")
mai$latitude <- jitter(mai$latitude)
mai$longitude <- jitter(mai$longitude)
# allzips$college <- allzips$college * 100
# allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
# row.names(allzips) <- allzips$zipcode

cleantable <- mai %>%
  select(
    Tinh = NAME_1,
    Huyen = NAME_2,
    Xa = NAME_3,
    Songuoi = songuoi,
    Danso = danso,
    Propop = pro.pop,
    DientichM = area,
    DientichKM = area_km,
    SonguoiKM = songuoi_km,
    PropopKM = pro.pop_km,
    Ho = Ho,
    PropMai = prop.mai,
    Lat = latitude,
    Long = longitude
  )

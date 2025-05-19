
# data prep
library(tidyverse)
library(sf)
library(terra)
# Generate spatial files -------------------------------------------------------
## depth contour ---------------------------------------------------------------
if(FALSE) {
   #comment out just to dummy shinyapp.io
  #r <- terra::rast("/u3/haf/Julian/maerl/data/Olex_50mBM_GEBCO_interp.tif")
  contour <-
    r |>
    terra::as.contour(levels = c(-100, -200, -400, -500, -750, -1000, -1500, -2000, -3000, -4000)) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326)
}
# can not get this to a simple tibble, hence do:
# load("~/ShinyApps/shiny-apps/botndyr/data/data.RData")


## Iceland ---------------------------------------------------------------------
island <- geo::island

# get the flatfile data --------------------------------------------------------
data <-
  mar::tbl_mar(mar::connect_mar(), "botndyr.sample_species_wtk_v") |>
  collect(n = Inf) |>
  mutate(loc = str_remove(location, "POINT \\("),
         loc = str_remove(loc, "\\)")) |>
  separate(loc, c("lon", "lat"), sep = " ") |>
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat),
         sampling_date = as_date(sampling_date)) |>
  rename(.sid = sample_no,
         .id = species_record_id,
         n = specimen_count,
         date = sampling_date,
         z = depth,
         zt = bottom_temp,
         zs = salinity) %>%
  rename_all(~stringr::str_replace(., "taxon_", "")) |>
  select(-c(location)) |>
  # NOTE: drop na's and zero counts
  filter(!is.na(n) & n > 0)
## Station data ----------------------------------------------------------------
dx <- 2  # used for 2 lon degrees and 1 lat degree
st <-
  data |>
  dplyr::select(.sid,
                date,
                lon,
                lat,
                z,
                zt,
                zs) |>
  distinct() |>
  mutate(glon = gisland::grade(lon, dx),
         glat = gisland::grade(lat, dx / 2)) |>
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE) |>
  st_transform(crs = 3857) %>% # have to use this pipe
  bind_cols(st_coordinates(.) |> as_tibble() |> janitor::clean_names()) |>
  st_drop_geometry()
## Biological data -------------------------------------------------------------
bi <-
  data |>
  select(.sid:phylum, n) |>
  # NOTE: summarise counts within species
  group_by(.sid, name, names_is, rank, genus, family, order, class, phylum) |>
  summarise(n = sum(n),
            .groups = "drop")
save(st, bi, contour, island, file = "data/data.RData")

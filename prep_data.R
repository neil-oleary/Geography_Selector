# prep_data.R ---------------------------------------------------------------
# Processes data, before launching Shiny app.R file.
#   1. read in the Census 2022 Small Area boundaries (Tailte Éireann / CSO),
#   2. join total population from the Census 2022 SAPS small-area CSV,
#   3. simplify the Small Area geometry with mapshaper (shared borders stay aligned),
#   4. split multi-part areas into single polygons, as leafgl requires,
#   5. save the result to data/small_areas.rds for the Shiny app.
#   6. save the result to data/small_areas.geojson for use in other settings.
#
# Inputs (download and place in data/raw/):
#   - "Small Areas National Statistical Boundaries 2022" (could also use ED boundaries)
#     here I am using the "Generalised 20m" version 
#   - The Census 2022 Small Area Population Statistics (SAPS) CSV to display population in small areas
# ---------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(rmapshaper)

boundary_path <- "data/SMALL_AREA_2022_Genralised_20m_view_7288946736364367594.geojson"   # .geojson / .gpkg / .shp
# only needed if the file has several layers (see st_layers())
saps_path <- "data/SAPS_2022_Small_Area_UR_171024.csv"
out_path_rds <- "data/small_areas.rds"
out_path_geojson <- "data/small_areas.geojson"
keep_prop <- 0.25   # share of vertices to keep (1 = no simplification);


# Return the first column name in `df` matching one of `candidates`
# (case-insensitive). Shapefiles truncate names to 10 characters, so the
# candidate lists include truncated variants.

# Report how many areas still have a usable geometry after each step, and
# stop (showing the geometry types present) if none do

# Normalise codes like "SA2017_017001001" or "A017001001" to "017001001"

# --- 1. Boundaries ---------------------------------------------------------
sa <- st_read(boundary_path, quiet = TRUE) |>
  st_zm() |>
  select(sa_guid = SA_GUID_2022,
         sa_code = SA_PUB2022,
         county = COUNTY_ENGLISH,
         ed = ED_ENGLISH)

# Unique id for Leaflet (some published codes are not unique). Assigned now,
# so every part of a multi-part area keeps the same id after splitting.
sa <- sa |>
  mutate(sa_id = paste0("sa_", row_number()))
n_start <- nrow(sa)

# --- 2. Population from SAPS -----------------------------------------------
saps <- read_csv(saps_path, show_col_types = FALSE) |>
  transmute(guid_key = tolower(GUID),
            population = T1_1AGETT)   # total population, all ages

sa <- sa |>
  mutate(guid_key = tolower(sa_guid)) |>
  left_join(saps, by = "guid_key") |>
  select(-guid_key)
message(sprintf("Population matched for %d of %d small areas.",
                sum(!is.na(sa$population)), nrow(sa)))

# --- 3. Geometry clean-up and simplification --------------------------------
# All repair is done in Irish Transverse Mercator (metres) with GEOS, so
# nothing is repaired on the sphere after transforming to WGS84
sa <- sa |>
  st_transform(2157) |>
  st_make_valid() |>
  st_collection_extract("POLYGON")

# Topology-aware: neighbouring areas are simplified along the same line,
# so no gaps or slivers open up between them. If this runs out of memory,
# install the mapshaper command-line tool and add `sys = TRUE`.
sa <- sa |>
  ms_simplify(keep = keep_prop, keep_shapes = TRUE) |>
  st_make_valid() |>
  st_collection_extract("POLYGON")

# --- 4. WGS84 single-part polygons for leaflet/leafgl -------------------------
# Split every feature into single-part POLYGONs, whatever geometry type it
# has (POLYGON, MULTIPOLYGON or a GEOMETRYCOLLECTION from repair). Lines,
# points and empty parts are dropped; all parts keep the area's attributes.
sa <- sa |>
  st_set_agr("constant") |>   # silences st_cast's repeated-attributes warning
  st_transform(4326) |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON")

# Round coordinates to 5 decimal places (about 1 m). This roughly halves the
# data the app sends to each browser, and the memory it needs to do so.
sa <- sa |> 
  mutate(geometry = 
           st_sfc(lapply(geometry, function(p) st_polygon(lapply(p, round, 5))),
    crs = 4326))
message(sprintf("%d of %d small areas in output.", n_distinct(sa$sa_id), n_start))

# --- 5. Save to rds------------------------------------------------------------
saveRDS(sa, out_path_rds)

# --- 6. Save to geojson--------------------------------------------------------
st_write(sa, out_path_geojson, driver = "GeoJSON", delete_dsn = TRUE)   # GeoJSON expects WGS84 lon/lat
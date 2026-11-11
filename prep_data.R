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
library(readr)
library(rmapshaper)

boundary_path <- "data/SMALL_AREA_2022_Genralised_20m_view_7288946736364367594.geojson"   # .geojson / .gpkg / .shp
boundary_layer <- NULL  # only needed if the file has several layers (see st_layers())
saps_path     <- "data/SAPS_2022_Small_Area_UR_171024.csv"
out_path_rds      <- "data/small_areas.rds"
out_path_geojson      <- "data/small_areas.geojson"
keep_prop     <- 0.25   # share of vertices to keep (1 = no simplification);


# Return the first column name in `df` matching one of `candidates`
# (case-insensitive). Shapefiles truncate names to 10 characters, so the
# candidate lists include truncated variants.
pick_col <- function(df, candidates, required = TRUE) {
  nm  <- names(df)
  idx <- match(toupper(candidates), toupper(nm))
  idx <- idx[!is.na(idx)]
  if (length(idx)) return(nm[idx[1]])
  if (required) {
    stop(sprintf("None of [%s] found. Available columns: %s",
                 paste(candidates, collapse = ", "),
                 paste(nm, collapse = ", ")), call. = FALSE)
  }
  NA_character_
}

col_or_na <- function(df, col) {
  if (is.na(col)) rep(NA_character_, nrow(df)) else as.character(df[[col]])
}

# Report how many areas still have a usable geometry after each step, and
# stop (showing the geometry types present) if none do
n_start <- NULL
check_step <- function(x, step) {
  n <- length(unique(x$sa_id[!st_is_empty(x)]))
  message(sprintf("%-34s %6d small areas", paste0(step, ":"), n))
  if (n == 0) {
    types <- table(st_geometry_type(x), st_is_empty(x), dnn = c("type", "empty"))
    print(types)
    stop("No small areas left after step: ", step, call. = FALSE)
  }
  if (!is.null(n_start) && n < n_start) {
    warning(n_start - n, " small areas were lost at step: ", step, call. = FALSE)
  }
  invisible(x)
}

# Split every feature into single-part POLYGONs, whatever geometry type it
# has (POLYGON, MULTIPOLYGON or a GEOMETRYCOLLECTION from repair). Lines,
# points and empty parts are dropped; all parts keep the area's attributes.
polygon_parts <- function(g) {
  if (inherits(g, "POLYGON")) {
    if (length(g)) list(g) else list()
  } else if (inherits(g, "MULTIPOLYGON")) {
    Filter(length, lapply(unclass(g), st_polygon))
  } else if (inherits(g, "GEOMETRYCOLLECTION")) {
    unlist(lapply(g, polygon_parts), recursive = FALSE)
  } else {
    list()
  }
}

split_polygons <- function(x) {
  parts <- lapply(st_geometry(x), polygon_parts)
  rows  <- rep(seq_len(nrow(x)), lengths(parts))
  st_sf(st_drop_geometry(x)[rows, , drop = FALSE],
        geometry = st_sfc(unlist(parts, recursive = FALSE), crs = st_crs(x)))
}

# Normalise codes like "SA2017_017001001" or "A017001001" to "017001001"
norm_code <- function(x) sub("^[A-Za-z]+", "", sub("^.*_", "", x))

# --- 1. Boundaries ---------------------------------------------------------
sa_raw <- if (is.null(boundary_layer)) {
  st_read(boundary_path, quiet = TRUE)
} else {
  st_read(boundary_path, layer = boundary_layer, quiet = TRUE)
}
if (nrow(sa_raw) == 0) {
  stop("The boundary file has no features. If it contains several layers, ",
       "set boundary_layer to the right one (list them with st_layers()).",
       call. = FALSE)
}
sa_raw <- st_zm(sa_raw)

guid_col <- pick_col(sa_raw, c("SA_GUID_2022", "SA_GUID_20", "GUID"), required = FALSE)
code_col <- pick_col(sa_raw, c("SA_PUB2022", "SA_GEOGID_2022", "SA_GEOGID_",
                                 "SMALL_AREA", "SA_PUB2016"))
county_col <- pick_col(sa_raw, c("COUNTY_ENGLISH", "COUNTY_ENG", "COUNTYNAME",
                                 "COUNTY"), required = FALSE)
ed_col <- pick_col(sa_raw, c("ED_ENGLISH", "ED_ENGLISH_", "EDNAME", "ED_NAME"),
                       required = FALSE)

sa <- st_sf(sa_guid = col_or_na(sa_raw, guid_col),
            sa_code = as.character(sa_raw[[code_col]]),
            county = col_or_na(sa_raw, county_col),
            ed = col_or_na(sa_raw, ed_col),
            geometry = st_geometry(sa_raw))

# Unique id for Leaflet (some published codes are not unique). Assigned now,
# so every part of a multi-part area keeps the same id after splitting.
sa$sa_id <- paste0("sa_", seq_len(nrow(sa)))
check_step(sa, "Read boundaries")
n_start <- nrow(sa)

# --- 2. Population from SAPS -----------------------------------------------
saps <- read_csv(saps_path, show_col_types = FALSE)
saps_guid <- pick_col(saps, "GUID", required = FALSE)
saps_geo <- pick_col(saps, "GEOGID", required = FALSE)
pop_col <- pick_col(saps, "T1_1AGETT")   # total population, all ages

sa$population <- NA_real_
if (!is.na(guid_col) && !is.na(saps_guid)) {
  sa$population <- as.numeric(saps[[pop_col]])[
    match(tolower(sa$sa_guid), tolower(saps[[saps_guid]]))]
}
if (all(is.na(sa$population)) && !is.na(saps_geo)) {
  message("GUID join failed; falling back to matching on small area code.")
  sa$population <- as.numeric(saps[[pop_col]])[
    match(norm_code(sa$sa_code), norm_code(saps[[saps_geo]]))]
}
message(sprintf("Population matched for %d of %d small areas.",
                sum(!is.na(sa$population)), nrow(sa)))

# --- 3. Geometry clean-up and simplification --------------------------------
# All repair is done in Irish Transverse Mercator (metres) with GEOS, so
# nothing is repaired on the sphere after transforming to WGS84
sa <- st_make_valid(st_transform(sa, 2157))
sa <- split_polygons(sa)
check_step(sa, "Repair geometry")

if (keep_prop < 1) {
  # Topology-aware: neighbouring areas are simplified along the same line,
  # so no gaps or slivers open up between them. If this runs out of memory,
  # install the mapshaper command-line tool and add `sys = TRUE`.
  sa <- ms_simplify(sa, keep = keep_prop, keep_shapes = TRUE)
  sa <- split_polygons(st_make_valid(sa))
  check_step(sa, "Simplify (mapshaper)")
}

# --- 4. WGS84 single-part polygons for leaflet/leafgl -------------------------
sa <- split_polygons(st_transform(sa, 4326))

# Round coordinates to 5 decimal places (about 1 m). This roughly halves the
# data the app sends to each browser, and the memory it needs to do so.
st_geometry(sa) <- st_sfc(
  lapply(st_geometry(sa), function(p) st_polygon(lapply(p, round, 5))),
  crs = 4326
)
check_step(sa, "Transform to WGS84")

# --- 5. Save to rds------------------------------------------------------------
dir.create(dirname(out_path_rds), showWarnings = FALSE, recursive = TRUE)
saveRDS(sa, out_path_rds)

# --- 6. Save to geojson--------------------------------------------------------
sa_out <- st_transform(sa, 4326)   # GeoJSON expects WGS84 lon/lat
st_write(sa_out, out_path_geojson, driver = "GeoJSON", delete_dsn = TRUE)

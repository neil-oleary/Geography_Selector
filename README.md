# Geography_Selector

## app.R

A shiny interactive app which:

- Displays the map of Ireland with Census Small Areas (CSA) overlaid using leaflet

- The interactive display:

  1.  Allows the user to pan and zoom on the map

  2.  Allows the user to select a CSA

  3.  Displays the population of any selected CSA (based on Census 2022 data)

  4.  Allows the user to reselect another CSA or confirm their selection

## prep_data.R

- Additional script to format spatial and linked demographic data from official Central Statistics Office and Tailte Éireann source for display in the app as a processed R dataset small_areas.rds

- Spatial boundary data of CSA polygons are coarsened and simplified to enable faster display in web-based app

## Data outputs

Formatted boundary data for CSAs with linked, flattened demographic data

`data/small_areas.rds` for direct use in the Shiny app

`data/small_areas.geojson` for use in other settings
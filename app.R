# app.R ---------------------------------------------------------------------
# Census 2022 small-area picker (prototype).
# Requires data/small_areas.rds (built by prep_data.R).
# ---------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(leafgl)
library(sf)

sa_path <- "data/small_areas.rds" # location of processed geography data
responses_path <- "responses/submissions.csv"   # placeholder for prototype, there is no back-end connection with database
min_select_zoom <- 13   # areas can only be chosen at this level of zoom or closer;

if (!file.exists(sa_path)) {
  stop("data/small_areas.rds not found. Run prep_data.R first.", call. = FALSE)
}
sa <- readRDS(sa_path)
dir.create(dirname(responses_path), showWarnings = FALSE, recursive = TRUE)

# --- Helpers ----------------------------------------------------------------
fmt_pop <- function(x) {
  if (is.na(x)) "Not available" else formatC(x, format = "d", big.mark = ",")
}

area_details <- function(row) {
  tags$table(class = "sa-table",
             tags$tr(tags$th("Small area code"), tags$td(row$sa_code)),
             tags$tr(tags$th("Electoral division"), tags$td(row$ed)),
             tags$tr(tags$th("County"), tags$td(row$county)),
             tags$tr(tags$th("Population (Census 2022)"), tags$td(fmt_pop(row$population))))
}

# Desktop only: on phones the panel below the map shows these details instead

# --- Base map, built once and shared by every session -------------------------
# All small areas are drawn with WebGL (leafgl), which handles ~19,000
# polygons far faster than leaflet's standard SVG/canvas renderer.
base_map <- leaflet(options = leafletOptions(minZoom = 6, zoomControl = FALSE)) |>
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light",
                   options = providerTileOptions(maxNativeZoom = 16, maxZoom = 19)) |>
  addProviderTiles(providers$OpenStreetMap, group = "Streets") |>
  addMapPane("selected", zIndex = 450) |>   # keeps the highlight on top
  addGlPolygons(data = sa,
                layerId = sa$sa_id,
                group = "Small areas",
                color = "#4f9a80",
                fillColor = "#4f9a80",
                fillOpacity = 0.15,
                stroke = TRUE,
                label = ~paste("Small area", sa_code)) |>
  fitBounds(-10.7, 51.4, -5.9, 55.45) |>
  setMaxBounds(-12.5, 50.5, -4.5, 56.5) |>
  addLayersControl(baseGroups = c("Streets", "Light"),
                   overlayGroups = "Small areas",
                   options = layersControlOptions(collapsed = TRUE)) |>
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      // Zoom buttons bottom-right, clear of the search box
      L.control.zoom({ position: 'bottomright' }).addTo(map);
      // Keep the map sized correctly when the panel next to it changes height
      new ResizeObserver(function() { map.invalidateSize(); }).observe(el);
    }
  ")

# --- Lookups for clicks, in plain R -----------------------------------------

# --- Address search -----------------------------------------------------------
# Runs entirely in the browser: the typed address goes straight to
# OpenStreetMap's Nominatim geocoder and only moves the map. The input is
# excluded from Shiny (data-shiny-no-bind-input, and no id), so the address
# never reaches the survey server and is never recorded.
search_box <- tags$form(class = "addr-search", role = "search", autocomplete = "off",
                        tags$div(class = "addr-row",
                                 tags$input(type = "search", class = "addr-input",
                                            placeholder = "Search for an address or place",
                                            `aria-label` = "Search for an address or place",
                                            enterkeyhint = "search", autocomplete = "off",
                                            `data-shiny-no-bind-input` = NA),
                                 tags$button(type = "submit", class = "addr-go", `aria-label` = "Search",
                                             icon("magnifying-glass"))),
                        tags$p(class = "addr-status", `aria-live` = "polite"),
                        tags$ul(class = "addr-results", hidden = NA))

search_js <- "
document.addEventListener('DOMContentLoaded', function() {
  var form   = document.querySelector('.addr-search');
  var input  = form.querySelector('.addr-input');
  var status = form.querySelector('.addr-status');
  var list   = form.querySelector('.addr-results');
  var lastRequest = 0;

  function getMap() {
    var w = HTMLWidgets.find('#map');
    return w && w.getMap ? w.getMap() : null;
  }
  function hideResults() { list.hidden = true; list.innerHTML = ''; }
  function goTo(r) {
    var map = getMap();
    var bb = r.boundingbox.map(Number);   // [south, north, west, east]
    if (map) map.fitBounds([[bb[0], bb[2]], [bb[1], bb[3]]], { maxZoom: 17 });
    hideResults();
    status.textContent = '';
    input.blur();                          // closes the phone keyboard
  }

  form.addEventListener('submit', function(e) {
    e.preventDefault();
    var q = input.value.trim();
    if (q.length < 3) return;
    // Nominatim's usage policy: search on submit only, at most once a second
    var now = Date.now();
    if (now - lastRequest < 1100) return;
    lastRequest = now;

    hideResults();
    status.textContent = 'Searching\\u2026';
    var url = 'https://nominatim.openstreetmap.org/search?format=jsonv2&limit=5' +
              '&countrycodes=ie&accept-language=en&q=' + encodeURIComponent(q);
    fetch(url)
      .then(function(res) { if (!res.ok) throw new Error(res.status); return res.json(); })
      .then(function(results) {
        if (!results.length) {
          status.textContent = 'No matches. Try a street, townland or town name.';
          return;
        }
        if (results.length === 1) { goTo(results[0]); return; }
        status.textContent = '';
        results.forEach(function(r) {
          var li = document.createElement('li');
          var b = document.createElement('button');
          b.type = 'button';
          b.textContent = r.display_name;
          b.addEventListener('click', function() { goTo(r); });
          li.appendChild(b);
          list.appendChild(li);
        });
        var credit = document.createElement('li');
        credit.className = 'credit';
        credit.textContent = 'Search by OpenStreetMap Nominatim';
        list.appendChild(credit);
        list.hidden = false;
      })
      .catch(function() {
        status.textContent = 'Search isn\\u2019t available right now. You can still zoom in on the map.';
      });
  });

  input.addEventListener('input', function() { status.textContent = ''; });
  document.addEventListener('pointerdown', function(e) {
    if (!form.contains(e.target)) hideResults();
  });
});
"

# --- Styles -------------------------------------------------------------------
# Phones: full-screen map with the panel below it. Wider screens (768px+):
# panel as a left-hand sidebar.
app_css <- "
:root {
  --ink: #1f2d2b; --muted: #5f6f6b; --line: #d5dfda; --panel: #f6f8f7;
  --accent: #1d6b52; --accent-dark: #16573f; --focus: #f2b134;
}
html, body { height: 100%; margin: 0; }
body { color: var(--ink); }

.app { height: 100vh; height: 100dvh; display: flex; flex-direction: column; }
.map-wrap { position: relative; flex: 1 1 auto; min-height: 0; }
#map { position: absolute; inset: 0; height: auto !important; }

.panel {
  order: 2; background: #fff; border-top: 1px solid var(--line);
  box-shadow: 0 -2px 10px rgba(0, 0, 0, 0.08);
  padding: 12px 16px calc(12px + env(safe-area-inset-bottom));
  max-height: 45vh; max-height: 45dvh; overflow-y: auto;
}
.panel h1 { font-size: 1.05rem; font-weight: 600; margin: 0 0 6px; }
.hint { color: var(--muted); margin: 0 0 10px; }
.sa-table { margin: 2px 0 10px; }
.sa-table th { font-weight: 600; padding: 0 12px 4px 0; vertical-align: top; }
.sa-table td { padding: 0 0 4px; }
.done { margin: 10px 0 0; font-weight: 600; color: var(--accent); }

#submit {
  width: 100%; min-height: 48px; font-size: 1rem;
  background: var(--accent); border-color: var(--accent);
}
#submit:hover:enabled { background: var(--accent-dark); }
#submit:disabled { opacity: 0.45; }
#submit:focus-visible, .addr-go:focus-visible, .addr-results button:focus-visible {
  outline: 3px solid var(--focus); outline-offset: 2px;
}

.addr-search { position: absolute; z-index: 1000; top: 10px; left: 10px; right: 10px; margin: 0; }
.addr-row {
  display: flex; background: #fff; border: 1px solid var(--line);
  border-radius: 8px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
}
.addr-input {
  flex: 1; min-width: 0; border: 0; background: transparent;
  padding: 10px 12px; font-size: 16px;   /* 16px stops iOS zooming on focus */
  border-radius: 8px 0 0 8px;
}
.addr-input:focus { outline: none; }
.addr-row:focus-within { border-color: var(--accent); }
.addr-go { border: 0; background: none; width: 46px; color: var(--accent); font-size: 17px; }
.addr-status:empty { display: none; }
.addr-status {
  background: #fff; border-radius: 6px; margin: 6px 0 0; padding: 6px 10px;
  font-size: 14px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
}
.addr-results {
  list-style: none; margin: 6px 0 0; padding: 0; background: #fff;
  border: 1px solid var(--line); border-radius: 8px; max-height: 40vh;
  overflow-y: auto; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
}
.addr-results button {
  display: block; width: 100%; text-align: left; border: 0; background: none;
  padding: 11px 12px; font-size: 15px; border-bottom: 1px solid var(--line);
}
.addr-results button:hover { background: var(--panel); }
.addr-results .credit { font-size: 11px; color: var(--muted); padding: 6px 12px; }

.leaflet-touch .leaflet-bar a { width: 40px; height: 40px; line-height: 40px; }

@media (max-width: 767px) {
  .leaflet-control-layers { display: none; }
}
@media (min-width: 768px) {
  .app { display: grid; grid-template-columns: 340px 1fr; }
  .panel {
    order: 0; max-height: none; padding: 22px 20px; background: var(--panel);
    border-top: 0; border-right: 1px solid var(--line); box-shadow: none;
  }
  .panel h1 { font-size: 1.35rem; margin-bottom: 10px; }
  .addr-search { right: auto; width: 360px; }
}
"

# --- UI ---------------------------------------------------------------------
ui <- bootstrapPage(title = "Record your local area",
                    tags$head(tags$meta(name = "viewport",
                                        content = "width=device-width, initial-scale=1, viewport-fit=cover"),
                              tags$style(HTML(app_css)),
                              tags$script(HTML(search_js))),
                    tags$div(class = "app",
                             tags$aside(class = "panel",
                                        tags$h1("Which Census small area do you live in?"),
                                        uiOutput("selection_info"),
                                        actionButton("submit", "Submit my small area",
                                                     class = "btn-primary", icon = icon("check"),
                                                     disabled = TRUE),
                                        uiOutput("submission_info")),
                             tags$main(class = "map-wrap",
                                       search_box,
                                       leafletOutput("map", height = "100%"))))

# --- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  selected <- reactiveVal(NULL)
  submitted <- reactiveVal(NULL)
  
  output$map <- renderLeaflet(base_map)
  
  # Tap a small area. Too far out to pick reliably: zoom in on that spot.
  # Close enough: highlight it, show its details, enable Submit.
  # leafgl reports clicks as input$<mapId>_glify_click.
  observeEvent(input$map_glify_click, {
    click <- input$map_glify_click
    req(click$id)
    
    zoom <- input$map_zoom
    if (zoom < min_select_zoom) {
      leafletProxy("map") |>
        flyTo(click$lng, click$lat, zoom = min(zoom + 3, min_select_zoom),
              options = list(duration = 0.6))
      return()
    }
    
    shape <- filter(sa, sa_id == click$id)
    selected(st_drop_geometry(shape) |> slice(1))
    
    leafletProxy("map") |>
      clearGroup("selected") |>
      addPolygons(data = shape,
                  group = "selected",
                  color = "#c9502c",
                  weight = 3,
                  fillColor = "#c9502c",
                  fillOpacity = 0.30,
                  options = pathOptions(pane = "selected", interactive = FALSE))
    updateActionButton(session, "submit", disabled = FALSE)
  })
  
  output$selection_info <- renderUI({
    row <- selected()
    if (!is.null(row)) {
      return(tagList(p(class = "hint", "Is this where you live?"), area_details(row)))
    }
    zoom <- input$map_zoom
    if (is.null(zoom) || zoom < min_select_zoom) {
      p(class = "hint",
        "Search for your address or zoom in on the map,",
        "then tap the small area where you live.")
    } else {
      p(class = "hint", "Tap the small area where you live.")
    }
  })
  
  # Submit: record this session's selection
  observeEvent(input$submit, {
    row <- selected()
    req(row)
    
    record <- row |>
      mutate(session_id = session$token,
             submitted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")) |>
      select(session_id, submitted_at, sa_code, sa_guid, county, ed, population)
    submitted(row$sa_code)
    
    # Placeholder storage; replace when the back end is in place
    write_csv(record, responses_path, append = file.exists(responses_path))
    
    updateActionButton(session, "submit", disabled = TRUE)
  })
  
  output$submission_info <- renderUI({
    req(submitted())
    p(class = "done", role = "status", icon("circle-check"),
      sprintf(" Small area %s submitted.", submitted()))
  })
}

shinyApp(ui, server)
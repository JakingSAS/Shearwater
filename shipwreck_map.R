# ============================================================
#  Shipwreck Interactive Map — NC Shipwreck Project
#  Reads data from Shipwreck_Project_Sites.csv
#  Generates a self-contained HTML file with clickable icons.
#
#  Packages required:
#    install.packages(c("leaflet", "htmlwidgets", "htmltools",
#                       "dplyr", "readr"))
# ============================================================

library(leaflet)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(readr)

# ── 1. LOAD DATA ─────────────────────────────────────────────
raw <- read_csv("/Users/jking/Projects/Subsurface/Shipwreck_Project_Sites.csv", show_col_types = FALSE)

wrecks <- raw |>
  rename(
    name  = Name,
    lat   = Lat,
    lng   = Lon,
    url   = Notes
  ) |>
  filter(!is.na(lat) & !is.na(lng)) |>
  mutate(
    url = ifelse(!is.na(url) & startsWith(url, "http"), url, NA_character_)
  ) |>
  select(name, lat, lng, url)

cat(sprintf("Loaded %d wrecks with coordinates.\n", nrow(wrecks)))

# ── 2. Custom shipwreck icon ──────────────────────────────────────────────────
wreck_icon <- makeIcon(
  #  iconUrl = "https://cdn-icons-png.flaticon.com/512/3126/3126647.png",
  iconUrl = "/Users/jking/Projects/Subsurface/wreck.png",
  iconWidth  = 32, iconHeight = 32,
  iconAnchorX = 16, iconAnchorY = 16,
  popupAnchorX = 0, popupAnchorY = -16
)

# ── 3. BUILD POPUP HTML ──────────────────────────────────────
make_popup <- function(name, url) {
  btn <- if (!is.na(url)) {
    tags$a(
      href   = url,
      target = "_blank",
      style  = paste(
        "display:inline-block;",
        "background:#1a3a5c;",
        "color:#c8a84b;",
        "padding:5px 12px;",
        "border-radius:4px;",
        "text-decoration:none;",
        "font-size:12px;",
        "font-weight:bold;",
        "margin-top:8px;"
      ),
      "\u2693 View Details"
    )
  } else {
    tags$span(
      style = "color:#888;font-size:11px;font-style:italic;",
      "No detail page available"
    )
  }

  as.character(
    tags$div(
      style = "font-family:Georgia,serif;min-width:190px;padding:4px 2px;",
      tags$h3(
        style = "margin:0 0 6px;color:#1a3a5c;font-size:15px;",
        name
      ),
      btn
    )
  )
}

popups <- mapply(
  make_popup,
  wrecks$name, wrecks$url,
  SIMPLIFY = TRUE
)

# ── 4. BUILD THE MAP ─────────────────────────────────────────
map <- leaflet(wrecks) |>
  addProviderTiles("OpenStreetMap",     group = "Street Map") |>
  addProviderTiles("Esri.OceanBasemap", group = "Ocean Basemap") |>
  setView(
    lng  = mean(wrecks$lng, na.rm = TRUE),
    lat  = mean(wrecks$lat, na.rm = TRUE),
    zoom = 8
  ) |>
  addMarkers(
    lng          = ~lng,
    lat          = ~lat,
    icon         = wreck_icon,
    popup        = popups,
    label        = ~name,
    labelOptions = labelOptions(
      style     = list("font-weight" = "bold",
                       "font-family" = "Georgia, serif",
                       "color"       = "#1a3a5c"),
      direction = "top",
      offset    = c(0, -20)
    )
  ) |>
  addLayersControl(
    baseGroups = c("Ocean Basemap", "Street Map"),
    options    = layersControlOptions(collapsed = FALSE)
  ) |>
  addMiniMap(toggleDisplay = TRUE, minimized = FALSE) |>
  addScaleBar(position = "bottomleft")

# ── 5. SAVE TO HTML ──────────────────────────────────────────
out_file <- "shipwreck_map.html"
saveWidget(map, file = out_file, selfcontained = TRUE)
cat("\u2705  Map saved to:", normalizePath(out_file), "\n")
cat(sprintf("    %d wrecks plotted | %d with detail-page links\n",
            nrow(wrecks),
            sum(!is.na(wrecks$url))))

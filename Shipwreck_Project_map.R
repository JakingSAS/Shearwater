# ============================================================
#  Shipwreck Interactive Map
#  Generates a self-contained HTML file with clickable icons
#  that link to individual shipwreck detail pages.
#
#  Data source: CSV file (Shipwreck_Project_Sites.csv)
# ============================================================

library(dplyr)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# ── 1. Load CSV ───────────────────────────────────────────────────────────────
# Update this path to wherever your CSV file lives.
csv_path <- "/Users/jking/Projects/Subsurface/Shipwreck_Project_Sites.csv"

raw <- read.csv(csv_path, stringsAsFactors = FALSE)

# Columns in the CSV:
#   Lat / Lon   – primary decimal-degree coordinates
#   NLat / NLon – alternate decimal-degree coordinates (used when Lat/Lon is NA)
#   Name        – wreck name shown in popup
#   Country     – numeric country code (retained but not displayed)
#   Notes       – URL of the detail page for this wreck (may be blank)

dive_sites <- raw %>%
  # Coerce coordinate columns to numeric (in case they were read as character)
  mutate(
    Lat  = suppressWarnings(as.numeric(Lat)),
    Lon  = suppressWarnings(as.numeric(Lon)),
    NLat = suppressWarnings(as.numeric(NLat)),
    NLon = suppressWarnings(as.numeric(NLon))
  ) %>%
  # Use NLat/NLon as a fallback when primary coordinates are missing
  mutate(
    lat = ifelse(!is.na(Lat), Lat, NLat),
    lon = ifelse(!is.na(Lon), Lon, NLon)
  ) %>%
  # Keep only rows that have usable coordinates
  filter(!is.na(lat) & !is.na(lon)) %>%
  # Normalise the Notes column: treat blank strings as NA
  mutate(
    notes = trimws(Notes),
    notes = ifelse(notes == "" | notes == "NA", NA_character_, notes)
  ) %>%
  select(name = Name, lat, lon, notes)
#dive_sites <- filter(dive_sites, !notes == "NA")


# ── 2. Build popup labels ─────────────────────────────────────────────────────
# Sites with a URL in notes get a clickable "Learn more" link;
# others show just the name.
dive_sites <- dive_sites %>%
  mutate(
    popup = ifelse(
      !is.na(notes),
      paste0(
        '<div style="font-family:\'Georgia\',serif;min-width:min(180px,70vw);">',
        '<b style="font-size:14px;">&#9875; ', htmlEscape(name), '</b><br/><br/>',
        '<a href="', notes, '" target="_blank" ',
        'style="color:#1a6fa8;font-size:12px;">&#128214; Learn more</a>',
        '</div>'
      ),
      paste0(
        '<div style="font-family:\'Georgia\',serif;min-width:min(160px,70vw);">',
        '<b style="font-size:14px;">&#9875; ', htmlEscape(name), '</b>',
        '</div>'
      )
    )
  )

# ── 3. Custom shipwreck icon ──────────────────────────────────────────────────
# Replace iconUrl with a local file path or any publicly accessible image URL.
wreck_icon <- makeIcon(
  iconUrl     = "/Users/jking/Projects/Subsurface/wreck.png",
  iconWidth   = 32, iconHeight   = 32,
  iconAnchorX = 16, iconAnchorY  = 16,
  popupAnchorX = 0, popupAnchorY = -16
)


# ── 4. Build the map ──────────────────────────────────────────────────────────
map <- leaflet(dive_sites, options = leafletOptions(tap = TRUE)) %>%
  # Nautical base tile
  addProviderTiles(
    "Esri.OceanBasemap",
    options = providerTileOptions(noWrap = FALSE)
  ) %>%
  # Fit view to all markers
  fitBounds(
    lng1 = min(dive_sites$lon, na.rm = TRUE),
    lat1 = min(dive_sites$lat, na.rm = TRUE),
    lng2 = max(dive_sites$lon, na.rm = TRUE),
    lat2 = max(dive_sites$lat, na.rm = TRUE)
  ) %>%
  addMarkers(
    lng    = ~lon,
    lat    = ~lat,
    icon   = wreck_icon,
    popup  = lapply(dive_sites$popup, htmltools::HTML),
    label  = ~name,
    clusterOptions = markerClusterOptions(
      showCoverageOnHover = FALSE,
      zoomToBoundsOnClick = TRUE,
      spiderfyOnMaxZoom   = TRUE
    )
  ) %>%
  addControl(
    html = '<div style="
              background:rgba(0,20,40,0.85);
              color:#c8e6f5;
              padding:10px 16px;
              border-radius:6px;
              font-family:Georgia,serif;
              font-size:15px;
              border:1px solid #2a6a9a;">
              &#9875; Shipwreck Project Sites
            </div>',
    position = "topleft"
  )


# ── 5. Save to HTML ───────────────────────────────────────────────────────────
htmlwidgets::saveWidget(map, "shipwreck_map.html", selfcontained = TRUE)

# ── Mobile fix: inject viewport tag + full-height CSS ────────────────────────
# htmlwidgets omits the viewport meta tag, and its auto-sizing JS can't measure
# the viewport on iOS/Android before the map renders, causing a zero-height map.
# We patch the saved HTML to add both fixes reliably.

html_raw <- paste(readLines("shipwreck_map.html"), collapse = "\n")

# 1. Viewport meta tag (prevents desktop-scale zoom on mobile)
viewport_tag <- '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">'

# 2. Full-height CSS (forces html/body/widget container to fill the screen).
#    Without this, iOS Safari reports body height as 0 and the map is invisible.
mobile_css <- '<style>html,body{height:100%;margin:0;padding:0;}.html-fill-container,.html-widget,.leaflet,.leaflet-container{height:100%!important;min-height:100dvh;}</style>'

# Insert both right after <head>
html_raw <- sub(
  "(<head[^>]*>)",
  paste0("\\1\n", viewport_tag, "\n", mobile_css),
  html_raw,
  ignore.case = TRUE
)

writeLines(html_raw, "shipwreck_map.html")
message("Map saved to shipwreck_map.html")

# Diagnostic: check if fixes made it into the saved file
# lines <- readLines("shipwreck_map.html")
# cat("'Learn more' occurrences in HTML:", sum(grepl("Learn more", lines)), "\n")
# cat("'<a href' occurrences in HTML:   ", sum(grepl("<a href", lines)), "\n")
cat("Viewport meta tag present:       ", any(grepl("viewport", lines)), "\n")
cat("Mobile CSS present:              ", any(grepl("min-height:100dvh", lines)), "\n")
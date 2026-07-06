# ============================================================
#  Shipwreck Interactive Map
#  Generates a self-contained HTML file with clickable icons
#  that link to individual shipwreck detail pages.
#  Mobile-responsive version.
# ============================================================

library(xml2)
library(dplyr)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# ── 1. Parse XML ──────────────────────────────────────────────────────────────
doc <- read_xml("/Users/jking/Projects/Subsurface/dive sites.xml")
sites <- xml_find_all(doc, "//site")

dive_sites <- tibble(
  uuid    = trimws(xml_attr(sites, "uuid")),
  name    = as.character(xml_attr(sites, "name")),
  lat     = as.numeric(sub(" .*", "", xml_attr(sites, "gps"))),
  lon     = as.numeric(sub(".* ", "", xml_attr(sites, "gps"))),
  type    = as.character(xml_attr(sites, "description")),
  notes   = as.character(sapply(sites, \(s) {
    n <- xml_find_first(s, "notes")
    if (is.na(n)) NA_character_ else trimws(xml_text(n))
  })),
  country = as.character(sapply(sites, \(s) {
    g <- xml_find_first(s, "geo[@cat='2']")
    if (is.na(g)) NA_character_ else xml_attr(g, "value")
  }))
) %>%
  mutate(site = sub(".*-", "", name))

dive_sites <- dive_sites |>
  mutate(notes = trimws(notes))
dive_sites <- filter(dive_sites, !is.na(lat))

wrecks <- filter(dive_sites, type == "Wreck")
reefs  <- filter(dive_sites, type == "Reef")


# ── 2. Build popup labels ─────────────────────────────────────────────────────
# Popups use relative/em units and min-width in % so they scale on mobile
make_popup <- function(nm, cty, url) {
  country_html <- if (!is.na(cty))
    paste0('<span style="color:#888;font-size:0.85em;">', htmlEscape(cty), '</span><br/>')
  else ""
  
  link_html <- if (!is.na(url) && nzchar(url))
    paste0('<br/><a href="', url, '" target="_blank" ',
           'style="color:#1a6fa8;font-size:0.85em;">&#x1F4D6; Learn more</a>')
  else ""
  
  paste0(
    '<div style="font-family:Georgia,serif;min-width:min(180px,70vw);max-width:90vw;',
    'font-size:clamp(13px,3.5vw,16px);line-height:1.4;">',
    '<b>&#9875; ', htmlEscape(nm), '</b><br/>',
    country_html,
    link_html,
    '</div>'
  )
}

wreck_popups <- mapply(make_popup, wrecks$name, wrecks$country, wrecks$notes,
                       USE.NAMES = FALSE, SIMPLIFY = TRUE)
reef_popups  <- mapply(make_popup, reefs$name,  reefs$country, reefs$notes,
                       USE.NAMES = FALSE, SIMPLIFY = TRUE)


# ── 2.5. Banner ───────────────────────────────────────────────────────────────
# Uses vw-relative font size and max-width so it shrinks gracefully on narrow screens
banner_html <- "
<div style='background-color:rgba(255,255,255,0.88);
            padding:8px 12px;
            border-radius:5px;
            max-width:min(280px,80vw);
            box-sizing:border-box;'>
  <h2 style='text-align:center;
             margin:0 0 4px 0;
             font-size:clamp(14px,4vw,20px);'>
    Jays Dive Site Map
  </h2>
  <p style='text-align:center;
            margin:0;
            font-size:clamp(11px,3vw,14px);'>
    Explore the wrecks and reefs below!
  </p>
</div>"


# ── 3. Custom icons ───────────────────────────────────────────────────────────
wreck_icon <- makeIcon(
  iconUrl     = "/Users/jking/Projects/Subsurface/wreck.png",
  iconWidth   = 32, iconHeight  = 32,
  iconAnchorX = 16, iconAnchorY = 16,
  popupAnchorX = 0, popupAnchorY = -16
)

reef_icon <- makeIcon(
  iconUrl     = "/Users/jking/Projects/Subsurface/reef.png",
  iconWidth   = 32, iconHeight  = 32,
  iconAnchorX = 16, iconAnchorY = 16,
  popupAnchorX = 0, popupAnchorY = -16
)

cluster_opts <- markerClusterOptions(
  showCoverageOnHover = FALSE,
  zoomToBoundsOnClick = TRUE,
  spiderfyOnMaxZoom   = TRUE
)


# ── 4. Build the map ──────────────────────────────────────────────────────────
map <- leaflet(
  # Tell Leaflet to fill the entire viewport on mobile
  options = leafletOptions(zoomControl = TRUE)
) |>
  addProviderTiles(
    "Esri.OceanBasemap",
    options = providerTileOptions(noWrap = FALSE)
  ) |>
  fitBounds(
    lng1 = min(dive_sites$lon, na.rm = TRUE),
    lat1 = min(dive_sites$lat, na.rm = TRUE),
    lng2 = max(dive_sites$lon, na.rm = TRUE),
    lat2 = max(dive_sites$lat, na.rm = TRUE)
  ) |>
  addMarkers(
    data   = wrecks,
    lng    = ~lon, lat = ~lat,
    icon   = wreck_icon,
    popup  = lapply(wreck_popups, HTML),
    label  = ~name,
    clusterOptions = cluster_opts
  ) |>
  addMarkers(
    data   = reefs,
    lng    = ~lon, lat = ~lat,
    icon   = reef_icon,
    popup  = lapply(reef_popups, HTML),
    label  = ~name,
    clusterOptions = cluster_opts
  ) |>
  addControl(
    html = '<div style="
              background:rgba(0,20,40,0.85);
              color:#c8e6f5;
              padding:8px 12px;
              border-radius:6px;
              font-family:Georgia,serif;
              font-size:clamp(12px,3.5vw,15px);
              border:1px solid #2a6a9a;">
              ⚓ Jays Dive Site Map
            </div>',
    position = "topleft"
  ) |>
  addControl(banner_html, position = "topright", className = "banner")


# ── 5. Inject mobile-friendly <head> tags and save ───────────────────────────
# saveWidget writes the HTML, then we patch it to add the viewport meta tag
# and CSS that makes the map fill the screen on phones.

out_file <- "dive_sites_map.html"
htmlwidgets::saveWidget(map, out_file, selfcontained = TRUE)

# Read back and inject viewport + full-screen CSS right after <head>
html_txt <- readLines(out_file, warn = FALSE)

viewport_line <- '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">'

mobile_css <- paste0(
  "<style>",
  "html, body { margin:0; padding:0; height:100%; width:100%; }",
  # Make the leaflet widget container fill the viewport
  ".html-widget-static-bound, .leaflet, .leaflet-container { ",
  "  height:100vh !important; width:100vw !important; }",
  # Prevent Leaflet popups from overflowing narrow screens
  ".leaflet-popup-content { max-width:85vw !important; word-break:break-word; }",
  # Enlarge tap targets for zoom buttons on mobile
  ".leaflet-control-zoom a { width:34px !important; height:34px !important; ",
  "  line-height:34px !important; font-size:18px !important; }",
  "</style>"
)

head_idx <- which(grepl("<head>", html_txt, ignore.case = TRUE))[1]
if (!is.na(head_idx)) {
  html_txt <- c(
    html_txt[1:head_idx],
    viewport_line,
    mobile_css,
    html_txt[(head_idx + 1):length(html_txt)]
  )
}

writeLines(html_txt, out_file)
message("Mobile-responsive map saved to ", out_file)

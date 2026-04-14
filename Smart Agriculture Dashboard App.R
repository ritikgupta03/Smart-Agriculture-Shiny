library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(dplyr)
library(plotly)
library(DT)
library(leaflet)
library(scales)
library(htmltools)
library(RColorBrewer)
library(httr)
library(jsonlite)

# -------------------------------------------------
# 1) LOAD DATA
# -------------------------------------------------
# Put your CSV at: data/smart_agri_data.csv
agri <- fread("data/smart_agri_data.csv")
setDT(agri)

# Standardize names
setnames(agri, old = names(agri), new = gsub("\\.", "_", names(agri)))
setnames(agri, old = names(agri), new = trimws(names(agri)))

# Required columns fallback
required_cols <- c(
  "Year", "State", "District", "Season", "Crop", "Area", "Production", "Yield",
  "Rainfall", "Temperature", "Humidity", "Soil_Type", "Irrigation_Area"
)
for (col in required_cols) {
  if (!col %in% names(agri)) agri[, (col) := NA]
}

# Convert numeric columns safely
num_cols <- c("Year", "Area", "Production", "Yield", "Rainfall", "Temperature", "Humidity", "Irrigation_Area")
for (col in num_cols) {
  agri[, (col) := suppressWarnings(as.numeric(get(col)))]
}

# Clean text columns
char_cols <- c("State", "District", "Season", "Crop", "Soil_Type")
for (col in char_cols) {
  agri[, (col) := as.character(get(col))]
}

# Remove empty rows
agri <- agri[!(is.na(Year) & is.na(State) & is.na(Crop) & is.na(Production))]

# Fallback yield if missing
if (all(is.na(agri$Yield)) && !all(is.na(agri$Area)) && !all(is.na(agri$Production))) {
  agri[, Yield := fifelse(Area > 0, Production / Area, NA_real_)]
}

# -------------------------------------------------
# 2) INDIA STATE COORDINATES
# -------------------------------------------------
state_coords <- data.table(
  State = c(
    "Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chhattisgarh","Delhi","Goa","Gujarat",
    "Haryana","Himachal Pradesh","Jharkhand","Karnataka","Kerala","Madhya Pradesh",
    "Maharashtra","Manipur","Meghalaya","Mizoram","Nagaland","Odisha","Punjab",
    "Rajasthan","Sikkim","Tamil Nadu","Telangana","Tripura","Uttar Pradesh",
    "Uttarakhand","West Bengal","Jammu and Kashmir"
  ),
  lat = c(
    15.9129,28.2180,26.2006,25.0961,21.2787,28.7041,15.2993,22.2587,
    29.0588,31.1048,23.6102,15.3173,10.8505,22.9734,
    19.7515,24.6637,25.4670,23.1645,26.1584,20.9517,31.1471,
    27.0238,27.5330,11.1271,18.1124,23.9408,26.8467,
    30.0668,22.9868,33.7782
  ),
  lng = c(
    79.7400,94.7278,92.9376,85.3131,81.8661,77.1025,74.1240,71.1924,
    76.0856,77.1734,85.2799,75.7139,76.2711,78.6569,
    75.7139,93.9063,91.3662,92.9376,94.5624,85.0985,75.3412,
    74.2179,88.5122,78.6569,79.0193,91.9882,80.9462,
    79.0193,87.8550,76.5762
  )
)

# -------------------------------------------------
# 3) DISTRICT COORDINATES (SAMPLE GIS MAP)
# -------------------------------------------------
# Add more districts here if your dataset contains more district names.
district_coords <- data.table(
  District = c(
    "Meerut","Lucknow","Varanasi","Kanpur","Ludhiana","Amritsar","Patiala",
    "Karnal","Hisar","Rohtak","Kota","Jaipur","Bikaner","Nashik","Pune","Nagpur",
    "Patna","Gaya","Muzaffarpur","Bhopal","Indore","Jabalpur","Ahmedabad","Surat",
    "Rajkot","Mysuru","Hubli","Bengaluru","Coimbatore","Madurai","Salem"
  ),
  dlat = c(
    28.9845,26.8467,25.3176,26.4499,30.9010,31.6340,30.3398,
    29.6857,29.1492,28.8955,25.2138,26.9124,28.0229,19.9975,18.5204,21.1458,
    25.5941,24.7914,26.1209,23.2599,22.7196,23.1815,23.0225,21.1702,
    22.3039,12.2958,15.3647,12.9716,11.0168,9.9252,11.6643
  ),
  dlng = c(
    77.7064,80.9462,82.9739,80.3319,75.8573,74.8723,76.3869,
    76.9905,75.7217,76.6066,75.8648,75.7873,73.3119,73.7898,73.8567,79.0882,
    85.1376,85.0002,85.3647,77.4126,75.8577,79.9864,72.5714,72.8311,
    70.8022,76.6394,75.1240,77.5946,76.9558,78.1198,78.1460
  )
)

# Merge coordinates
agri <- merge(agri, state_coords, by = "State", all.x = TRUE)
agri <- merge(agri, district_coords, by = "District", all.x = TRUE)

# -------------------------------------------------
# 4) WEATHER API FUNCTION
# -------------------------------------------------
# Uses Open-Meteo. For city-specific output, selected state's coordinates are used.
get_weather <- function(lat_val = 28.6139, lng_val = 77.2090) {
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast?latitude=", lat_val,
    "&longitude=", lng_val,
    "&current_weather=true"
  )
  
  out <- tryCatch({
    res <- GET(url)
    txt <- content(res, "text", encoding = "UTF-8")
    dat <- fromJSON(txt)
    list(
      temperature = dat$current_weather$temperature,
      windspeed = dat$current_weather$windspeed,
      weathercode = dat$current_weather$weathercode
    )
  }, error = function(e) {
    list(temperature = NA, windspeed = NA, weathercode = NA)
  })
  
  out
}

# -------------------------------------------------
# 5) FARMER ADVISORY RULE FUNCTION
# -------------------------------------------------
farmer_advice <- function(temp, rainfall, humidity, crop_name = NULL) {
  msgs <- c()
  
  if (!is.na(rainfall)) {
    if (rainfall < 500) msgs <- c(msgs, "Low rainfall detected. Use drip irrigation, mulching, and drought-tolerant crops.")
    if (rainfall >= 500 && rainfall < 900) msgs <- c(msgs, "Moderate rainfall. Maintain balanced irrigation and monitor soil moisture.")
    if (rainfall >= 900) msgs <- c(msgs, "High rainfall zone. Ensure drainage management to avoid waterlogging.")
  }
  
  if (!is.na(temp)) {
    if (temp > 32) msgs <- c(msgs, "High temperature alert. Prefer evening irrigation and heat-stress management.")
    if (temp < 15) msgs <- c(msgs, "Cool conditions. Rabi crops may perform better in many areas.")
    if (temp >= 15 && temp <= 32) msgs <- c(msgs, "Temperature is in a workable range for many common crops.")
  }
  
  if (!is.na(humidity)) {
    if (humidity > 80) msgs <- c(msgs, "High humidity may increase fungal disease risk. Monitor leaves and soil drainage.")
    if (humidity < 35) msgs <- c(msgs, "Low humidity may increase evapotranspiration. Increase moisture conservation practices.")
  }
  
  if (!is.null(crop_name) && !is.na(crop_name) && nzchar(crop_name)) {
    msgs <- c(msgs, paste("Selected crop focus:", crop_name, "- monitor crop-specific irrigation and pest schedule."))
  }
  
  paste(unique(msgs), collapse = " ")
}

# -------------------------------------------------
# 6) CSS / THEME
# -------------------------------------------------
custom_css <- tags$style(HTML('
  .content-wrapper, .right-side {background-color:#f4f7fb;}
  .skin-blue .main-header .logo {background:#14532d; color:#fff; font-weight:700;}
  .skin-blue .main-header .navbar {background:#166534;}
  .skin-blue .main-sidebar {background:#0f172a;}
  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {background:#1e293b; border-left-color:#22c55e;}
  .box {border-radius:18px; box-shadow:0 8px 25px rgba(0,0,0,0.08); border-top:0;}
  .small-box {border-radius:18px; box-shadow:0 8px 25px rgba(0,0,0,0.08);}
  .info-card {padding:18px; border-radius:16px; background:white; box-shadow:0 8px 20px rgba(0,0,0,.06); margin-bottom:15px;}
  .advice-good {background:#ecfdf5; border-left:6px solid #22c55e; padding:14px; border-radius:12px; margin-bottom:10px;}
  .advice-warn {background:#fff7ed; border-left:6px solid #f97316; padding:14px; border-radius:12px; margin-bottom:10px;}
  .advice-bad {background:#fef2f2; border-left:6px solid #ef4444; padding:14px; border-radius:12px; margin-bottom:10px;}
  .section-title {font-size:20px; font-weight:700; margin-bottom:10px; color:#14532d;}
'))

# -------------------------------------------------
# 7) UI
# -------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Smart Agriculture Pro Dashboard"),
  dashboardSidebar(
    width = 290,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("State Map", tabName = "map_tab", icon = icon("globe-asia")),
      menuItem("District GIS Map", tabName = "district_map_tab", icon = icon("map-location-dot")),
      menuItem("Rainfall Heatmap", tabName = "heatmap_tab", icon = icon("cloud-rain")),
      menuItem("Crop Intelligence", tabName = "crop_rules", icon = icon("seedling")),
      menuItem("Weather & Farmer Advisory", tabName = "weather_tab", icon = icon("cloud-sun")),
      menuItem("Animated Trends", tabName = "animated_tab", icon = icon("play-circle")),
      menuItem("State & Crop Analysis", tabName = "analysis_tab", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "table_tab", icon = icon("table")),
      br(),
      pickerInput(
        "state_filter", "Select State(s)",
        choices = sort(unique(na.omit(agri$State))),
        selected = head(sort(unique(na.omit(agri$State))), 5),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        "crop_filter", "Select Crop(s)",
        choices = sort(unique(na.omit(agri$Crop))),
        selected = head(sort(unique(na.omit(agri$Crop))), 4),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      sliderInput(
        "year_filter", "Select Year Range",
        min = min(agri$Year, na.rm = TRUE),
        max = max(agri$Year, na.rm = TRUE),
        value = c(min(agri$Year, na.rm = TRUE), max(agri$Year, na.rm = TRUE)),
        sep = ""
      ),
      radioButtons(
        "metric_filter", "Primary Metric",
        choices = c("Production", "Yield", "Rainfall", "Temperature"),
        selected = "Production"
      ),
      br(),
      downloadButton("downloadReport", "Download Filtered Report")
    )
  ),
  dashboardBody(
    custom_css,
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_production", width = 3),
          valueBoxOutput("avg_yield", width = 3),
          valueBoxOutput("avg_rainfall", width = 3),
          valueBoxOutput("best_state", width = 3)
        ),
        fluidRow(
          valueBoxOutput("weather_box", width = 4),
          valueBoxOutput("wind_box", width = 4),
          valueBoxOutput("top_crop_box", width = 4)
        ),
        fluidRow(
          box(width = 8, title = "Yearly Performance Trend", solidHeader = TRUE, status = "success", plotlyOutput("overview_trend", height = 360)),
          box(width = 4, title = "Top Crops", solidHeader = TRUE, status = "warning", plotlyOutput("top_crops", height = 360))
        ),
        fluidRow(
          box(width = 6, title = "Production by Season", solidHeader = TRUE, status = "primary", plotlyOutput("season_plot", height = 320)),
          box(width = 6, title = "Yield vs Rainfall", solidHeader = TRUE, status = "info", plotlyOutput("yield_rain_plot", height = 320))
        )
      ),
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(width = 8, title = "State-wise Agriculture Map", solidHeader = TRUE, status = "success", leafletOutput("agri_map", height = 560)),
          box(width = 4, title = "Map Insights", solidHeader = TRUE, status = "primary", htmlOutput("map_insights"))
        )
      ),
      tabItem(
        tabName = "district_map_tab",
        fluidRow(
          box(width = 8, title = "District GIS Agriculture Map", solidHeader = TRUE, status = "warning", leafletOutput("district_map", height = 560)),
          box(width = 4, title = "District Insights", solidHeader = TRUE, status = "primary", htmlOutput("district_insights"))
        )
      ),
      tabItem(
        tabName = "heatmap_tab",
        fluidRow(
          box(width = 12, title = "Rainfall Heatmap by State and Year", solidHeader = TRUE, status = "info", plotlyOutput("rainfall_heatmap", height = 560))
        )
      ),
      tabItem(
        tabName = "crop_rules",
        fluidRow(
          box(width = 4, title = "Rule-based Crop Advisor", solidHeader = TRUE, status = "warning",
              sliderInput("rule_rainfall", "Expected Rainfall", min = 100, max = 2000, value = 800),
              sliderInput("rule_temp", "Expected Temperature", min = 5, max = 45, value = 28),
              sliderInput("rule_humidity", "Expected Humidity", min = 20, max = 100, value = 60),
              selectInput("rule_soil", "Soil Type", choices = sort(unique(na.omit(agri$Soil_Type)))),
              actionButton("run_rule_engine", "Generate Recommendation", icon = icon("wand-magic-sparkles"), class = "btn-success")
          ),
          box(width = 8, title = "Recommended Crops and Smart Advisory", solidHeader = TRUE, status = "success", htmlOutput("crop_rule_output"))
        )
      ),
      tabItem(
        tabName = "weather_tab",
        fluidRow(
          box(width = 4, title = "Live Weather", solidHeader = TRUE, status = "info", htmlOutput("weather_details")),
          box(width = 8, title = "Farmer Advisory System", solidHeader = TRUE, status = "success", htmlOutput("farmer_advisory_output"))
        )
      ),
      tabItem(
        tabName = "animated_tab",
        fluidRow(
          box(width = 12, title = "Animated Crop Trend", solidHeader = TRUE, status = "danger", plotlyOutput("animated_plot", height = 560))
        )
      ),
      tabItem(
        tabName = "analysis_tab",
        fluidRow(
          box(width = 6, title = "State Comparison", solidHeader = TRUE, status = "primary", plotlyOutput("state_compare", height = 360)),
          box(width = 6, title = "Crop Comparison", solidHeader = TRUE, status = "success", plotlyOutput("crop_compare", height = 360))
        ),
        fluidRow(
          box(width = 6, title = "Temperature vs Production", solidHeader = TRUE, status = "warning", plotlyOutput("temp_prod", height = 340)),
          box(width = 6, title = "Irrigation vs Yield", solidHeader = TRUE, status = "info", plotlyOutput("irr_yield", height = 340))
        )
      ),
      tabItem(
        tabName = "table_tab",
        fluidRow(
          box(width = 12, title = "Filtered Agriculture Data", solidHeader = TRUE, status = "primary", DTOutput("data_table"))
        )
      )
    )
  )
)

# -------------------------------------------------
# 8) SERVER
# -------------------------------------------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- agri[Year >= input$year_filter[1] & Year <= input$year_filter[2]]
    if (!is.null(input$state_filter) && length(input$state_filter) > 0) {
      df <- df[State %in% input$state_filter]
    }
    if (!is.null(input$crop_filter) && length(input$crop_filter) > 0) {
      df <- df[Crop %in% input$crop_filter]
    }
    df
  })
  
  metric_summary <- reactive({
    df <- filtered_data()
    df[, .(
      Production = sum(Production, na.rm = TRUE),
      Yield = mean(Yield, na.rm = TRUE),
      Rainfall = mean(Rainfall, na.rm = TRUE),
      Temperature = mean(Temperature, na.rm = TRUE)
    ), by = .(State, lat, lng)]
  })
  
  district_summary <- reactive({
    df <- filtered_data()
    df[, .(
      Production = sum(Production, na.rm = TRUE),
      Yield = mean(Yield, na.rm = TRUE),
      Rainfall = mean(Rainfall, na.rm = TRUE),
      Temperature = mean(Temperature, na.rm = TRUE)
    ), by = .(District, State, dlat, dlng)]
  })
  
  selected_state_coord <- reactive({
    df <- filtered_data()[!is.na(lat) & !is.na(lng)]
    if (nrow(df) == 0) return(list(lat = 28.6139, lng = 77.2090))
    list(lat = mean(df$lat, na.rm = TRUE), lng = mean(df$lng, na.rm = TRUE))
  })
  
  live_weather <- reactive({
    coord <- selected_state_coord()
    get_weather(coord$lat, coord$lng)
  })
  
  # ---------------- Value boxes ----------------
  output$total_production <- renderValueBox({
    df <- filtered_data()
    valueBox(comma(sum(df$Production, na.rm = TRUE)), "Total Production", icon = icon("industry"), color = "green")
  })
  
  output$avg_yield <- renderValueBox({
    df <- filtered_data()
    valueBox(round(mean(df$Yield, na.rm = TRUE), 2), "Average Yield", icon = icon("chart-area"), color = "yellow")
  })
  
  output$avg_rainfall <- renderValueBox({
    df <- filtered_data()
    valueBox(round(mean(df$Rainfall, na.rm = TRUE), 2), "Average Rainfall", icon = icon("cloud-rain"), color = "aqua")
  })
  
  output$best_state <- renderValueBox({
    df <- filtered_data()[, .(Production = sum(Production, na.rm = TRUE)), by = State][order(-Production)]
    best_state_name <- if (nrow(df) > 0) df$State[1] else "N/A"
    valueBox(best_state_name, "Best Performing State", icon = icon("award"), color = "purple")
  })
  
  output$weather_box <- renderValueBox({
    w <- live_weather()
    valueBox(paste0(w$temperature, " °C"), "Current Temperature", icon = icon("cloud-sun"), color = "light-blue")
  })
  
  output$wind_box <- renderValueBox({
    w <- live_weather()
    valueBox(paste0(w$windspeed, " km/h"), "Wind Speed", icon = icon("wind"), color = "teal")
  })
  
  output$top_crop_box <- renderValueBox({
    df <- filtered_data()[, .(Production = sum(Production, na.rm = TRUE)), by = Crop][order(-Production)]
    top_crop <- if (nrow(df) > 0) df$Crop[1] else "N/A"
    valueBox(top_crop, "Top Crop", icon = icon("leaf"), color = "olive")
  })
  
  # ---------------- Overview charts ----------------
  output$overview_trend <- renderPlotly({
    df <- filtered_data()[, .(
      Production = sum(Production, na.rm = TRUE),
      Yield = mean(Yield, na.rm = TRUE),
      Rainfall = mean(Rainfall, na.rm = TRUE)
    ), by = Year][order(Year)]
    
    plot_ly(df, x = ~Year) |>
      add_lines(y = ~Production, name = "Production") |>
      add_lines(y = ~Yield * 1000, name = "Yield x1000") |>
      layout(title = "Yearly Agriculture Trend", yaxis = list(title = "Value"))
  })
  
  output$top_crops <- renderPlotly({
    df <- filtered_data()[, .(Production = sum(Production, na.rm = TRUE)), by = Crop][order(-Production)][1:min(10, .N)]
    plot_ly(df, x = ~reorder(Crop, Production), y = ~Production, type = "bar") |>
      layout(xaxis = list(title = "Crop"), yaxis = list(title = "Production"))
  })
  
  output$season_plot <- renderPlotly({
    df <- filtered_data()[, .(Production = sum(Production, na.rm = TRUE)), by = Season]
    plot_ly(df, labels = ~Season, values = ~Production, type = "pie", hole = 0.45)
  })
  
  output$yield_rain_plot <- renderPlotly({
    df <- filtered_data()[!is.na(Rainfall) & !is.na(Yield)]
    plot_ly(df, x = ~Rainfall, y = ~Yield, type = "scatter", mode = "markers",
            text = ~paste("State:", State, "<br>Crop:", Crop)) |>
      layout(xaxis = list(title = "Rainfall"), yaxis = list(title = "Yield"))
  })
  
  # ---------------- State map ----------------
  output$agri_map <- renderLeaflet({
    df <- metric_summary()
    metric <- input$metric_filter
    
    df <- df[!is.na(lat) & !is.na(lng)]
    values <- df[[metric]]
    pal <- colorNumeric("YlGnBu", domain = values, na.color = "#d1d5db")
    radius_vals <- rescale(values, to = c(8, 28), from = range(values, na.rm = TRUE))
    
    leaflet(df) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = radius_vals,
        fillColor = pal(values),
        fillOpacity = 0.8,
        color = "#0f172a",
        weight = 1,
        popup = ~paste0(
          "<b>", State, "</b><br>",
          "Production: ", comma(round(Production, 2)), "<br>",
          "Yield: ", round(Yield, 2), "<br>",
          "Rainfall: ", round(Rainfall, 2), "<br>",
          "Temperature: ", round(Temperature, 2)
        )
      ) |>
      addLegend("bottomright", pal = pal, values = values, title = metric)
  })
  
  output$map_insights <- renderUI({
    df <- metric_summary()[order(-get(input$metric_filter))]
    top_state <- if (nrow(df) > 0) df$State[1] else "N/A"
    second_state <- if (nrow(df) > 1) df$State[2] else "N/A"
    
    HTML(paste0(
      "<div class='info-card'><div class='section-title'>Map Summary</div>",
      "<p><b>Primary metric:</b> ", input$metric_filter, "</p>",
      "<p><b>Top state:</b> ", top_state, "</p>",
      "<p><b>Second best:</b> ", second_state, "</p>",
      "<p>Circle size and color intensity both increase with stronger agricultural performance.</p></div>"
    ))
  })
  
  # ---------------- District GIS map ----------------
  output$district_map <- renderLeaflet({
    df <- district_summary()
    metric <- input$metric_filter
    
    df <- df[!is.na(dlat) & !is.na(dlng)]
    values <- df[[metric]]
    pal <- colorNumeric("YlOrRd", domain = values, na.color = "#d1d5db")
    radius_vals <- rescale(values, to = c(6, 20), from = range(values, na.rm = TRUE))
    
    leaflet(df) |>
      addProviderTiles(providers$Esri.WorldTopoMap) |>
      addCircleMarkers(
        lng = ~dlng, lat = ~dlat,
        radius = radius_vals,
        fillColor = pal(values),
        fillOpacity = 0.85,
        color = "#111827",
        weight = 1,
        popup = ~paste0(
          "<b>District:</b> ", District, "<br>",
          "<b>State:</b> ", State, "<br>",
          "Production: ", comma(round(Production, 2)), "<br>",
          "Yield: ", round(Yield, 2), "<br>",
          "Rainfall: ", round(Rainfall, 2), "<br>",
          "Temperature: ", round(Temperature, 2)
        )
      ) |>
      addLegend("bottomright", pal = pal, values = values, title = paste("District", metric))
  })
  
  output$district_insights <- renderUI({
    df <- district_summary()[order(-get(input$metric_filter))]
    top_dist <- if (nrow(df) > 0) df$District[1] else "N/A"
    top_state <- if (nrow(df) > 0) df$State[1] else "N/A"
    
    HTML(paste0(
      "<div class='info-card'><div class='section-title'>District GIS Summary</div>",
      "<p><b>Top district:</b> ", top_dist, "</p>",
      "<p><b>State:</b> ", top_state, "</p>",
      "<p>This map is district-point based GIS visualization using latitude and longitude coordinates.</p>",
      "<p>Add more district coordinates in <b>district_coords</b> to expand coverage.</p></div>"
    ))
  })
  
  # ---------------- Rainfall heatmap ----------------
  output$rainfall_heatmap <- renderPlotly({
    df <- filtered_data()[, .(Rainfall = mean(Rainfall, na.rm = TRUE)), by = .(State, Year)]
    heat_data <- dcast(df, State ~ Year, value.var = "Rainfall")
    z <- as.matrix(heat_data[, -1, with = FALSE])
    rownames(z) <- heat_data$State
    
    plot_ly(
      x = colnames(z),
      y = rownames(z),
      z = z,
      type = "heatmap",
      colorscale = "YlGnBu"
    ) |>
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "State"),
        title = "Rainfall Intensity Heatmap"
      )
  })
  
  # ---------------- Rule-based crop recommendation ----------------
  observeEvent(input$run_rule_engine, {
    df <- agri[!is.na(Crop)]
    
    rule_df <- df[, .(
      avg_rain = mean(Rainfall, na.rm = TRUE),
      avg_temp = mean(Temperature, na.rm = TRUE),
      avg_hum = mean(Humidity, na.rm = TRUE),
      avg_yield = mean(Yield, na.rm = TRUE),
      common_soil = names(sort(table(Soil_Type), decreasing = TRUE))[1]
    ), by = Crop]
    
    rule_df[, rain_score := 100 - abs(avg_rain - input$rule_rainfall) / max(1, input$rule_rainfall) * 100]
    rule_df[, temp_score := 100 - abs(avg_temp - input$rule_temp) / max(1, input$rule_temp) * 100]
    rule_df[, hum_score := 100 - abs(avg_hum - input$rule_humidity) / max(1, input$rule_humidity) * 100]
    rule_df[, soil_score := ifelse(common_soil == input$rule_soil, 100, 60)]
    rule_df[, yield_score := rescale(avg_yield, to = c(60, 100), from = range(avg_yield, na.rm = TRUE))]
    rule_df[, final_score := round((rain_score * 0.30) + (temp_score * 0.25) + (hum_score * 0.15) + (soil_score * 0.10) + (yield_score * 0.20), 2)]
    rule_df <- rule_df[order(-final_score)]
    
    top3 <- head(rule_df, 3)
    
    advice_block <- ""
    if (input$rule_rainfall < 500) {
      advice_block <- paste0(advice_block, "<div class='advice-bad'><b>Low Rainfall Alert:</b> Choose drought-tolerant crops and use drip irrigation.</div>")
    } else if (input$rule_rainfall < 900) {
      advice_block <- paste0(advice_block, "<div class='advice-warn'><b>Moderate Rainfall:</b> Wheat, maize, bajra, and cotton-like crops generally perform better.</div>")
    } else {
      advice_block <- paste0(advice_block, "<div class='advice-good'><b>High Rainfall Zone:</b> Water-demanding crops such as rice and sugarcane may perform well.</div>")
    }
    
    if (input$rule_temp > 32) {
      advice_block <- paste0(advice_block, "<div class='advice-warn'><b>High Temperature:</b> Use mulching, evening irrigation, and heat-stress management.</div>")
    } else if (input$rule_temp < 18) {
      advice_block <- paste0(advice_block, "<div class='advice-warn'><b>Cool Conditions:</b> Rabi-friendly crops can be suitable in many cases.</div>")
    } else {
      advice_block <- paste0(advice_block, "<div class='advice-good'><b>Balanced Temperature:</b> Broad crop suitability is possible under current conditions.</div>")
    }
    
    crop_cards <- paste0(
      apply(top3, 1, function(x) {
        paste0(
          "<div class='info-card'>",
          "<div class='section-title'>", x[["Crop"]], "</div>",
          "<p><b>Suitability Score:</b> ", x[["final_score"]], "/100</p>",
          "<p><b>Avg Rainfall Need:</b> ", round(as.numeric(x[["avg_rain"]]), 2), "</p>",
          "<p><b>Avg Temperature:</b> ", round(as.numeric(x[["avg_temp"]]), 2), "</p>",
          "<p><b>Avg Yield:</b> ", round(as.numeric(x[["avg_yield"]]), 2), "</p>",
          "<p><b>Common Soil:</b> ", x[["common_soil"]], "</p>",
          "</div>"
        )
      }),
      collapse = ""
    )
    
    output$crop_rule_output <- renderUI({
      HTML(paste0(
        advice_block,
        "<div class='section-title'>Top Recommended Crops</div>",
        crop_cards,
        "<div class='advice-good'><b>Rule Note:</b> This is a non-ML rule engine based on climate similarity, soil match, and historical average yield.</div>"
      ))
    })
  }, ignoreInit = FALSE)
  
  # ---------------- Weather and farmer advisory ----------------
  output$weather_details <- renderUI({
    w <- live_weather()
    coord <- selected_state_coord()
    HTML(paste0(
      "<div class='info-card'><div class='section-title'>Current Weather Snapshot</div>",
      "<p><b>Latitude:</b> ", round(coord$lat, 4), "</p>",
      "<p><b>Longitude:</b> ", round(coord$lng, 4), "</p>",
      "<p><b>Temperature:</b> ", w$temperature, " °C</p>",
      "<p><b>Wind Speed:</b> ", w$windspeed, " km/h</p>",
      "<p><b>Weather Code:</b> ", w$weathercode, "</p></div>"
    ))
  })
  
  output$farmer_advisory_output <- renderUI({
    df <- filtered_data()
    w <- live_weather()
    avg_rain <- mean(df$Rainfall, na.rm = TRUE)
    avg_hum <- mean(df$Humidity, na.rm = TRUE)
    focus_crop <- if (length(input$crop_filter) > 0) input$crop_filter[1] else NA_character_
    msg <- farmer_advice(w$temperature, avg_rain, avg_hum, focus_crop)
    
    HTML(paste0(
      "<div class='advice-good'><b>Farmer Advisory:</b> ", msg, "</div>",
      "<div class='advice-warn'><b>Operational Tip:</b> Use the selected filters to generate crop- and state-specific insight before taking farming decisions.</div>",
      "<div class='advice-bad'><b>Important:</b> This is a dashboard advisory system and should be used along with local agricultural guidance.</div>"
    ))
  })
  
  # ---------------- Animated chart ----------------
  output$animated_plot <- renderPlotly({
    df <- filtered_data()[, .(Production = sum(Production, na.rm = TRUE)), by = .(Year, Crop)]
    
    plot_ly(
      df,
      x = ~Crop,
      y = ~Production,
      frame = ~Year,
      type = "bar",
      text = ~paste("Year:", Year, "<br>Crop:", Crop, "<br>Production:", comma(Production)),
      hoverinfo = "text"
    ) |>
      layout(
        title = "Animated Crop Production Trend by Year",
        xaxis = list(title = "Crop"),
        yaxis = list(title = "Production")
      )
  })
  
  # ---------------- Analysis charts ----------------
  output$state_compare <- renderPlotly({
    df <- filtered_data()[, .(Production = sum(Production, na.rm = TRUE)), by = State][order(-Production)][1:min(12, .N)]
    plot_ly(df, x = ~reorder(State, Production), y = ~Production, type = "bar") |>
      layout(xaxis = list(title = "State"), yaxis = list(title = "Production"))
  })
  
  output$crop_compare <- renderPlotly({
    df <- filtered_data()[, .(Yield = mean(Yield, na.rm = TRUE)), by = Crop][order(-Yield)]
    plot_ly(df, x = ~reorder(Crop, Yield), y = ~Yield, type = "bar") |>
      layout(xaxis = list(title = "Crop"), yaxis = list(title = "Average Yield"))
  })
  
  output$temp_prod <- renderPlotly({
    df <- filtered_data()[!is.na(Temperature) & !is.na(Production)]
    plot_ly(df, x = ~Temperature, y = ~Production, type = "scatter", mode = "markers",
            text = ~paste("State:", State, "<br>Crop:", Crop)) |>
      layout(xaxis = list(title = "Temperature"), yaxis = list(title = "Production"))
  })
  
  output$irr_yield <- renderPlotly({
    df <- filtered_data()[!is.na(Irrigation_Area) & !is.na(Yield)]
    plot_ly(df, x = ~Irrigation_Area, y = ~Yield, type = "scatter", mode = "markers",
            text = ~paste("State:", State, "<br>Crop:", Crop)) |>
      layout(xaxis = list(title = "Irrigation Area"), yaxis = list(title = "Yield"))
  })
  
  # ---------------- Data table ----------------
  output$data_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---------------- Download report ----------------
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("smart_agriculture_report_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(filtered_data(), file)
    }
  )
}


shinyApp(ui, server)

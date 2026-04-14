library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

# -----------------------------
# Load Data
# -----------------------------
agri <- read.csv("data/smart_agri_data.csv", stringsAsFactors = FALSE)

# Standardize column names
names(agri) <- gsub("\\.", "_", names(agri))
names(agri) <- trimws(names(agri))

# Convert important columns safely
num_cols <- c("Year", "Area", "Production", "Yield", "Rainfall", "Temperature", "Humidity", "Irrigation_Area")
for (col in num_cols) {
  if (col %in% names(agri)) {
    agri[[col]] <- suppressWarnings(as.numeric(agri[[col]]))
  }
}

char_cols <- c("State", "District", "Season", "Crop", "Soil_Type")
for (col in char_cols) {
  if (col %in% names(agri)) {
    agri[[col]] <- as.character(agri[[col]])
  }
}

# Fill missing columns if not present
needed_cols <- c("Year","State","District","Season","Crop","Area","Production",
                 "Yield","Rainfall","Temperature","Humidity","Soil_Type","Irrigation_Area")

for (col in needed_cols) {
  if (!col %in% names(agri)) {
    agri[[col]] <- NA
  }
}

# Remove fully empty rows
agri <- agri %>%
  filter(!(is.na(Year) & is.na(State) & is.na(Crop) & is.na(Production)))

# -----------------------------
# UI
# -----------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Smart Agriculture Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Crop Analysis", tabName = "crop_analysis", icon = icon("seedling")),
      menuItem("Climate Analysis", tabName = "climate_analysis", icon = icon("cloud-sun-rain")),
      menuItem("State Comparison", tabName = "state_comparison", icon = icon("chart-bar")),
      menuItem("Smart Advisory", tabName = "smart_advisory", icon = icon("lightbulb")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table")),
      
      br(),
      selectInput("state_filter", "Select State(s):",
                  choices = c("All", sort(unique(na.omit(agri$State)))),
                  selected = "All", multiple = TRUE),
      
      selectInput("crop_filter", "Select Crop(s):",
                  choices = c("All", sort(unique(na.omit(agri$Crop)))),
                  selected = "All", multiple = TRUE),
      
      sliderInput("year_filter", "Select Year Range:",
                  min = min(agri$Year, na.rm = TRUE),
                  max = max(agri$Year, na.rm = TRUE),
                  value = c(min(agri$Year, na.rm = TRUE), max(agri$Year, na.rm = TRUE)),
                  sep = "")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------- Overview ----------------
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_production"),
                valueBoxOutput("total_area"),
                valueBoxOutput("avg_yield")
              ),
              fluidRow(
                valueBoxOutput("avg_rainfall"),
                valueBoxOutput("avg_temp"),
                valueBoxOutput("top_crop")
              ),
              fluidRow(
                box(width = 6, title = "Production Trend", status = "primary", solidHeader = TRUE,
                    plotlyOutput("production_trend")),
                box(width = 6, title = "Top 10 Crops by Production", status = "success", solidHeader = TRUE,
                    plotlyOutput("top_crops_plot"))
              )
      ),
      
      # ---------------- Crop Analysis ----------------
      tabItem(tabName = "crop_analysis",
              fluidRow(
                box(width = 4, title = "Crop Selection", status = "warning", solidHeader = TRUE,
                    selectInput("crop_single", "Choose Crop:",
                                choices = sort(unique(na.omit(agri$Crop)))))
              ),
              fluidRow(
                box(width = 6, title = "Yearly Production of Selected Crop", status = "primary", solidHeader = TRUE,
                    plotlyOutput("crop_yearly_plot")),
                box(width = 6, title = "State-wise Yield of Selected Crop", status = "success", solidHeader = TRUE,
                    plotlyOutput("crop_state_yield_plot"))
              )
      ),
      
      # ---------------- Climate Analysis ----------------
      tabItem(tabName = "climate_analysis",
              fluidRow(
                box(width = 6, title = "Rainfall vs Production", status = "info", solidHeader = TRUE,
                    plotlyOutput("rainfall_production_plot")),
                box(width = 6, title = "Temperature vs Yield", status = "danger", solidHeader = TRUE,
                    plotlyOutput("temp_yield_plot"))
              ),
              fluidRow(
                box(width = 12, title = "Yearly Climate Trend", status = "primary", solidHeader = TRUE,
                    plotlyOutput("climate_trend_plot"))
              )
      ),
      
      # ---------------- State Comparison ----------------
      tabItem(tabName = "state_comparison",
              fluidRow(
                box(width = 12, title = "State Comparison", status = "primary", solidHeader = TRUE,
                    plotlyOutput("state_compare_plot"))
              ),
              fluidRow(
                box(width = 6, title = "Top States by Production", status = "success", solidHeader = TRUE,
                    plotlyOutput("top_states_plot")),
                box(width = 6, title = "Top States by Yield", status = "warning", solidHeader = TRUE,
                    plotlyOutput("top_yield_states_plot"))
              )
      ),
      
      # ---------------- Smart Advisory ----------------
      tabItem(tabName = "smart_advisory",
              fluidRow(
                box(width = 12, title = "Rule-Based Agriculture Advisory", status = "danger", solidHeader = TRUE,
                    htmlOutput("advisory_output"))
              )
      ),
      
      # ---------------- Data Table ----------------
      tabItem(tabName = "data_table",
              fluidRow(
                box(width = 12, title = "Filtered Agriculture Data", status = "primary", solidHeader = TRUE,
                    DTOutput("agri_table"))
              )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- agri %>%
      filter(Year >= input$year_filter[1], Year <= input$year_filter[2])
    
    if (!("All" %in% input$state_filter)) {
      df <- df %>% filter(State %in% input$state_filter)
    }
    
    if (!("All" %in% input$crop_filter)) {
      df <- df %>% filter(Crop %in% input$crop_filter)
    }
    
    df
  })
  
  # ---------------- Value Boxes ----------------
  output$total_production <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = comma(sum(df$Production, na.rm = TRUE)),
      subtitle = "Total Production",
      icon = icon("industry"),
      color = "green"
    )
  })
  
  output$total_area <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = comma(sum(df$Area, na.rm = TRUE)),
      subtitle = "Total Cultivated Area",
      icon = icon("draw-polygon"),
      color = "blue"
    )
  })
  
  output$avg_yield <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = round(mean(df$Yield, na.rm = TRUE), 2),
      subtitle = "Average Yield",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$avg_rainfall <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = round(mean(df$Rainfall, na.rm = TRUE), 2),
      subtitle = "Average Rainfall",
      icon = icon("cloud-rain"),
      color = "aqua"
    )
  })
  
  output$avg_temp <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = round(mean(df$Temperature, na.rm = TRUE), 2),
      subtitle = "Average Temperature",
      icon = icon("temperature-high"),
      color = "red"
    )
  })
  
  output$top_crop <- renderValueBox({
    df <- filtered_data()
    top_crop_name <- df %>%
      group_by(Crop) %>%
      summarise(prod = sum(Production, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(prod)) %>%
      slice(1) %>%
      pull(Crop)
    
    if (length(top_crop_name) == 0) top_crop_name <- "N/A"
    
    valueBox(
      value = top_crop_name,
      subtitle = "Top Crop",
      icon = icon("leaf"),
      color = "purple"
    )
  })
  
  # ---------------- Overview plots ----------------
  output$production_trend <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Year) %>%
      summarise(Production = sum(Production, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(df, aes(x = Year, y = Production)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(x = "Year", y = "Production")
    
    ggplotly(p)
  })
  
  output$top_crops_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Crop) %>%
      summarise(Production = sum(Production, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Production)) %>%
      slice_head(n = 10)
    
    p <- ggplot(df, aes(x = reorder(Crop, Production), y = Production)) +
      geom_col() +
      coord_flip() +
      labs(x = "Crop", y = "Production")
    
    ggplotly(p)
  })
  
  # ---------------- Crop analysis ----------------
  output$crop_yearly_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(Crop == input$crop_single) %>%
      group_by(Year) %>%
      summarise(Production = sum(Production, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(df, aes(x = Year, y = Production)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(title = paste("Production Trend -", input$crop_single),
           x = "Year", y = "Production")
    
    ggplotly(p)
  })
  
  output$crop_state_yield_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(Crop == input$crop_single) %>%
      group_by(State) %>%
      summarise(Yield = mean(Yield, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Yield)) %>%
      slice_head(n = 10)
    
    p <- ggplot(df, aes(x = reorder(State, Yield), y = Yield)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Top States by Yield -", input$crop_single),
           x = "State", y = "Yield")
    
    ggplotly(p)
  })
  
  # ---------------- Climate analysis ----------------
  output$rainfall_production_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(!is.na(Rainfall), !is.na(Production))
    
    p <- ggplot(df, aes(x = Rainfall, y = Production, text = paste("State:", State, "<br>Crop:", Crop))) +
      geom_point(alpha = 0.7) +
      labs(x = "Rainfall", y = "Production")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$temp_yield_plot <- renderPlotly({
    df <- filtered_data() %>%
      filter(!is.na(Temperature), !is.na(Yield))
    
    p <- ggplot(df, aes(x = Temperature, y = Yield, text = paste("State:", State, "<br>Crop:", Crop))) +
      geom_point(alpha = 0.7) +
      labs(x = "Temperature", y = "Yield")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$climate_trend_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Year) %>%
      summarise(
        Avg_Rainfall = mean(Rainfall, na.rm = TRUE),
        Avg_Temperature = mean(Temperature, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Avg_Rainfall, linetype = "Rainfall"), linewidth = 1) +
      geom_line(aes(y = Avg_Temperature, linetype = "Temperature"), linewidth = 1) +
      labs(x = "Year", y = "Average Value", linetype = "Metric")
    
    ggplotly(p)
  })
  
  # ---------------- State comparison ----------------
  output$state_compare_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(State) %>%
      summarise(
        Production = sum(Production, na.rm = TRUE),
        Yield = mean(Yield, na.rm = TRUE),
        Rainfall = mean(Rainfall, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Production)) %>%
      slice_head(n = 15)
    
    p <- ggplot(df, aes(x = reorder(State, Production), y = Production)) +
      geom_col() +
      coord_flip() +
      labs(x = "State", y = "Production")
    
    ggplotly(p)
  })
  
  output$top_states_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(State) %>%
      summarise(Production = sum(Production, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Production)) %>%
      slice_head(n = 10)
    
    p <- ggplot(df, aes(x = reorder(State, Production), y = Production)) +
      geom_col() +
      coord_flip() +
      labs(x = "State", y = "Production")
    
    ggplotly(p)
  })
  
  output$top_yield_states_plot <- renderPlotly({
    df <- filtered_data() %>%
      group_by(State) %>%
      summarise(Yield = mean(Yield, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Yield)) %>%
      slice_head(n = 10)
    
    p <- ggplot(df, aes(x = reorder(State, Yield), y = Yield)) +
      geom_col() +
      coord_flip() +
      labs(x = "State", y = "Yield")
    
    ggplotly(p)
  })
  
  # ---------------- Smart advisory ----------------
  output$advisory_output <- renderUI({
    df <- filtered_data()
    
    avg_rain <- mean(df$Rainfall, na.rm = TRUE)
    avg_temp <- mean(df$Temperature, na.rm = TRUE)
    avg_yld  <- mean(df$Yield, na.rm = TRUE)
    
    latest_year <- max(df$Year, na.rm = TRUE)
    
    latest_df <- df %>% filter(Year == latest_year)
    latest_prod <- sum(latest_df$Production, na.rm = TRUE)
    
    overall_prod_by_year <- df %>%
      group_by(Year) %>%
      summarise(Production = sum(Production, na.rm = TRUE), .groups = "drop") %>%
      arrange(Year)
    
    decline_msg <- ""
    if (nrow(overall_prod_by_year) >= 2) {
      last_prod <- tail(overall_prod_by_year$Production, 1)
      prev_prod <- tail(overall_prod_by_year$Production, 2)[1]
      if (!is.na(last_prod) && !is.na(prev_prod) && last_prod < prev_prod) {
        decline_msg <- "<li><b>Production Alert:</b> Latest year production is lower than previous year. Consider crop diversification and better input planning.</li>"
      }
    }
    
    rain_msg <- if (!is.na(avg_rain) && avg_rain < 800) {
      "<li><b>Water Advisory:</b> Rainfall is low. Prefer drip irrigation, mulching, and drought-resistant crops.</li>"
    } else {
      "<li><b>Water Advisory:</b> Rainfall is acceptable. Maintain balanced irrigation scheduling.</li>"
    }
    
    temp_msg <- if (!is.na(avg_temp) && avg_temp > 30) {
      "<li><b>Heat Advisory:</b> Temperature is high. Use shade nets, timely irrigation, and heat-tolerant crop varieties.</li>"
    } else {
      "<li><b>Heat Advisory:</b> Temperature is within a moderate range for many crops.</li>"
    }
    
    yield_msg <- if (!is.na(avg_yld) && avg_yld < mean(agri$Yield, na.rm = TRUE)) {
      "<li><b>Yield Advisory:</b> Average yield is below overall dataset average. Review soil health, fertilizer use, and irrigation methods.</li>"
    } else {
      "<li><b>Yield Advisory:</b> Yield performance is satisfactory. Focus on maintaining current best practices.</li>"
    }
    
    HTML(paste0(
      "<h4>Smart Agriculture Recommendations</h4>",
      "<ul>",
      rain_msg,
      temp_msg,
      yield_msg,
      decline_msg,
      "<li><b>General Advice:</b> Monitor rainfall, temperature, and yield together before making seasonal crop decisions.</li>",
      "</ul>"
    ))
  })
  
  # ---------------- Data table ----------------
  output$agri_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

# -----------------------------
# Run App
# -----------------------------
shinyApp(ui, server)
# Jessica Byrd (jb2799)
# Activity: App V3 
# Due 5.5.26
# Air Quality data
# App uses (4) data sets: air_quality.csv, sites.rds, bg.jeogson, and metro.rds


# App Description
# This app shows the Air Quality Trends for PM2.5 concentration levels across 
# (5) NYC boroughs for 2024- 2025. Users can filter by borough, year, and season
# to see how PM2.5 levels vary over time.The app includes multiple visualizations
# such as monthly trends, seasonal patterns, and before vs. after comparisons of 
# congestion pricing. It also uses spatial analysis through borough-level maps
# and clustering in order to highlight geographical differences in PM2.5 levels.
# The app is in Workspace/ sts - jb2799 
# cloud> project > Activities and HWs > App V1.R
# Data sets are located in cloud > project> AppV1 > air_quality


library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate)
library(bslib)
library(sf)
library(viridis)


# ==============================================================================
# LOAD DATA (cleaned)
# ==============================================================================

# For trends tab----------------------------------------------------------
air_quality <- read_rds("/cloud/project/AppV1/air_quality/air_small.rds") %>%
  filter(value >= 0) %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    county = case_when(
      county == "005" ~ "Bronx",
      county == "047" ~ "Brooklyn",
      county == "061" ~ "Manhattan",
      county == "081" ~ "Queens",
      county == "085" ~ "Staten Island",
      TRUE ~ county
    ),
    year = year(datetime),
    month_num = month(datetime),
    month_name = month(datetime, label = TRUE, abbr = TRUE),
    day_of_week = wday(datetime, label = TRUE, abbr = FALSE),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring",
      month_num %in% c(6, 7, 8) ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall"
    ),
    pricing_period = case_when(
      datetime < as.POSIXct("2025-01-06", tz = "UTC") ~ "Before Congestion Pricing",
      datetime >= as.POSIXct("2025-01-06", tz = "UTC") ~ "After Congestion Pricing"
    )
  )

# For Spatial tab--------------------------------------------------------
nyc_counties <- c("005", "047", "061", "081", "085")

blocks <- read_sf("/cloud/project/AppV1/air_quality/bg.geojson") %>%
  mutate(county = substr(geoid, 3, 5)) %>%
  filter(county %in% nyc_counties)

county_labels <- tibble(
  county = c("005", "047", "061", "081", "085"),
  county_name = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
)

# =============================================================================
# UI
# =============================================================================

ui <- page_sidebar(
  title = "Air Quality Dashboard: NYC PM2.5",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .nav-tabs .nav-link {
        color: #6c757d !important;
        background-color: #f1f3f5 !important;
        border: 1px solid #dee2e6 !important;
        margin-right: 4px;
      }

      .nav-tabs .nav-link:hover {
        background-color: #e9ecef !important;
        color: #000000 !important;
      }

      .nav-tabs .nav-link.active {
        background-color: #000000 !important;
        color: #ffffff !important;
        border-color: #000000 !important;
        font-weight: 600;
      }
    "))
  ),
  
  div(
    style = "font-size:14px; color:#6c757d; margin-bottom:15px;",
    "This dashboard shows PM2.5 air quality trends across the five NYC boroughs for 2024–2025. Use the filters to explore how pollution levels vary by borough and year."
  ),
  
  sidebar = sidebar(
    selectInput("county", "Select Borough:", choices = sort(unique(air_quality$county))),
    selectInput("year_filter", "Select Year:", choices = c("All Years", "2024", "2025")),
    selectInput(
      "season_filter",
      "Filter by Season:",
      choices = c("All Seasons (No Filter)", "Winter", "Spring", "Summer", "Fall")
    ),
    hr(),
    h4("Summary"),
    textOutput("selection_count"),
    width = 300
  ),
  
  navset_tab(
    nav_panel(
      "Trends",
      layout_column_wrap(
        width = 1/2,
        uiOutput("avg_pm25_box"),
        uiOutput("obs_count_box")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Monthly PM2.5 Trend"),
          plotOutput("monthly_plot", height = "400px")
        ),
        
        conditionalPanel(
          condition = "input.season_filter != 'All Seasons (No Filter)'",
          card(
            card_header("PM2.5 in Selected Season"),
            plotOutput("season_line_plot", height = "400px")
          )
        )
      ),
      
      br(),
      
      card(
        card_header("Before vs After Congestion Pricing"),
        plotOutput("pricing_plot", height = "450px"),
        br(),
        textOutput("pricing_summary")
      )
    ),
    
    nav_panel(
      "Spatial Analysis",
      
      layout_column_wrap(
        width = 1/2,
        uiOutput("highest_borough_box"),
        uiOutput("lowest_borough_box")
      ),
      
      card(
        card_header("County Map"),
        plotOutput("county_map", height = "500px"),
        br(),
        textOutput("county_map_text")
      ),
      
      br(),
      
      card(
        card_header("Spatial Clusters of PM2.5 Monitoring Points"),
        plotOutput("cluster_map", height = "500px"),
        br(),
        textOutput("cluster_map_text")
      )
    )
  )
)
# ==============================================================================
# SERVER
# ==============================================================================

# Reactive filtered data set
server <- function(input, output, session) {
  
  # Reactive filtered data set for Trends tab
  filtered_data <- reactive({
    
    df <- air_quality %>%
      filter(county == input$county)
    
    if (input$year_filter != "All Years") {
      df <- df %>% filter(year == as.numeric(input$year_filter))
    }
    
    if (input$season_filter != "All Seasons (No Filter)") {
      df <- df %>% filter(season == input$season_filter)
    }
    
    df
  })
  
  # Reactive spatial data for Spatial Analysis tab----------------------------
  spatial_data <- reactive({
    
    df <- air_quality
    
    if (input$year_filter != "All Years") {
      df <- df %>% filter(year == as.numeric(input$year_filter))
    }
    
    county_pm <- df %>%
      mutate(
        county = case_when(
          county == "Bronx" ~ "005",
          county == "Brooklyn" ~ "047",
          county == "Manhattan" ~ "061",
          county == "Queens" ~ "081",
          county == "Staten Island" ~ "085",
          TRUE ~ county
        )
      ) %>%
      
      group_by(county) %>%
      summarise(avg_pm25 = mean(value, na.rm = TRUE), .groups = "drop")
    
    blocks %>%
      left_join(county_pm, by = "county") %>%
      left_join(county_labels, by = "county")
  })
  
  label_points <- reactive({
    spatial_data() %>%
      group_by(county, county_name, avg_pm25) %>%
      summarise(geometry = st_union(geometry), .groups = "drop") %>%
      st_centroid()
  })
  
  cluster_data <- reactive({
    
      df <- air_quality
      
      if (input$year_filter != "All Years") {
        df <- df %>% filter(year == as.numeric(input$year_filter))
      }
      
      # Summarize PM2.5 by monitor/site first
      site_pm25 <- df %>%
        mutate(aqs_id_full = as.character(aqs_id_full)) %>%
        group_by(aqs_id_full) %>%
        summarise(
          avg_pm25 = mean(value, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Load monitoring site locations
      sites <- read_rds("/cloud/project/AppV1/air_quality/sites.rds") %>%
        mutate(aqs_id_full = as.character(aqs_id_full)) %>%
        st_as_sf() %>%
        st_transform(4326)
      
      # Join PM2.5 values to raw monitoring points
      point_data <- sites %>%
        left_join(site_pm25, by = "aqs_id_full") %>%
        filter(!is.na(avg_pm25))
      
      req(nrow(point_data) >= 3)
      
      # Extract x/y coordinates from the monitoring points
      coords <- st_coordinates(point_data)
      
      cluster_input <- point_data %>%
        st_drop_geometry() %>%
        mutate(
          x = coords[, 1],
          y = coords[, 2]
        ) %>%
        select(x, y, avg_pm25) %>%
        scale()
      
      # Spatial k-means using location + PM2.5
      set.seed(123)
      km <- kmeans(cluster_input, centers = 3)
      
      point_data$cluster <- factor(km$cluster)
      
      point_data
    })
  
  
  # Value box 1-----------------------------------------------------------------
  output$avg_pm25_box <- renderUI({
    
    df <- filtered_data()
    
    avg_value <- if (nrow(df) == 0) NA else round(mean(df$value, na.rm = TRUE), 2)
    
    value_box(
      title = "Average PM2.5",
      value = ifelse(is.na(avg_value), "No data", paste(avg_value, "µg/m³")),
      theme = value_box_theme(bg = "#4682B4", fg = "white")
    )
  })
  
  # Value box 2-----------------------------------------------------------------
  output$obs_count_box <- renderUI({
    
    df <- filtered_data()
    
    value_box(
      title = "Observations in Selection",
      value = nrow(df),
      theme = value_box_theme(bg = "#e9ecef", fg = "black")
    )
  })
  
  # Monthly data----------------------------------------------------------------
  monthly_data <- reactive({
    
    df <- filtered_data()
    
    df %>%
      mutate(month_name = as.character(month_name)) %>%  
      group_by(month_num, month_name) %>%
      summarise(avg_pm25 = mean(value, na.rm = TRUE), .groups = "drop") %>%
      right_join(
        data.frame(
          month_num = 1:12,
          month_name = month.abb
        ),
        by = c("month_num", "month_name")
      ) %>%
      arrange(month_num)
  })
  
  # Monthly plot----------------------------------------------------------------
  
  output$monthly_plot <- renderPlot({
    
    plot_data <- monthly_data()
    
    subtitle_text <- paste0(
      "Year selected: ", input$year_filter,
      " | Missing months indicate no available data"
    )
    
    ggplot(plot_data, aes(x = month_name, y = avg_pm25)) +
      geom_col(fill = "steelblue", color = "white", na.rm = TRUE) +
      scale_x_discrete(limits = month.abb) +
      labs(
        title = paste("Average PM2.5 by Month in", input$county),
        subtitle = subtitle_text,
        x = "Month",
        y = "PM2.5 (µg/m³)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  # Season line plot------------------------------------------------------------
  output$season_line_plot <- renderPlot({
    
    req(input$season_filter != "All Seasons (No Filter)")
    
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      ggplot() +
        annotate(
          "text", x = 1, y = 1,
          label = "No data available for this season and year selection.",
          size = 5
        ) +
        xlim(0, 2) +
        ylim(0, 2) +
        theme_void()
      
    } else {
      
      line_data <- df %>%
        mutate(month_name = as.character(month_name)) %>%
        group_by(month_num, month_name) %>%
        summarise(avg_pm25 = mean(value, na.rm = TRUE), .groups = "drop") %>%
        right_join(
          data.frame(
            month_num = 1:12,
            month_name = month.abb
          ),
          by = c("month_num", "month_name")
        ) %>%
        arrange(month_num)
      
      n_months_with_data <- sum(!is.na(line_data$avg_pm25))
      
      p <- ggplot(line_data, aes(x = month_num, y = avg_pm25)) +
        scale_x_continuous(
          breaks = 1:12,
          labels = month.abb
        ) +
        labs(
          title = paste(input$season_filter, "PM2.5 Trend in", input$county),
          subtitle = paste("Year selected:", input$year_filter),
          x = "Month",
          y = "PM2.5 (µg/m³)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      if (n_months_with_data >= 3) {
        p +
          geom_line(linewidth = 1, color = "#2F9E44", na.rm = TRUE) +
          geom_point(size = 2, color = "#2F9E44", na.rm = TRUE)
      } else {
        p +
          geom_point(size = 3, color = "#2F9E44", na.rm = TRUE)
      }
    }
  })
  
  # Before vs After Congestion Pricing plot-------------------------------------
  output$pricing_plot <- renderPlot({
    
    df <- air_quality %>%
      filter(county == input$county) %>%
      filter(year %in% c(2024, 2025))
    
    ggplot(df, aes(x = pricing_period, y = value, fill = pricing_period)) +
      geom_boxplot(alpha = 0.9) +
      
      scale_fill_manual(values = c(
        "Before Congestion Pricing" = "#dee2e6",
        "After Congestion Pricing" = "#2F9E44"
      )) +
      
      labs(
        title = paste("PM2.5 Before vs After Congestion Pricing in", input$county),
        subtitle = "Congestion pricing start date: January 6, 2025",
        x = "",
        y = "PM2.5 (µg/m³)"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$pricing_summary <- renderText({
    df <- air_quality %>%
      filter(county == input$county) %>%
      filter(year %in% c(2024, 2025))
    
    before <- mean(df$value[df$pricing_period == "Before Congestion Pricing"], na.rm = TRUE)
    after  <- mean(df$value[df$pricing_period == "After Congestion Pricing"], na.rm = TRUE)
    
    change <- round(before - after, 2)
    
    if (after < before) {
      paste(
        "PM2.5 levels decreased by", change, "µg/m³ after congestion pricing in",
        input$county,
        ", suggesting a measurable improvement in air quality following the policy change."
      )
    } else {
      paste(
        "PM2.5 levels increased by", abs(change), "µg/m³ after congestion pricing in",
        input$county,
        ", suggesting no improvement in air quality following the policy change."
      )
    }
  })
  
  # Summary text----------------------------------------------------------------
  output$selection_count <- renderText({
    
    df <- filtered_data()
    
    paste("Number of observations in selection:", nrow(df))
  })
  
  # 2 new metric boxes for the spatial trends info--------------------------------
  output$highest_borough_box <- renderUI({
    
    df <- spatial_data() %>%
      st_drop_geometry() %>%
      select(county_name, avg_pm25) %>%
      distinct() %>%
      filter(!is.na(avg_pm25)) %>%
      arrange(desc(avg_pm25))
    
    req(nrow(df) > 0)
    
    top_row <- df[1, ]
    
    value_box(
      title = "Highest PM2.5 Borough",
      value = paste0(top_row$county_name, " (", round(top_row$avg_pm25, 2), " µg/m³)"),
      theme = value_box_theme(bg = "#4682B4", fg = "white")
    )
  })
  
  output$lowest_borough_box <- renderUI({
    
    df <- spatial_data() %>%
      st_drop_geometry() %>%
      select(county_name, avg_pm25) %>%
      distinct() %>%
      filter(!is.na(avg_pm25)) %>%
      arrange(avg_pm25)
    
    req(nrow(df) > 0)
    
    low_row <- df[1, ]
    
    value_box(
      title = "Lowest PM2.5 Borough",
      value = paste0(low_row$county_name, " (", round(low_row$avg_pm25, 2), " µg/m³)"),
      theme = value_box_theme(bg = "#e9ecef", fg = "black")
    )
  })
  
  # County Map Render plot
  output$county_map <- renderPlot({
    
    ggplot() +
      geom_sf(data = spatial_data(), aes(fill = avg_pm25), color = NA) +
      geom_sf_text(
        data = label_points(),
        aes(label = county_name),
        size = 4,
        fontface = "bold"
      ) +
      scale_fill_viridis_c(
        option = "C",
        begin = 0.45,
        end = 0.95,
        na.value = "grey90",
        name = expression("PM2.5 ("*mu*"g/"*m^3*")")
      ) +
      labs(
        title = "Average PM2.5 Levels Across NYC Boroughs",
        subtitle = paste("Year selected:", input$year_filter),
        caption = "Source: Posit Cloud STS air quality dataset"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        legend.position = "right",
        axis.title = element_blank()
      )
  })
  
  # Cluster map render plot
  output$cluster_map <- renderPlot({
    
    point_clusters <- cluster_data()
    
    county_shapes <- blocks %>%
      group_by(county) %>%
      summarise(geometry = st_union(geometry), .groups = "drop")
    
    ggplot() +
      geom_sf(data = blocks, fill = "grey95", color = NA) +
      geom_sf(data = county_shapes, fill = NA, color = "white", linewidth = 1.2) +
      geom_sf(
        data = point_clusters,
        aes(color = cluster, size = avg_pm25),
        alpha = 0.8
      ) +
      scale_size_continuous(name = "Avg PM2.5") +
      labs(
        title = "Spatial Clusters of PM2.5 Monitoring Points",
        subtitle = paste("Year selected:", input$year_filter),
        color = "Spatial Cluster"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "right",
        axis.title = element_blank()
      )
  })
  
  # Reactive text boxes for Spatial trends----------------------------------------
  output$county_map_text <- renderText({
    
    df <- spatial_data() %>%
      st_drop_geometry() %>%
      select(county_name, avg_pm25) %>%
      distinct() %>%
      filter(!is.na(avg_pm25)) %>%
      arrange(desc(avg_pm25))
    
    req(nrow(df) >= 2)
    
    paste0(
      "Average PM2.5 varies across NYC boroughs. ",
      df$county_name[1], " has the highest average PM2.5 (",
      round(df$avg_pm25[1], 2), " µg/m³), while ",
      df$county_name[nrow(df)], " has the lowest (",
      round(df$avg_pm25[nrow(df)], 2), " µg/m³)."
    )
  })
  
  output$cluster_map_text <- renderText({
    df <- cluster_data() %>%
      st_drop_geometry() %>%
      group_by(cluster) %>%
      summarise(
        number_of_sites = n(),
        avg_cluster_pm25 = mean(avg_pm25, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_cluster_pm25))
    
    req(nrow(df) > 0)
    
    paste0(
      "This clustering analysis uses raw monitoring points instead of borough averages. ",
      "The clusters are based on each monitor's geographic location and average PM2.5 value. ",
      "The highest-pollution cluster has an average PM2.5 of ",
      round(df$avg_cluster_pm25[1], 2),
      " µg/m³ across ",
      df$number_of_sites[1],
      " monitoring point(s)."
    )
  })
  
}
# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)

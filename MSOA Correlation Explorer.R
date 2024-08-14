library(tidyverse)
library(fingertipsR)
library(PHEindicatormethods)
library(shiny)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)
library(thematic)
library(shinythemes)
library(bslib)



# Get data from fingertips etc: -----------------------------------------------

# Find the local health profile ID
profs <- profiles()
# it is 143

# Find the area type id
area_types()  
# msoa is AreaTypeID = 3

#Get list of indicators in the profile id 143
inds <- indicators(ProfileID = 143)


#retrieve data for selected indicator and area type
msoa_ft <- fingertips_data(inds$IndicatorID, AreaTypeID = 3) %>%
  janitor::clean_names() 



msoa_ft_ssot <- msoa_ft %>% 
  filter(parent_name %in% c("Staffordshire", "Stoke-on-Trent")) %>% 
  mutate (indicator_name = str_replace_all(indicator_name, "chronic obstructive pulmonary disease", "")) %>% 
  mutate (indicator_name = str_replace(indicator_name,"\\(COPD\\)", "COPD")) %>% 
  mutate (indicator_name = str_squish(indicator_name))

unique(msoa_ft_ssot$indicator_name)

#Get a list of the main indicators (rates etc) that we want based on ID:
rate_inds <- msoa_ft_ssot %>%   
  filter(str_detect(indicator_name, "standardised|admissions|rate|obesity|overweight")) %>% 
  distinct(indicator_id) %>% 
  mutate(indicator_id = paste0("x",indicator_id)) %>% 
  pull() 


#Identify the demographic indicators that we want:
inds_inequal <- c("Percentage of population whose ethnicity is not 'White UK'",
                  "Modelled estimates of the proportion of households in fuel poverty (%)",
                  "Index of Multiple Deprivation (IMD) Score",
                  "Child Poverty, Income deprivation affecting children index (IDACI)",
                  "Income deprivation, English Indices of Deprivation",
                  "Older people in poverty, income deprivation affecting older people Index (IDAOPI)")


#Pull out a list of the demographic indicators that we want based on ID:
demographic_inds <- msoa_ft_ssot %>%   
  filter(indicator_name %in% inds_inequal) %>% 
  distinct(indicator_id) %>% 
  mutate(indicator_id = paste0("x",indicator_id)) %>% 
  pull() 


#Create lookup of ID -> Name for the indicators:
lookup_df <- msoa_ft_ssot %>% 
  mutate(indicator_id = paste0("x",indicator_id)) %>% 
  filter(indicator_id %in% rate_inds | indicator_id %in% demographic_inds ) %>% 
  mutate(cat = case_when
         (indicator_id %in% rate_inds ~ "x",
           indicator_id %in% demographic_inds ~ "y",
           TRUE ~ "na"
         )) %>% 
  select(id = indicator_id, name = indicator_name, cat) %>% 
  distinct() %>% 
  mutate(group = case_when(
    str_detect(name, "births")  ~ 'Start Well',
    str_detect(name, "Reception|Year 6|under 5 years|under 15 years|15 to 24 years old|IDACI") ~ 'Grow Well',
    str_detect(name, "65 years and over|Older people in poverty|IDAOPI") ~ 'Age Well',
    TRUE ~ "Live well"
  ))


#Get a list of our LADs:
lads <-  msoa_ft_ssot %>% 
  distinct(parent_name) %>% 
  pull()



msoa_imd <- read.csv("https://research.mysociety.org/sites/imd2019/media/data/imd2019_msoa_level_data.csv") %>% 
  janitor::clean_names()


#Put our Fingertips data in a wide format - we also slice the data so we only get the latest version by year of each indicator:
msoa_data_wide <- msoa_ft_ssot %>% 
  mutate(indicator_id = paste0("x",indicator_id)) %>% 
  group_by(indicator_id, indicator_name, area_name, sex,age, category_type,category) %>%  #group by most unique criteria per indicator
  slice(which.max(timeperiod_sortable)) %>% # take the most recent rows for eaach inidicator only 
  ungroup() %>%
  filter(indicator_id %in% rate_inds | indicator_id %in% demographic_inds ) %>% 
  select(indicator_id,  area_code, area_name, parent_name, value) %>% 
  pivot_wider(names_from = indicator_id,
              values_from = value) %>% 
  left_join(msoa_imd, by = c("area_code" = "msoac")) 


# Load shapefile --------------------------------------

#reproject and filter:

msoa11_url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/MSOA_Dec_2011_Boundaries_Super_Generalised_Clipped_BSC_EW_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"


my_shp <- sf::read_sf(msoa11_url) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326) %>%
  filter(str_detect(MSOA11NM, "Staffordshire|Stoke|Lichfield|Cannock Chase|Newcastle-under-Lyme|Tamworth|Stafford")) %>% 
  rename(area_code = MSOA11CD) %>% 
  left_join(msoa_imd, by = c("area_code" = "msoac")) %>% 
  arrange(msoahocln) %>% 
  left_join(msoa_data_wide) 





# Shiny Dashboard: --------------------------------------------------------
thematic::thematic_shiny(font = "auto")


# Define UI
ui <- fluidPage(
  
  theme = bs_theme(
    bootswatch = "cerulean",
    base_font = font_google("Hind"),
    navbar_bg = "#25443B"
  ),
  
  # tags$head(
  #   tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Hind:wght@400;700&display=swap")
  # ),
  # 
  titlePanel("SSoT Inequalities Explorer"),
  tabsetPanel(
    id = "tabs",
    

    #MSOA correlation section with inputs, text, maps and charts:   
    tabPanel(
      title = "MSOA and Correlation Analysis",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 3,  # Adjust this number to make it narrower (default is 4)
            selectInput("group_filter", "Group Filter", 
                        choices = c("All", unique(lookup_df$group))),
            selectInput("x_indicator", "X-Axis Indicator", choices = NULL),
            selectInput("y_indicator", "Y-Axis Indicator", choices = lookup_df$name[lookup_df$cat == "y"]),
            selectInput("region_filter", "Filter by Region", choices = c("All", unique(my_shp$lad19n))),
            
            # Add text and code here
            tags$p("Select indicators for the X and Y axes from the dropdown menus above. You can also filter the data by region."),
            tags$p("Hover over points on the map to see more details, and the corresponding point on the scatter plot will be highlighted."),
            tags$p("Use the scatter plot to analyze the relationship between the selected indicators."),
            tags$br(), 
            tags$br(),  # Line break
            uiOutput("top_correlations"),
            tags$p(""),
            tags$br(),  # Line break
            tags$p("It should be noted that whilst the correlations highlighted here can provide valuable insights into potential relationships between variables, it is crucial to remember that correlation does not imply causation. Several factors can contribute to a strong correlation, and interpreting these results without considering these factors can be misleading."),
            tags$p("Additional sections use statistical tests and specialised techniques to interrogate the data more thoroughly. These methods aim to provide more robust insights and help to identify potential causal relationships."),
          ),
          
          mainPanel(
            fluidRow(
              column(6, 
                     tags$div(id = "map1_title"),
                     leafletOutput("map1", height = "500px")),
              column(6, 
                     tags$div(id = "map2_title"),
                     leafletOutput("map2", height = "500px"))
            ),
            plotlyOutput("scatterplot", height = "345px"),
            uiOutput("correlation_text")
            #uiOutput("top_correlations")  
          )
        )
      )
    ),
    
    

  
)



# Define server logic
server <- function(input, output, session) {
  
  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light
  # ))
  
  
  # Function to map user-friendly name to id of the MSOA shp dataset
  name_to_id <- function(name) {
    lookup_df$id[lookup_df$name == name]
  }
  
  # Function to map id to user-friendly name
  id_to_name <- function(id) {
    lookup_df$name[lookup_df$id == id]
  }
  
  
  # Filtered MSOA and shp dataframe based on selected inputs:
  filtered_data <- reactive({
    filtered <- my_shp
    if (input$region_filter != "All") {
      filtered <- filtered %>% filter(lad19n == input$region_filter)
    }
    filtered
  })
  
  
  
  
  # Update x and y indicator choices based on the selected group
  observe({
    filtered_lookup <- lookup_df %>%
      filter(group == input$group_filter | input$group_filter == "All")
    
    filtered_lookup_rii <- lookup_df %>%
      filter(group == input$group_filter | input$group_filter == "All")
    
    updateSelectInput(session, "x_indicator", choices = filtered_lookup$name[filtered_lookup$cat == "x"])
    #updateSelectInput(session, "y_indicator", choices = filtered_lookup$name[filtered_lookup$cat == "y"])
  })
  
  
  
  
  
  # Create the first leaflet map based on x_indicator input with some dynamic text
  
  output$map1 <- renderLeaflet({
    data <- filtered_data() # Get the filtered data
    
    x_col <- name_to_id(input$x_indicator) # Get the column name for x_indicator
    
    shp <- data %>% 
      arrange(area_name) %>% 
      mutate(pointNumber = row_number()) 
    
    # Ensure x_col is a column name for classIntervals
    x_col_sym <- sym(x_col)
    
    # Calculate class intervals based on natural breaks jenks method from classInt package
    classes <- classInt::classIntervals(data[[x_col]], n = 4, style = "jenks")
    leg_values <- unlist(classes$brks)
    
    # Mutate to add fillColor column
    to_map <- shp %>% 
      mutate(fillColor = case_when(
        !!x_col_sym >= leg_values[1] & !!x_col_sym < leg_values[2] ~ "#a6bddb",
        !!x_col_sym >= leg_values[2] & !!x_col_sym < leg_values[3] ~ "#74a9cf",
        !!x_col_sym >= leg_values[3] & !!x_col_sym < leg_values[4] ~ "#2b8cbe",
        !!x_col_sym >= leg_values[4] ~ "#045a8d",
        TRUE ~ "#DCDCDC"
      )) %>% 
      mutate(desc = paste0(area_name,"-", round(.data[[x_col]], 1)))
    
    leaflet(to_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(-2.2, 52.8, 9) %>%
      addPolygons(
        layerId = ~pointNumber,  # Set the layerId to the area_code
        fillColor = ~fillColor,
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = ~desc,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px")),
        highlightOptions = highlightOptions(
          weight = 4,
          color = "yellow",
          #fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        colors = c("#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d"),
        labels = c(
          paste0(round(leg_values[1], 0), " to ", round(leg_values[2], 1)),
          paste0(round(leg_values[2], 0), " to ", round(leg_values[3], 0)),
          paste0(round(leg_values[3], 0), " to ", round(leg_values[4], 0)),
          paste0(round(leg_values[4], 0), " to ", round(leg_values[5], 0))
        ),
        #title = input$x_indicator,
        position = "bottomleft"
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset Zoom",
        onClick = JS("function(btn, map){ map.setZoom(9); map.setCenter([-2.2, 52.8]); }")
      )) %>% 
      addFullscreenControl()
  })
  
  
  
  
  
  
  
  
  
  # Create the second map based on y_indicator input:
  output$map2 <- renderLeaflet({
    data <- filtered_data()
    y_col <- name_to_id(input$y_indicator)
    
    shp <- data %>% 
      arrange(area_name) %>% 
      mutate(pointNumber = row_number()) 
    
    classes <- classInt::classIntervals(data[[y_col]], n = 4, style = "jenks")   
    leg_values <- unlist(classes$brks)
    
    to_map <-  shp %>% 
      mutate(fillColor = case_when(
        .data[[y_col]] >= leg_values[1] & .data[[y_col]] < leg_values[2] ~ "#a6bddb",
        .data[[y_col]] >= leg_values[2] & .data[[y_col]] < leg_values[3] ~ "#74a9cf",
        .data[[y_col]] >= leg_values[3] & .data[[y_col]] < leg_values[4] ~ "#2b8cbe",
        .data[[y_col]] >= leg_values[4] ~ "#045a8d",
        TRUE ~ "#DCDCDC"
      ))
    
    leaflet(to_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(-2.2, 52.8, 9) %>%
      addPolygons(
        layerId = ~pointNumber,  # Set the layerId to the area_code
        fillColor = ~fillColor,
        fillOpacity = 0.8,
        color =  "white",
        weight = 1,
        label = ~desc,
        # label = ~paste0(area_name , " the rate is <b>",  round(.data[[y_col]], 1), "</b>  per 100,000") %>%
        #   lapply(htmltools::HTML),
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px")),
        highlightOptions = highlightOptions(
          weight = 4,
          color = "yellow",
          #fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>% 
      addLegend(
        colors = c("#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d"),
        labels = c(
          paste0(round(leg_values[1], 0), " to ", round(leg_values[2], 1)),
          paste0(round(leg_values[2], 0), " to ", round(leg_values[3], 0)),
          paste0(round(leg_values[3], 0), " to ", round(leg_values[4], 0)),
          paste0(round(leg_values[4], 0), " to ", round(leg_values[5], 0))
        ),
        #title = input$y_indicator,
        position = "bottomleft"
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = "Reset Zoom",
        onClick = JS("function(btn, map){ map.setZoom(9); map.setCenter([-2.2, 52.8]); }")
      )) %>% 
      addFullscreenControl()
  })
  
  
  
  
  #Use observe to sync the maps:
  observe({
    coords1 <- input$map1_bounds
    if (!is.null(coords1)) {
      leafletProxy("map2") %>%
        fitBounds(coords1$west,
                  coords1$south,
                  coords1$east,
                  coords1$north)
    }
  })
  
  observe({
    coords2 <- input$map2_bounds
    if (!is.null(coords2)) {
      leafletProxy("map1") %>%
        fitBounds(coords2$west,
                  coords2$south,
                  coords2$east,
                  coords2$north)
    }
  })
  
  
  
  # Add dynamic titles for the maps
  observe({
    map1_title <- input$x_indicator
    map2_title <- input$y_indicator
    
    # Update the title for map1
    shinyjs::html("map1_title", paste0("<h4>", map1_title, "</h4>"))
    
    # Update the title for map2
    shinyjs::html("map2_title", paste0("<h4>", map2_title, "</h4>"))
  })
  
  
  
  
  # Correlation Scatterplot: ------------------------------------------------
  # Create scatterplot correlations chart in plotly with some dynamic text
  # We will also use observeEvent in order to 'link' interactions between this chart and the leaflet maps (highlight on hover)
  output$scatterplot <- renderPlotly({
    
    data <- filtered_data()
    x_col <- name_to_id(input$x_indicator)
    y_col <- name_to_id(input$y_indicator)
    
    # Create the tooltip text
    tooltip_text <- sprintf("Area: %%s<br>%s: %%s<br>%s: %%s", input$x_indicator, input$y_indicator)
    
    
    
    
    p <- ggplot(filtered_data(), aes_string(x = x_col, y = y_col)) +
      geom_point(aes(x = !!sym(x_col), y = !!sym(y_col),
                     text = paste("area_code:", filtered_data()$area_code, "<br>area_name:", filtered_data()$area_name)),
                 size = 4, fill = "#00A9CE", color = "#00A9CE", alpha = 0.7) +
      geom_smooth(method=lm , color="red", fill="#69b3a2", se=FALSE) +
      labs(x = input$x_indicator, y = input$y_indicator) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),  # Transparent background
        plot.background = element_rect(fill = "transparent", color = NA),  # Transparent background
        panel.grid.major = element_line(color = "rgba(255, 255, 255, 0.25)"),  # Partially transparent white major gridlines
        panel.grid.minor = element_line(color = "rgba(255, 255, 255, 0.25)"),  # More transparent white minor gridlines
        axis.text = element_text(color = "#425563"),  # White axis text
        axis.title = element_text(color = "#425563"),  # White axis titles
        plot.title = element_text(color = "#425563")  # White plot title
      )
    
    
    p <- ggplotly(p, tooltip = c("text"), source = "A")
    p <- event_register(p, "plotly_hover")
    p <- event_register(p, "plotly_unhover")
    p
    
  })
  
  
  # Highlight the region on maps when hovering over a point on the scatterplot
  observeEvent(event_data("plotly_hover", source = "A"), {
    hover <- event_data("plotly_hover", source = "A")
    # print(hover)  # Debugging: Print hover event data
    if (is.null(hover)) return()
    
    # Extract the pointnumber from hover event data on the scatter plot:
    sct_pt_number <- hover$pointNumber
    #print(paste0("Selected Scatterplot point id is" , sct_pt_number))  # Debugging: Print the text data
    
    shp <- filtered_data() %>%  #take my filtered data and assign it a number in the same order that the scatterpoints are given pointNumber
      arrange(area_name) %>%
      mutate(shp_pt_number = row_number()-1)
    
    leafletProxy("map1") %>%
      clearGroup("highlight") %>%
      addPolylines(data = shp[shp$shp_pt_number == sct_pt_number, ], #add my poly lines based on the filtered shp data
                   color = "yellow", weight = 2, opacity = 1, fillOpacity = 0, group = "highlight")
    
    leafletProxy("map2") %>%
      clearGroup("highlight") %>%
      addPolylines(data = shp[shp$shp_pt_number == sct_pt_number, ],
                   color = "yellow", weight = 2, opacity = 1, fillOpacity = 0, group = "highlight")
  })
  
  observeEvent(event_data("plotly_unhover", source = "A"), {
    unhover <- event_data("plotly_unhover", source = "A")
    #print(unhover)  # Debugging: Print unhover event data
    leafletProxy("map1") %>% clearGroup("highlight")
    leafletProxy("map2") %>% clearGroup("highlight")
  })
  
  
  
  
  # # Highlight the point on scatterplot when hovering over a region on either map
  observeEvent(input$map1_shape_mouseover, {
    event <- input$map1_shape_mouseover
    #print(event)  # Debugging: Print map1_shape_mouseover event data
    if (is.null(event)) return()
    
    area_code <- as.numeric(event$id) 
    
    #Debugging:
    #print(paste0("Selected area_code is ", area_code))  # Debugging: Print the area_code
    
    # Get the x and y coordinates for the area_code
    data <- filtered_data() %>%
      arrange(area_name) %>%
      mutate(id = row_number())
    
    # Convert id to numeric to match area_code type
    data <- data %>%
      mutate(id = as.numeric(id))
    
    # Debugging: Print the data to verify
    # print(head(data))  # Debugging: Print the first few rows of the data
    # 
    # print(colnames(data)) 
    # 
    # Ensure that x_indicator and y_indicator are correctly passed
    # print(paste0("x_indicator is ",  name_to_id(input$x_indicator)))
    # print(paste0("y_indicator is ",  name_to_id(input$y_indicator)))
    # 
    # print(paste0("Selected area_code is ", area_code))  # Debugging: Print the area_code
    
    
    test_value <- area_code
    
    
    
    # Filter the selected point
    selected_point <- data %>% filter(id == test_value)
    
    # Print selected_point to verify
    #print(selected_point)  # Debugging: Print the selected point
    
    x_val <- selected_point[[ name_to_id(input$x_indicator)]]
    y_val <- selected_point[[ name_to_id(input$y_indicator)]]
    
    # Print x and y values to verify
    # print(paste0("x is ", x_val, " y is: ", y_val)) # debugging
    
    plotlyProxy("scatterplot", session) %>%
      plotlyProxyInvoke("addTraces", list(x = list(x_val), y = list(y_val), 
                                          type = 'scatter', mode = 'markers',
                                          marker = list(size = 20, color = "yellow")))
    
    
    leafletProxy("map2") %>%
      clearGroup("highlight") %>%
      addPolylines(data = data[data$id + 1 == test_value + 1, ],
                   color = "yellow", weight = 2, opacity = 1, fillOpacity = 0, group = "highlight")
    
    
  })
  
  # # Remove highlighting on mouseout
  observeEvent(input$map1_shape_mouseout, {
    plotlyProxy("scatterplot", session) %>%
      plotlyProxyInvoke("deleteTraces", list(2))  # Assuming the highlighted point is the first trace
    
    leafletProxy("map2") %>% clearGroup("highlight")  #clear highlight on the other map
    
  })
  
  
  
  
  # # Highlight the point on scatterplot when hovering over a region on either map
  observeEvent(input$map2_shape_mouseover, {
    event <- input$map2_shape_mouseover
    #print(event)  # Debugging: Print map1_shape_mouseover event data
    if (is.null(event)) return()
    
    area_code <- as.numeric(event$id) # Adjust if area_code is 0-based or 1-based
    
    # Get the x and y coordinates for the area_code
    data <- filtered_data() %>%
      arrange(area_name) %>%
      mutate(id = row_number())
    
    # Convert id to numeric to match area_code type
    data <- data %>%
      mutate(id = as.numeric(id))
    
    test_value <- area_code
    
    # Filter the selected point
    selected_point <- data %>% filter(id == test_value)
    
    x_val <- selected_point[[ name_to_id(input$x_indicator)]]
    y_val <- selected_point[[ name_to_id(input$y_indicator)]]
    
    plotlyProxy("scatterplot", session) %>%
      plotlyProxyInvoke("addTraces", list(x = list(x_val), y = list(y_val), 
                                          type = 'scatter', mode = 'markers',
                                          marker = list(size = 20, color = "yellow")))
    
    leafletProxy("map1") %>%
      clearGroup("highlight") %>%
      addPolylines(data = data[data$id + 1 == test_value + 1, ],
                   color = "yellow", weight = 2, opacity = 1, fillOpacity = 0, group = "highlight")
    
    
  })
  
  # # Remove highlighting on mouseout
  observeEvent(input$map2_shape_mouseout, {
    plotlyProxy("scatterplot", session) %>%
      plotlyProxyInvoke("deleteTraces", list(2))  # Assuming the highlighted point is the first trace
    
    leafletProxy("map1") %>% clearGroup("highlight")  #clear highlight on the other map
  })
  
  
  
  # Correlation chart text: -------------------------------------------------
  # Calculate narrative text for correlation interpretation using dynamic text based on inputs and valuees 
  output$correlation_text <- renderText({
    data <- filtered_data()
    x_col <- name_to_id(input$x_indicator)
    y_col <- name_to_id(input$y_indicator)
    correlation <- cor(data[[x_col]], data[[y_col]], use = "complete.obs")
    r_squared <- correlation^2
    
    # Determine correlation strength
    correlation_strength <- ""
    if (abs(correlation) > 0.8) {
      correlation_strength <- "a very strong"
    } else if (abs(correlation) > 0.6) {
      correlation_strength <- "a strong"
    } else if (abs(correlation) > 0.4) {
      correlation_strength <- "a moderate"
    } else if (abs(correlation) > 0.2) {
      correlation_strength <- "a weak"
    } else if (abs(correlation) > 0.1) {
      correlation_strength <- "an extremely weak"
    } else {
      correlation_strength <- "no"
    }
    
    # Determine direction
    direction <- ifelse(correlation > 0, "positive", "negative")
    
    # Meaningfulness based on R squared
    r_squared_meaning <- ""
    if (r_squared > 0.8) {
      r_squared_meaning <- "indicating that the data fits the linear trend line very well, suggesting a highly predictive relationship. This means most areas follow the linear trend line closely."
    } else if (r_squared > 0.6) {
      r_squared_meaning <- "indicating that the data fits the linear trend line well, suggesting a strong predictive relationship. This means many areas follow the linear trend line closely."
    } else if (r_squared > 0.4) {
      r_squared_meaning <- "indicating a moderate fit to the linear trend line, suggesting a moderate predictive relationship. This means some areas deviate from the linear trend line."
    } else if (r_squared > 0.2) {
      r_squared_meaning <- "indicating a weak fit to the linear trend line, suggesting a weak predictive relationship. This means several areas do not follow the linear trend line closely."
    } else {
      r_squared_meaning <- "indicating a very weak fit to the linear trend line, suggesting a very weak predictive relationship. This means many areas do not follow the linear trend line."
    }
    
    
    # Determine direction
    selected_region <- ifelse(input$region_filter == "All", "Staffordshire and Stoke-on-Trent ICB", input$region_filter)
    
    HTML(paste(
      "<br><br><p style='font-size: 24px; font-family: \"Hind\", sans-serif; color: #425563;'><strong>",selected_region,"</strong></p>",
      "<br><p style='font-size: 20px; font-family: \"Hind\", sans-serif; color: #425563;'><strong>Correlation coefficient:</strong> ", round(correlation, 3), "</p>",
      "<p style='font-size: 18px; font-family: \"Hind\", sans-serif; color: #425563;'>This suggests there is ", correlation_strength, " ", direction, " correlation whereby <strong>", input$x_indicator, "</strong> is associated with ", ifelse(direction == "positive", "an increase", "a decrease"), " in <strong>", input$y_indicator, "</strong>.</p>",
      "<p style='font-size: 20px; font-family: \"Hind\", sans-serif; color: #425563;'><strong>R squared:</strong> ", round(r_squared, 3), "</p>",
      "<p style='font-size: 18px; font-family: \"Hind\", sans-serif; color: #425563;'>The R squared value is a measure of how well the data fits the linear trend line. The R squared value in this case suggests ", r_squared_meaning, ".</p>"
    ))
  })
  
  
  
  # Top 5 Correlation Text: -------------------------------------------------
  # Calculate and display top 5 correlations with narrative using dynamic text based on inputs and valuees 
  output$top_correlations <- renderUI({
    data <- filtered_data()
    region_name <- ifelse(input$region_filter == "All", "Staffordshire and Stoke-on-Trent ICB", input$region_filter)
    
    # Get the list of x and y indicators
    x_indicators <- lookup_df$name[lookup_df$cat == "x"]
    y_indicators <- lookup_df$name[lookup_df$cat == "y"]
    
    # Calculate correlations for all pairs
    correlation_list <- list()
    for (x_ind in x_indicators) {
      for (y_ind in y_indicators) {
        x_col <- name_to_id(x_ind)
        y_col <- name_to_id(y_ind)
        correlation <- cor(data[[x_col]], data[[y_col]], use = "complete.obs")
        correlation_list <- append(correlation_list, list(list(x_ind = x_ind, y_ind = y_ind, correlation = correlation)))
      }
    }
    
    # Sort by absolute correlation value and select top 5
    top_correlations <- correlation_list[order(sapply(correlation_list, function(c) abs(c$correlation)), decreasing = TRUE)][1:5]
    
    # Generate UI for top correlations
    tagList(
      tags$h4(paste("In", region_name, "the top 5 correlations are for:")),
      tags$ul(
        lapply(top_correlations, function(cor_info) {
          tags$li(paste0(
            cor_info$x_ind, " and ", cor_info$y_ind, 
            ": ", round(cor_info$correlation, 2)
          ))
        })
      )
    )
  })
  
  
  
  
  
  
}




# Include shinyjs for dynamic title updates
ui <- tagList(
  shinyjs::useShinyjs(),
  ui
)




# Run the application
shinyApp(ui = ui, server = server)




























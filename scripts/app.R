library(dplyr)
library(tidyverse)
library(vroom)
library(shiny)
library(plotly)
library(DT)


### Current goals:

### - Adjust JS implementation so that colors appear whether or not Average is on the screen
### - Strike Rate should be a combination of all the shadow zones, not a strike rate based on all called pitches NEXT
### - Make it so rank is dynamic, always being in order.
### - Spruce up app to add clear descriptions of functionality and meaning of statistics, similar to Baseball Savant

ucsb_catch <- vroom('BigWest_21_25.csv')

print(ucsb_catch %>% count(Catcher), n=100)

ucsb_catch_red <- ucsb_catch %>% select(Date, Catcher, PitchNo, Pitcher, PitcherId, PitcherTeam, 
                                        Outs, Balls, Strikes,
                                        PitchCall, PlateLocHeight, PlateLocSide) %>% 
  filter(PitchCall %in% c("StrikeCalled", "BallCalled")) %>% group_by(Catcher)

ucsb_catch_red$PlateLocHeight <- as.numeric(ucsb_catch_red$PlateLocHeight)
ucsb_catch_red$PlateLocSide <- as.numeric(ucsb_catch_red$PlateLocSide)

avg_zone_max <- 3.5066667
avg_zone_min <- 1.7175
avg_zone_width <- 0.80791667


ucsb_catch_red <- ucsb_catch_red %>%
  filter(!is.na(PlateLocSide) & !is.na(PlateLocHeight))

hexdata_all <- ucsb_catch_red %>%
  mutate(HexBinX = round(PlateLocSide, 1),
         HexBinY = round(PlateLocHeight, 1)) %>%
  group_by(HexBinX, HexBinY) %>%
  summarise(
    TotalPitches = n(),
    Strikes = sum(PitchCall == "StrikeCalled")
  ) %>%
  mutate(StrikePercentage = Strikes / TotalPitches * 100)


ucsb_catch_red <- ucsb_catch_red %>%
  mutate(HexBinX = round(PlateLocSide, 1),
         HexBinY = round(PlateLocHeight, 1)) %>%
  left_join(hexdata_all %>% select(HexBinX, HexBinY, StrikePercentage),
            by = c("HexBinX", "HexBinY"))

zones <- list(
  Zone11 = list(x_range = c(-1.108, -0.558), y_range = c(3.167, 3.833)),
  Zone12 = list(x_range = c(-0.557, 0.558), y_range = c(3.167, 3.833)),
  Zone13 = list(x_range = c(0.559, 1.108), y_range = c(3.167, 3.833)),
  Zone14 = list(x_range = c(-1.108, -0.558), y_range = c(1.833, 3.166)),
  Zone16 = list(x_range = c(0.559, 1.108), y_range = c(1.833, 3.166)),
  Zone17 = list(x_range = c(-1.108, -0.558), y_range = c(1.45, 1.832)),
  Zone18 = list(x_range = c(-0.557, 0.558), y_range = c(1.167, 1.832)),
  Zone19 = list(x_range = c(0.559, 1.108), y_range = c(1.167, 1.832))
)

# Helper function to check if a pitch is in a rectangular zone
is_in_rect <- function(x, y, rect) {
  x_min <- rect$x_range[1]
  x_max <- rect$x_range[2]
  y_min <- rect$y_range[1]
  y_max <- rect$y_range[2]
  
  return(x >= x_min && x <= x_max && y >= y_min && y <= y_max)
}

# General function to assign a zone
assign_zone_rect <- function(x, y, rect_zones) {
  for (zone in names(rect_zones)) {
    if (is_in_rect(x, y, rect_zones[[zone]]) == TRUE ) {
      return(zone)
    }
  }
  return(NA) # Outside defined zones
}

ui <- fluidPage(
  titlePanel("Catcher Framing Analysis"),
  
  div(p("Shadow Zones are shown from the pitcher's perspective."), 
      style = "font-size:16px; font-style:italic; text-align:center; margin-bottom:20px;"),
  
  fluidRow(
    column(12, 
           h3("Filters:"),
           
           # Conditional UI: Show both filters on tab 1, only catcher filter on tab 2
           column(6, conditionalPanel(
             condition = "input.tabs == 'Hexagonal Plot' || input.tabs == 'Framing Metrics Table'",
             selectInput("catcher_team", "Select Team:", choices = NULL)
           )), 
           column(6, conditionalPanel(
             condition = "input.tabs == 'Hexagonal Plot' || input.tabs == 'Framing Metrics Table'",
             selectInput("catcher", "Select Catcher:", choices = NULL)
           ))
           ,
           column(8, conditionalPanel(
             condition = "input.tabs == 'Hexagonal Plot'",
             sliderInput("strikeRate", "Strike Percentage Range:", min = 0, max = 100, value = c(40, 60)))
           ), 
           column(8, conditionalPanel(
             condition = "input.tabs == 'Framing Metrics Table'",
             checkboxInput("show_avg", "Show Average Row", value = TRUE)
           ))
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",  # Add an ID for conditionalPanel reference
        tabPanel("Hexagonal Plot", plotlyOutput("hexPlot", width = '600px', height = '600px')),
        tabPanel("Framing Metrics Table", DTOutput("metricsTable"))))))

server <- function(input, output, session) {
  # Reactive data filtering
  reactiveData <- reactive({
    filtered <- ucsb_catch_red %>% 
      filter(StrikePercentage >= input$strikeRate[1] & 
               StrikePercentage <= input$strikeRate[2])
    if (!is.null(input$catcher) && input$catcher != "All") {
      filtered <- filtered %>% filter(Catcher == input$catcher)
    }
    if (!is.null(input$catcher_team) && input$catcher_team != "All") {
      filtered <- filtered %>% filter(PitcherTeam == input$catcher_team)
    }
    
    filtered
  })
  
  reactiveData2 <- reactive({
    filtered <- ucsb_catch_red
    if (!is.null(input$catcher) && input$catcher != "All") {
      filtered <- filtered %>% filter(Catcher == input$catcher)
    }
    if (!is.null(input$catcher_team) && input$catcher_team != "All") {
      filtered <- filtered %>% filter(PitcherTeam == input$catcher_team)
    }
    filtered
  })
  
  # Populate Catcher Dropdown
  observe({
    catcher_teams <- c("All", unique(na.omit(ucsb_catch_red$PitcherTeam)))
    updateSelectInput(session, "catcher_team", choices = catcher_teams)
    
    catchers <- c("All", unique(na.omit(ucsb_catch_red$Catcher)))
    updateSelectInput(session, "catcher", choices = catchers)
  })
  
  # Hexagonal Plot
  output$hexPlot <- renderPlotly({
    plot <- ggplot(reactiveData(), aes(x = HexBinX, y = HexBinY, fill = StrikePercentage)) +
      geom_hex(stat = "identity") +  
      scale_x_continuous(limits = c(-1.75, 1.75)) + 
      scale_y_continuous(limits = c(0.5, 4.5)) + 
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 50) +
      geom_rect(aes(xmin = -avg_zone_width, xmax = avg_zone_width, ymin = avg_zone_min, ymax = avg_zone_max), fill = NA, color = 'black') + 
      geom_point(aes(text = paste("Strike %:", round(StrikePercentage, 2))), alpha = 0) + 
      theme_minimal() +
      labs(title = "Hexagonal Plot of Strike Percentage",
           x = "Plate Location Side", y = "Plate Location Height")
    ggplotly(plot, tooltip = 'text')
  })
  
  # Framing Metrics Table
  
  
  frameable_wide <- reactive({
    req(reactiveData2()) %>%
      mutate(Zone = mapply(assign_zone_rect, PlateLocSide, PlateLocHeight, MoreArgs = list(rect_zones = zones))) %>%
      group_by(Catcher, PitcherTeam, Zone) %>%
      summarise(
        FramingRate = round(sum(PitchCall == "StrikeCalled") / n() * 100, 2),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = Zone,
        values_from = FramingRate,
        names_glue = "FramingRate_{Zone}" # Creates columns like FramingRate_Zone11
      )
  })
  
  
  output$metricsTable <- renderDT({
    df <- reactiveData2() %>%
      mutate(Zone = mapply(assign_zone_rect, PlateLocSide, PlateLocHeight, MoreArgs = list(rect_zones = zones))) %>%
      group_by(Catcher, PitcherTeam) %>%
      summarise(
        TotalFrameablePitches = n(),
        FramedStrikes = sum(PitchCall == "StrikeCalled"),
        FramingRate = round(FramedStrikes / TotalFrameablePitches * 100, 2),
        AverageStrikePercentage = round(mean(StrikePercentage, na.rm = TRUE), 2),
        FramingRuns = round(0.125 * ((FramingRate - AverageStrikePercentage) / 100) * TotalFrameablePitches), 
        FramingRuns_per_500 = round(FramingRuns / TotalFrameablePitches * 500, 1)
      ) %>%
      filter(TotalFrameablePitches >= 50) %>%
      arrange(desc(FramingRuns))
    
    df_avg <- ucsb_catch_red %>%
      mutate(Zone = mapply(assign_zone_rect, PlateLocSide, PlateLocHeight, MoreArgs = list(rect_zones = zones))) %>%
      group_by(Catcher, PitcherTeam) %>%
      summarise(
        TotalFrameablePitches = n(),
        FramedStrikes = sum(PitchCall == "StrikeCalled"),
        FramingRate = round(FramedStrikes / TotalFrameablePitches * 100, 2),
        AverageStrikePercentage = round(mean(StrikePercentage, na.rm = TRUE), 2),
        FramingRuns = round(0.125 * ((FramingRate - AverageStrikePercentage) / 100) * TotalFrameablePitches), 
        FramingRuns_per_500 = round(FramingRuns / TotalFrameablePitches * 500, 1)
      ) %>%
      filter(TotalFrameablePitches >= 10) %>%
      arrange(desc(FramingRuns))
    
    
    frameable_wide2 <-
      ucsb_catch_red %>%
        mutate(Zone = mapply(assign_zone_rect, PlateLocSide, PlateLocHeight, MoreArgs = list(rect_zones = zones))) %>%
        group_by(Catcher, PitcherTeam, Zone) %>%
        summarise(
          FramingRate = round(sum(PitchCall == "StrikeCalled") / n() * 100, 2),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = Zone,
          values_from = FramingRate,
          names_glue = "FramingRate_{Zone}" # Creates columns like FramingRate_Zone11
        )
    
    
    framing_data <- frameable_wide()
    
    # Merge both average rows
    avg_row <- frameable_wide2 %>% full_join(df_avg, by = c("Catcher", "PitcherTeam")) %>% 
      summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
      mutate(Catcher = "Average", PitcherTeam = "All Teams")
    
    # Combine the full dataset with the single averaged row
    final_data <- full_join(df, framing_data, by = c("Catcher", "PitcherTeam")) %>%
      filter(TotalFrameablePitches >= 10)  %>%
      distinct() %>% arrange(desc('FramingRuns'))
    
    final_data <- bind_rows(avg_row, final_data)
      
      if (!input$show_avg) {
        final_data <- final_data %>% filter(Catcher != "Average")
      }
    
    final_data <- final_data %>% mutate(across(c(FramingRate, FramingRate_Zone11, FramingRate_Zone12, FramingRate_Zone13, 
                                                 FramingRate_Zone14, FramingRate_Zone16, FramingRate_Zone17, FramingRate_Zone18, 
                                                 FramingRate_Zone19), ~ ifelse(is.na(.), "-", paste0(format(round(. , 1), nsmall = 1), "%")))) %>%
      mutate(across(c(TotalFrameablePitches, FramedStrikes, FramingRuns, FramingRuns_per_500), ~ round(.x, 2))) %>% select(Catcher, PitcherTeam, 
                                                                                                                           TotalFrameablePitches, FramingRuns, FramingRuns_per_500, FramingRate, FramingRate_Zone11,
                                                                                                                           FramingRate_Zone12, FramingRate_Zone13, FramingRate_Zone14, FramingRate_Zone16,
                                                                                                                           FramingRate_Zone17, FramingRate_Zone18, FramingRate_Zone19)
    
    colnames(final_data) <- c("Catcher", "Pitcher Team", "Frameable Pitches", "Framing Runs (FR)", "FR AVG (per 500 Pitches)", "Strike Rate", "FramingRate_Zone11", 'FramingRate_Zone12', 
                              'FramingRate_Zone13', 'FramingRate_Zone14', 'FramingRate_Zone16', 'FramingRate_Zone17', 'FramingRate_Zone18', 'FramingRate_Zone19')
    
    avg_values <- final_data[final_data$Catcher == "Average", c("Framing Runs (FR)","FR AVG (per 500 Pitches)", "Strike Rate", "FramingRate_Zone11", 'FramingRate_Zone12', 
                                                                'FramingRate_Zone13', 'FramingRate_Zone14', 'FramingRate_Zone16', 'FramingRate_Zone17', 
                                                                'FramingRate_Zone18', 'FramingRate_Zone19')]
    
    callback_js <- JS("
  function(settings, json) {
    var table = this.api();
    var avgRowIdx = null;
    
    // Find the index of the 'Average' row
    table.rows().every(function(rowIdx, tableLoop, rowLoop) {
      var data = this.data();
      if (data[1].trim() === 'Average') { // Check column 1 for 'Average'
        avgRowIdx = rowIdx;
      }
    });

    if (avgRowIdx === null) {
      console.log('Average row not found');
      return;
    }

    var avgRow = table.row(avgRowIdx).data(); // Get 'Average' row data
    console.log('Found Average row at index:', avgRowIdx, 'Data:', avgRow);

    table.rows().every(function(rowIdx, tableLoop, rowLoop) {
      if (rowIdx === avgRowIdx) return; // Skip the 'Average' row

      var data = this.data();
      var cols = [4,5,6,7,8,9,10,11,12,13]; // Adjust if necessary based on column shifts

      for (var i = 0; i < cols.length; i++) {
        var colIdx = cols[i];
        var avgValue = parseFloat(avgRow[colIdx]) || 0;
        var cellValue = parseFloat(data[colIdx]) || 0;

        if (!isNaN(cellValue) && !isNaN(avgValue)) {
          var diff = cellValue - avgValue;
          var color = 'white';

          if (diff > 5) { color = '#ff9999'; }   // Light red (better)
          else if (diff > 2) { color = '#ffcccc'; }  
          else if (diff > 0) { color = '#ffeeee'; }  
          else if (diff < -5) { color = '#9999ff'; } // Light blue (worse)
          else if (diff < -2) { color = '#ccccff'; }  
          else if (diff < 0) { color = '#eeeeff'; }  

          $('td', table.row(rowIdx).node()).eq(colIdx).css('background-color', color);
        }
      }
    });
  }
")
    
    
    
    
    datatable(
      final_data,
      escape = FALSE,  # Allows HTML rendering
      options = list(
        autoWidth = TRUE,
        columnDefs = list(
          list(
            targets = "_all",
            className = "dt-center"  # Centers headers and data
          )
        ),
        headerCallback = JS(
          "function(thead, data, start, end, display) {",
          "  $(thead).find('th').each(function(index) {",
          "    var columnName = $(this).text();",
          "    var imgSrc = '';",
          "    if (columnName.includes('FramingRate_Zone11')) { imgSrc = 'https://i.postimg.cc/TPJ2zMvF/zone-11.png'; }",
          "    if (columnName.includes('FramingRate_Zone12')) { imgSrc = 'https://i.postimg.cc/tTrqg3Tz/zone-12.png'; }",
          "    if (columnName.includes('FramingRate_Zone13')) { imgSrc = 'https://i.postimg.cc/QtNNs4Xt/zone-13.png'; }",
          "    if (columnName.includes('FramingRate_Zone14')) { imgSrc = 'https://i.postimg.cc/VvdY0J0P/zone-14.png'; }",
          "    if (columnName.includes('FramingRate_Zone16')) { imgSrc = 'https://i.postimg.cc/Rhyv7vw6/zone-16.png'; }",
          "    if (columnName.includes('FramingRate_Zone17')) { imgSrc = 'https://i.postimg.cc/gcTcFH1z/zone-17.png'; }",
          "    if (columnName.includes('FramingRate_Zone18')) { imgSrc = 'https://i.postimg.cc/qM6JkCd8/zone-18.png'; }",
          "    if (columnName.includes('FramingRate_Zone19')) { imgSrc = 'https://i.postimg.cc/BnWqtQqR/zone-19.png'; }",
          "    if (imgSrc) {",
          "      $(this).html('<img src=\"' + imgSrc + '\" height=\"60\" width=\"40\">');",
          "    }",
          "  });",
          "}"
        ), 
        rowCallback = callback_js
      )
    )
  })
}

## Run the App

shinyApp(ui = ui, server = server)

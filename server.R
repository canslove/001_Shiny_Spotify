library(shiny)

server <- function(input, output, session) {
  
  
  # observe ----------------------------------------
  observe({
    new_y <- unique(raw.df %>% filter(ARTIST == input$x) %>% select(TRACK_NAME))
    updateSelectizeInput(
      session, "y",
      choices = new_y)
  })
  
  # reactive ----------------------------------------
  selected_trends <- reactive({
    req(input$Id_date)
    validate(need(!is.na(input$Id_date[1]) & !is.na(input$Id_date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$Id_date[1] < input$Id_date[2], "Error: Start date should be earlier than end date."))
    
    #regionList = ifelse(input$reCkUse,input$dynamic,region.df[,"REGION"])
    if (input$sel_tg=="ARTIST") {
      raw.df %>%
        filter(ARTIST == input$x,
               DATE > (input$Id_date[1]) & DATE < (input$Id_date[2]),
               REGION %in% input$Id_RegionSel)
    }
    else {
      raw.df %>%
        filter(TRACK_NAME == input$y,
               DATE > (input$Id_date[1]) & DATE < (input$Id_date[2]),
               REGION %in% input$Id_RegionSel)
    }
  }) 
  
  # renderUI ----------------------------------------
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "slider" = sliderInput(inputId = "dynamic", "From January to December in 2017:",
                                  min = 1, max = 12, value = 1, step = 1,
                                  animate = animationOptions(interval = 500, loop = TRUE)),
           # "text" = textInput("dynamic", "Dynamic",
           #                    value = "starting value"),
           # "numeric" =  numericInput("dynamic", "Dynamic",
           #                           value = 12),
           "radioButtons" = radioButtons(inputId = "dynamic", "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"),
           # "checkbox" = checkboxInput(inputId = "dynamic", label = "Dynamic",
           #                            value = TRUE),
           "checkboxGroup" = checkboxGroupInput(inputId = "dynamic", "Dynamic",
                                                choices = list_RegionMap,
                                                selected = "USA" ),
           # "selectInput" = selectInput("dynamic", "Dynamic",
           #                             choices = c("Option 1" = "option1",
           #                                         "Option 2" = "option2"),
           #                             selected = "option2"
           # ),
           # "selectInput (multi)" = selectInput("dynamic", "Dynamic",
           #                                     choices = c("Option 1" = "option1",
           #                                                 "Option 2" = "option2"),
           #                                     selected = c("option1", "option2"),
           #                                     multiple = TRUE
           # ),
           "date" = dateInput(inputId = "dynamic", "Dynamic"),
           "daterange" = dateRangeInput(inputId = "dynamic", "Dynamic")
    )#switch
  })#output$ui <- renderUI
  
  output$input_type_text <- renderText({
    input$input_type
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
  output$sel_region <- renderPrint({
    str(input$Id_RegionSel)
  })
  
  # Rest reactive when input$input_type changes input$dynamic:
  # data.frame(Name = c("Animation"), Value0 = as.character(c(input$dynamic)), stringsAsFactors = FALSE)
  sliderValues <- reactive({
    switch(input$input_type,
           "slider" = raw.df,
           "radioButtons" = raw.df,
           "checkboxGroup" = raw.df,
           "date" = raw.df,
           "daterange" = raw.df
    )#switch
  })#reactive
  
  # output$values0 <- renderTable({
  #   sliderValues()
  # })
  
  # Data Table ----------------------------------------
  output$table1 <- DT::renderDataTable({
    #datatable(raw.df[input$slider0[1]:input$slider0[2],], rownames=FALSE, filter = 'top') %>% 
    datatable(raw.df, rownames=FALSE, filter = 'top') %>% 
    formatStyle(input$selected, background="skyblue", fontWeight='bold')
    # Highlight selected column using formatStyle
  })#output$table <- DT::renderDataTable
  
  # output$table2 <- DT::renderDataTable({
  #   datatable(raw.df[input$slider0[1]:input$slider0[2],], rownames=FALSE, filter = 'top') %>% 
  #   #datatable(raw.df[raw.df[,"DATE"]==input$Id_date[1],], rownames=FALSE, filter = 'top') %>% 
  #     #datatable(raw.df4dt()[input$slider0[1]:input$slider0[2],], rownames=FALSE) %>% 
  #     #caption = 'Table 1: This is a simple caption for the table.' %>%
  #     formatStyle(input$selected,  
  #                 background="skyblue", fontWeight='bold')
  #   # Highlight selected column using formatStyle
  # })#output$table <- DT::renderDataTable
  
  
  # multichart ----------------------------------------
  output$descplotRank <- renderText({
    paste0("The Spotify Ranking of ", input$sel_tg, " in selected Countries (", input$Id_date[1], "~", input$Id_date[2], ")")
  }) #output$desc <- renderText
  
  if(1) {
    output$plotRank <- renderPlot({
      facets <- paste0(input$facet_row, '~' ,input$facet_col)
      facets = ifelse(input$facet_row == ".",
                      paste0('~',input$facet_col),
                      paste0(input$facet_row, '~' ,input$facet_col))
      #print(facets)
      
      geom_User <- switch(input$Id_plotType,
                          "Scatter" = geom_point(aes()),
                          "Bar" = geom_bar(aes(),color='green', stat="identity"),
                          "Histogram" = geom_histogram(aes(),stat="identity", color='yellow', fill='blue'),
                          "Distribution" = geom_density(aes(),stat="identity", fill='#FF6666'))
      # "Scatter", "Bar", "Histogram", "Distribution"
      selected_trends() %>%
        #gather(key = type, value = delay, departure, arrival) %>%
        ggplot(aes(x = DATE, y=RANKING)) +
        #ggplot(aes(x = DATE, y=input$Id_varY)) +
        geom_User +
        #geom_point() +
        #stat_smooth(method = "lm", se = FALSE) + facet_grid(~REGION) + #input$facet_col) +
        geom_smooth(method = "lm", se = TRUE) + 
        facet_grid(~REGION) + #input$facet_col) +
        #facet_grid(facets) +
        ggtitle(paste0(input$sel_tg, " - Rank by DATE (", input$Id_date[1], "~", input$Id_date[2], ")")) + 
        #theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black"))
        #theme_tufte() 
        #theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black"))
        #theme(plot.background = element_rect(fill = "black"))
        #theme_tufte() + theme(plot.background = element_rect(fill = "black"))
        theme_wsj()+ scale_colour_wsj("colors6") +
       #theme_calc()+ scale_colour_calc()
      #theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black"))
        theme(axis.text.x=element_text(angle = -90, hjust = 0))
      
    }) 
  }
  else {
    output$plotRank <- renderPlotly({
      # facets <- paste0(input$facet_row, '~' ,input$facet_col)
      # facets = ifelse(input$facet_row == ".",
      #                 paste0('~',input$facet_col),
      #                 paste0(input$facet_row, '~' ,input$facet_col))
      # geom_User <- switch(input$Id_plotType,
      #                     "Scatter" = geom_point(aes(color='green')),
      #                     "Bar" = geom_bar(aes(color='green'),stat="identity"),
      #                     "Histogram" = geom_histogram(aes(color='green', fill='yellow'),stat="identity"),
      #                     "Distribution" = geom_density(aes(fill='lime'),stat="identity"))
      # 
      # p <- ggplot(selected_trends(), aes(x = DATE, y=RANKING)) +
      #   geom_bar(aes(color='green'),stat="identity") #, position = "dodge") +
      #   #stat_smooth(method = "lm", se = FALSE) + facet_grid(~REGION) #+ #input$facet_col) +
      #   #theme_tufte()
      # p$elementId <- NULL
      # ggplotly(p)
      plot_ly(mtcars, x = ~mpg, y = ~wt)
    })
  }
  
  
  #--- Ex)
  #g <- ggplot(data = a, aes(x=DATE, y=RANKING))
  #g + geom_point() + stat_smooth(method = "lm", se = FALSE) + facet_wrap(~REGION)
  #--- Ex)
  
  # # Create scatterplot object the plotOutput function is expecting
  # output$lineplot <- renderPlot({
  #   color = "#434343"
  #   par(mar = c(4, 4, 1, 1))
  #   plot(x = selected_trends()$DATE, y = selected_trends()$RANKING, type = "l",
  #        xlab = "Date", ylab = "Trend index \n(Ranking & Streams)", col = color, fg = color, col.lab = color, col.axis = color)
  #   # plot(x = selected_trends()$DATE, y = selected_trends()$STREAMS, type = "l",
  #   #      xlab = "Date", ylab = "Trend index \n(Ranking & Streams)", col = color, fg = color, col.lab = color, col.axis = color)
  #   # Display only if smoother is checked
  #   if(input$smoother){
  #     smooth_curve <- lowess(x = as.numeric(selected_trends()$DATE), y = selected_trends()$RANKING, f = input$f)
  #     lines(smooth_curve, col = "#E6553A", lwd = 3)
  #   }
  # }) # output$lineplot <- renderPlot
  
  # Pull in description of trend
  
  
  
  # set.seed(122)
  # histdata <- rnorm(500)
  # output$plot1 <- renderPlot({
  #   #data <- histdata[seq_len(input$slider0[1])]
  #   data <- histdata[seq_len(input$dynamic)]
  #   hist(data)
  # })
  
  # shold be updated that the df set should selected or filtere by inptt$dynamic
  raw.df.sel <- reactive({
    raw.df %>%
      #filter_(top_n(50))
      head(.,10)
  })#reactive
  
  ## shold be updated that the df set should selected or filtere by inptt$dynamic
  # output$plot100 <- renderPlot({
  #   #raw.df %>% filter(DATE == as.Date("2017-01-03", "%Y-%m-%d")) %>%
  #   raw.df %>% filter(DATE == input$Id_date[1]) %>%
  #     ggplot(aes(x=RANKING, y=STREAMS, group = REGION)) +
  #     geom_point(aes(color = "REGIO")) +
  #     ggtitle("Preliminary")
  # })
  
  output$descplot100 <- renderText({
    paste0("Regional Ranking Trend for Selected Artists (", input$Id_date[1], "~", input$Id_date[2], ")")
  }) #output$desc <- renderText
  
  output$plot100 <- renderPlot({
    raw.df %>%
      filter(ARTIST == input$x, DATE > (input$Id_date[1]) & DATE < (input$Id_date[2]),
             REGION %in% input$Id_RegionSel) %>%
      ggplot(aes(x=DATE, y=-1*RANKING, group = REGION)) +
      geom_point(aes(color = REGION)) +
      geom_line(aes(color = REGION)) +
      ggtitle("Regional Ranking Trend for Selected Artists") +
      xlab("DATE") + ylab("RANKING") +
      #theme_tufte() 
      #theme(plot.background = element_rect(fill = "black"))
      #theme_tufte() + theme(plot.background = element_rect(fill = "black"))
      #theme_wsj()+ scale_colour_wsj("colors6")
      #theme_calc()+ scale_colour_calc()
      theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black"))
  })
  
  # input$summaryplot100 <- renderPrint({
  #   summary(output$plot100)
  # })
  
  output$descplot1000 <- renderText({
    paste0("Regional Ranking Trend for Selected Track_Name (", input$Id_date[1], "~", input$Id_date[2], ")")
  }) #output$desc <- renderText
  
  output$plot1000 <- renderPlot({
    raw.df %>%
      filter(TRACK_NAME == input$y, 
             DATE > (input$Id_date[1]) & DATE < (input$Id_date[2]),
             REGION %in% input$Id_RegionSel) %>%
      ggplot(aes(x=DATE, y=-1*RANKING, group = REGION)) +
      geom_point(aes(color = REGION)) +
      geom_line(aes(color = REGION)) +
      ggtitle("Regional Ranking Trend for Selected Track_Name") +
      xlab("DATE") + ylab("RANKING") +
      theme_hc()+ scale_colour_hc() + theme(plot.background = element_rect(fill = "black"))
  })
  
  # Status ---------------------------------------- 
  output$p30 <- renderPlot({
    #A() %>%
    raw.df %>%
      filter(DATE >= (input$Id_date[1]) & DATE <= (input$Id_date[2]),
             REGION %in% input$Id_RegionSel) %>%
      group_by(TRACK_NAME, REGION, ARTIST) %>%
      summarise(sum_Stream = sum(STREAMS)) %>%
      group_by(REGION) %>%
      top_n(input$rankinteger, sum_Stream) %>%
      arrange(REGION, desc(sum_Stream)) %>%
      ggplot(aes(x=ARTIST, y=sum_Stream, label=sum_Stream )) +
      geom_bar(aes(fill= REGION), stat = "identity", position = "dodge") +
      ggtitle(paste0("Rankings in each Countries (",input$date[1],"~",input$date[2],")" )) +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      ylab("Total played number of Streams") +
      xlab("ARTIST or Track Name") +
      coord_flip() +
      theme_tufte()
  })
  
  
  # map ----------------------------------------
  points <- eventReactive(input$recalc, {
    #cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    cbind(region_map.df$Longitude, region_map.df$Latitude)
  }, ignoreNULL = FALSE)
  
  
  #addPolylines(~Long, ~Lat) %>%  ## long and bold line
  #addMarkers(lng=-74.0059, lat=40.7128, popup="New York City") #%>%
  
  # points <- switch(input$Id_plotType,
  #                     "Scatter" = geom_point(),
  #                     "Bar" = geom_bar(stat="identity"),
  #                     "Histogram" = geom_histogram(stat="identity"),
  #                     "Distribution" = geom_density(stat="identity"))
  
  raw.df.allcountry <- reactive({
    raw.df %>%
      filter(ARTIST== input$x, TRACK_NAME == input$y, 
             DATE > (input$Id_date[1]) & DATE < (input$Id_date[2])) %>%
      group_by_(REGION) %>%
      summarise_(REGION, Avg_ranking = mean(RANKING), Avg_stream = mean(STREAMS)) 
  })#reactive
  
  #### Left join require to adjust the map for showing the avg_ranking number ########################################
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points(), popup = region_map.df$COUNTRY) #%>%
    # addMarkers(lng=-74.0059, lat=40.7128, popup="New York City") 
    # addCircleMarkers() %>%
    # addPopups() %>%
    # addPolylines() %>%
    # addPolygons() %>%
    # addCircles(radius = ~10^mag/10, weight = 1, color = "#777777", 
    #            fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag))
    # addRectangles() %>%
    # addTopoJSON() %>%
    # addGeoJSON()
  })
  
  
  # word cloud ----------------------------------------
  
  # reactive ----------------------------------------
  terms <- reactive({
    track_4_wc %>%
      filter(REGION == input$wc_sel,
             DATE > (input$wc_date_range[1]) & DATE < (input$wc_date_range[2])) %>%
      select(input$wc_sel_tg)
  })
  
  sliderValues <- reactive({
    data.frame(
      Name = c("Region",
               "Year",
               "Object",
               "Min. Frequency",
               "Max. Words"),
      Value = as.character(c(input$wc_sel,
                             paste(input$wc_date_range, collapse = " ~ "),
                             input$wc_sel_tg,
                             input$wc_freq,
                             input$wc_max)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$wc_values <- renderTable({
    sliderValues()
  })

  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$plot200 <- renderPlot({
    v <- terms()[,input$wc_sel_tg]
    #par(mfrow = c(1, 3), bg="black")
    wordcloud_rep(v, min.freq = input$wc_freq, max.words=input$wc_max, 
                  scale = c(4,0.5), random.color = TRUE, colors = brewer.pal(8, "Dark2"))
  })

  # map/hist selectized input start --------------------
  # show statistics using infoBox
  output$maxBox <- renderInfoBox({
    max_value <- max(state_stat[,input$selected2])
    max_state <- 
      state_stat$state.name[state_stat[,input$selected2]==max_value]
    infoBox(max_state, max_value, icon = icon("hand-o-up"))
  })
  output$minBox <- renderInfoBox({
    min_value <- min(state_stat[,input$selected2])
    min_state <- 
      state_stat$state.name[state_stat[,input$selected2]==min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG.", input$selected2),
            mean(state_stat[,input$selected2]), 
            icon = icon("calculator"), fill = TRUE))
  
  # show map using googleVis
  output$map <- renderGvis({
    gvisGeoChart(state_stat, "state.name", input$selected2,
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width="auto", height="auto"))
    # using width="auto" and height="auto" to
    # automatically adjust the map size
  })
  # show histogram using googleVis
  output$hist <- renderGvis(
    gvisHistogram(state_stat[,input$selected2, drop=FALSE]))
  # map/hist selectized input end----------------------
  
  
  
  
  # Distribution Map =====================================================
  #(1)
  
  
  #(2) =======================================================
  # # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$worldmap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("worldmap", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("worldmap", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
    
    # # Fit the view to within these bounds (can also use setView)
    # proxy %>% fitBounds(0, 0, 11, 11)
    # # Create circles with layerIds of "A", "B", "C"...
    # proxy %>% addCircles(1:10, 1:10, layerId = LETTERS[1:10])
    # # Remove some of the circles
    # proxy %>% removeShape(c("B", "F"))
    # # Clear all circles (and other shapes)
    # proxy %>% clearShapes()
    
  })#observe
  
  
  
  # Distribution Map =====================================================
  
  output$worldmap <- renderLeaflet({
    leaflet(Andrew) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      #addPolylines(~Long, ~Lat) %>%
      addMarkers(data = points(), popup = region_map.df$COUNTRY)
      #addMarkers(lng=-74.0059, lat=40.7128, popup="New York City") #%>%
    #addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap map tiles
    
  })
  
  observeEvent(input$show, {
    proxy <- leafletProxy("mymap")
    if(input$show) {
      proxy %>% addPolygons(data=colStates, stroke = FALSE,
                            #fillColor = heat.colors(6, alpha = 1),
                            fillColor = heat.colors(6, alpha = NULL),
                            layerId = LETTERS[1:6])
    } else {
      #proxy %>% removeShape(layerId = LETTERS[1:6])
      proxy %>% removeShape(layerId = c('A', 'B', 'C'))
    }
  })
  # map-end ---------------------
  
  
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    messageData <- data.frame(
      from = c("Sales Dept", "New User","Support"),
      message = c("Sales are steady this month.", "How do I register?", "The new server is ready.")
    )
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  
} #server <- function
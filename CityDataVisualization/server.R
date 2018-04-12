#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observe({
    updateCheckboxGroupInput(
      session,
      'checkGroup',
      choiceNames = as.character(by_state_order$state_name),
      choiceValues = by_state_order$State_Code,
      selected = if (input$bar)
        by_state_order$State_Code
    )
  })
  
  checkbox_state_filter <- reactive({
    plot.df %>%
      filter(State_Code %in% input$checkGroup)
    
  })
  
  
  df3 <- reactive({
    state_group.all %>%
      filter(State_Code %in% input$checkGroup)
    
  })
  
  
  df1 <- reactive({
    a <-
      subset(
        cities_dataset,
        select = c(state_code , name_of_city, state_name, population_total)
      ) %>%
      filter(state_code %in% input$checkGroup)
    a$pathString <- paste("world",
                          a$state_name,
                          a$name_of_city,
                          sep = "/")
    population <- as.Node(a)
    return(population)
    
  })
  
  df2 <- reactive({
    cities_dataset %>%
      filter(state_code %in% input$checkGroup)
    
  })
  
  output$graph1 <- renderHighchart({
    
    a <- checkbox_state_filter()
    if(length(a$state_name)>0){
      hc <-
        hchart(a,
               "column",
               hcaes(x = state_name, y = Total, color = state_name))  %>%
        hc_add_theme(hc_theme_google()) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5,
          pointFormat = "<b>Total Population</b>: {point.y}"
        ) %>%
        hc_xAxis(title = list(text = "States"))
      
      # Print highchart -----------------------------------------------
      hc
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$graph2 <- renderHighchart({
    a <- checkbox_state_filter()
    hc <-
      hchart(a,
             "treemap",
             hcaes(
               x = state_name,
               value = Total,
               color = Total
             ))  %>% hc_add_theme(hc_theme_google())
    
    
    # Print highchart -----------------------------------------------
    hc
  })
  
  
  # Create the map
  output$map <- renderLeaflet({
    l <- leaflet() %>% addTiles()
    
    l %>%
      addMarkers(data = cities_dataset,
                 ~ Latitude,
                 ~ Longitude,
                 clusterOptions = markerClusterOptions())
    
    cities_df <- split(cities_dataset, cities_dataset$state_code)
    
    l2 <- l
    names(cities_df) %>%
      purrr::walk(function(df) {
        l2 <<- l2 %>%
          addMarkers(
            data = cities_df[[df]],
            lng =  ~ Latitude,
            lat =  ~ Longitude,
            popup =  ~ as.character(name_of_city),
            group = df,
            clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
            labelOptions = labelOptions(noHide = F,
                                        direction = 'auto')
          )
      })
    
    l2 %>%
      addLayersControl(overlayGroups = names(cities_df),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  
  output$graph4 <- renderHighchart({
    if (input$top_states == "all_view") {
      hc <-
        hcmap(
          "countries/in/custom/in-all-andaman-and-nicobar",
          data = cities_dataset.merge,
          value = input$countries,
          joinBy = c("hc-a2", "hc-a2"),
          name = input$countries,
          dataLabels = list(enabled = TRUE, format = "{point.name}"),
          borderColor = "#FAFAFA",
          borderWidth = 0.1,
          tooltip = list(valueDecimals = 2)
        ) %>% hc_colorAxis(minColor = "blue",
                           maxColor = "red",
                           stops = color_stops(n = 5))
      
      hc
      
    } else{
      if (input$countries == "Population") {
        top_c <- cities_dataset %>% arrange(desc(population_total))
        top_c.10 <- top_c[1:20, ]
        sel_attr <- top_c.10$population_total
      }
      else {
        if (input$countries == "Male_Population") {
          top_c <-
            cities_dataset %>% mutate(population_male_m = population_male / population_total) %>% arrange(desc(population_male_m))
          top_c.10 <- top_c[1:20, ]
          sel_attr <- top_c.10$population_male_m
          
        } else{
          if (input$countries == "Female_Population") {
            top_c <-
              cities_dataset %>% mutate(population_female_m = population_female / population_total) %>% arrange(desc(population_female_m))
            top_c.10 <- top_c[1:20, ]
            sel_attr <- top_c.10$population_female_m
            
          } else{
            if (input$countries == "Literates_Total") {
              top_c <-
                cities_dataset %>% mutate(literates_total_m = literates_total / population_total) %>% arrange(desc(literates_total_m))
              top_c.10 <- top_c[1:20, ]
              sel_attr <- top_c.10$literates_total_m
            } else{
              if (input$countries == "Literates_Male") {
                top_c <-
                  cities_dataset %>% mutate(literates_male_m = literates_male / population_total) %>% arrange(desc(literates_male_m))
                top_c.10 <- top_c[1:20, ]
                sel_attr <- top_c.10$literates_male_m
              } else{
                if (input$countries == "Literates_Female") {
                  top_c <-
                    cities_dataset %>% mutate(literates_female_m = literates_female / population_total) %>% arrange(desc(literates_female_m))
                  top_c.10 <- top_c[1:20, ]
                  sel_attr <- top_c.10$literates_female_m
                } else{
                  if (input$countries == "Sex_Ratio") {
                    top_c <- cities_dataset %>% mutate(desc(sex_ratio))
                    top_c.10 <- top_c[1:20, ]
                    sel_attr <- top_c.10$sex_ratio
                  } else{
                    if (input$countries == "Total_graduates") {
                      top_c <-
                        cities_dataset %>% mutate(total_graduates_m = total_graduates / population_total) %>% arrange(desc(total_graduates_m))
                      top_c.10 <- top_c[1:20, ]
                      sel_attr <- top_c.10$total_graduates_m
                    } else{
                      if (input$countries == "Graduates_Male") {
                        top_c <-
                          cities_dataset %>% mutate(male_graduates_m = male_graduates / population_total) %>% arrange(desc(male_graduates_m))
                        top_c.10 <- top_c[1:20, ]
                        sel_attr <- top_c.10$male_graduates_m
                      } else{
                        if (input$countries == "Graduates_Female") {
                          top_c <-
                            cities_dataset %>% mutate(female_graduates_m = female_graduates / population_total) %>% arrange(desc(female_graduates_m))
                          top_c.10 <- top_c[1:20, ]
                          sel_attr <- top_c.10$female_graduates_m
                        } else{
                          top_c <- cities_dataset  %>% mutate(desc(population_total))
                          top_c.10 <- top_c[1:20, ]
                          sel_attr <- top_c.10$population_total
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
      cities_10 <- data_frame(
        name = top_c.10$name_of_city,
        lat = top_c.10$Longitude,
        lon = top_c.10$Latitude,
        z = sel_attr,
        color = colorize(z)
      )
      hcmap(
        "countries/in/custom/in-all-andaman-and-nicobar",
        showInLegend = FALSE,
        borderColor = "black",
        borderWidth = 1
      ) %>%
        hc_add_series(
          data = cities_10,
          type = "mapbubble",
          name = "Cities",
          maxSize = '10%',
          dataLabels = list(enabled = TRUE, format = '{point.name}'),
          showInLegend = FALSE
        )
      
      
    }
    
    
    
    
  })
  
  output$map <- renderLeaflet({
    if (input$states == "All") {
      l <- leaflet()  %>% addTiles()
      
      l2 <- l
      names(spllitted_cities) %>%
        purrr::walk(function(df) {
          l2 <<- l2 %>%
            addMarkers(
              data = spllitted_cities[[df]],
              lng =  ~ Latitude,
              lat =  ~ Longitude,
              popup = paste(
                "<h4>",
                spllitted_cities[[df]]$name_of_city,
                "</h4>",
                "<b>Population:</b>",
                spllitted_cities[[df]]$population_total,
                "<br>",
                "<b>Population Male:</b>",
                spllitted_cities[[df]]$population_male,
                "<br>",
                "<b>Population Female:</b>",
                spllitted_cities[[df]]$population_female,
                "<br>",
                "<b>Total Literacy:</b>",
                spllitted_cities[[df]]$literates_total,
                "<br>",
                "<b>Male Literacy:</b>",
                spllitted_cities[[df]]$literates_male,
                "<br>",
                "<b>Female Literacy:</b>",
                spllitted_cities[[df]]$literates_female,
                "<br>",
                "<b>Sex Ratio:</b>",
                spllitted_cities[[df]]$sex_ratio
              ),
              group = df,
              clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
              labelOptions = labelOptions(noHide = F,
                                          direction = 'auto')
            )
        })
      
      l2
    } else{
      df <-
        filter(cities_dataset,
               cities_dataset$state_code == input$states)
      l <- leaflet()  %>% addTiles() %>%
        addMarkers(
          data = df,
          lng =  ~ Latitude,
          lat =  ~ Longitude,
          popup = paste(
            "<h4>",
            df$name_of_city,
            "</h4>",
            "<b>Population:</b>",
            df$population_total,
            "<br>",
            "<b>Population Male:</b>",
            df$population_male,
            "<br>",
            "<b>Population Female:</b>",
            df$population_female,
            "<br>",
            "<b>Total Literacy:</b>",
            df$literates_total,
            "<br>",
            "<b>Male Literacy:</b>",
            df$literates_male,
            "<br>",
            "<b>Female Literacy:</b>",
            df$literates_female,
            "<br>",
            "<b>Sex Ratio:</b>",
            df$sex_ratio
          )
        )
      
      l2 <- l
      
    }
  })
  
  output$total_cities <- renderValueBox({
    if (input$states == "All") {
      valueBox(
        length(cities_dataset$name_of_city),
        "Total Cities",
        color = "purple",
        width = 6
      )
    } else{
      df <- filter(state_group.all, state_group.all$State_Code == input$states)
      valueBox(df$Total,
               "Total Cities",
               color = "purple",
               width = 6)
    }
    
  })
  
  output$total_population <- renderValueBox({
    if (input$states == "All") {
      valueBox(
        sum(cities_dataset$population_total),
        "Total Population",
        color = "orange",
        width = 6
      )
    } else{
      df <- filter(state_group.all, state_group.all$State_Code == input$states)
      valueBox(df$Population,
               "Total Cities",
               color = "orange",
               width = 6)
    }
    
  })
  
  output$male_pop_perc <- renderValueBox({
    if (input$states == "All") {
      valueBox(round((
        sum(cities_dataset$population_male) / sum(cities_dataset$population_total)
      ) * 100, 2),
      "% Male Population",
      color = "green",
      width = 6)
    } else{
      df <- filter(state_group.all, state_group.all$State_Code == input$states)
      valueBox(
        round(df$Male_Population, 2),
        "% Male Population",
        color = "green",
        width = 6
      )
    }
    
  })
  
  output$female_pop_perc <- renderValueBox({
    if (input$states == "All") {
      valueBox(
        round(
          sum(cities_dataset$population_female) / sum(cities_dataset$population_total) *
            100,
          2
        ),
        "% Female Population",
        color = "blue",
        width = 6
      )
    } else{
      df <- filter(state_group.all, state_group.all$State_Code == input$states)
      valueBox(
        round(df$Female_Population, 2),
        "% Female Population",
        color = "blue",
        width = 6
      )
    }
    
  })
  
  output$literacy_rate <- renderValueBox({
    if (input$states == "All") {
      valueBox(round(
        sum(cities_dataset$literates_total) / sum(cities_dataset$population_total) *
          100,
        2
      ),
      "Literacy Rate",
      color = "red",
      width = 6)
    } else{
      df <- filter(state_group.all, state_group.all$State_Code == input$states)
      valueBox(
        round(df$Literates_Total, 2),
        "Literacy Rate",
        color = "red",
        width = 6
      )
    }
    
  })
  
  output$sex_ratio <- renderValueBox({
    if (input$states == "All") {
      valueBox(round(
        sum(cities_dataset$sex_ratio) / length(cities_dataset$sex_ratio),
        2
      ),
      "Sex Ratio",
      color = "yellow",
      width = 6)
    } else{
      df <- filter(state_group.all, state_group.all$State_Code == input$states)
      valueBox(round(df$Sex_Ratio, 2),
               "Sex Ratio",
               color = "yellow",
               width = 6)
    }
    
  })
  
  
  output$polar_plot <- renderHighchart({
    if (input$states == "All") {
      hc <- highchart() %>%
        hc_chart(polar = TRUE)  %>%
        hc_xAxis(
          categories = c(
            "Male Population %",
            "Female Population %",
            "Literacy %",
            "Graduates %"
          ),
          tickmarkPlacement = "on",
          lineWidth = 0
        ) %>%
        hc_yAxis(
          gridLineInterpolation = "polygon",
          lineWidth = 0,
          min = 0
        ) %>%
        hc_series(
          list(
            name =  "All States",
            data = c(
              round((
                sum(cities_dataset$population_male) / sum(cities_dataset$population_total)
              ) * 100, 2),
              round((
                sum(cities_dataset$population_female) / sum(cities_dataset$population_total)
              ) * 100, 2),
              round(
                sum(cities_dataset$literates_total) / sum(cities_dataset$population_total) *
                  100,
                2
              ),
              round(
                sum(cities_dataset$total_graduates) / sum(cities_dataset$population_total) *
                  100,
                2
              )
            ),
            pointPlacement = "on",
            colorByPoint = TRUE,
            type = "column",
            colors = c("#F00", "#0F0", "#00F", "#F0F")
          )
        )
      
      hc
    } else{
      df <- filter(state_group.all, state_group.all$State_Code == input$states)
      hc <- highchart() %>%
        hc_chart(polar = TRUE)  %>%
        hc_xAxis(
          categories = c(
            "Male Population %",
            "Female Population %",
            "Literacy %",
            "Graduates %"
          ),
          tickmarkPlacement = "on",
          lineWidth = 0
        ) %>%
        hc_yAxis(
          gridLineInterpolation = "polygon",
          lineWidth = 0,
          min = 0
        ) %>%
        hc_series(
          list(
            name = df$state_name,
            data = c(
              df$Male_Population,
              df$Female_Population,
              df$Literates_Total,
              df$Total_graduates
            ),
            pointPlacement = "on",
            colorByPoint = TRUE,
            type = "column",
            colors = c("#F00", "#0F0", "#00F", "#F0F")
          )
        )
      
      hc
    }
    
    
    
    
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    hist(data)
  })
  
  output$plot2 <- renderHighchart({
    a <- checkbox_state_filter()
    hc <- highchart()  %>%
      hc_chart(type = "column") %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_xAxis(title = list(text = "States"),
               categories = a$state_name) %>%
      hc_add_series(
        name = "Male Polulation",
        data = a$Male_Population.mean,
        color = "green"
      ) %>%
      hc_add_series(
        name = "Female Polulation",
        data = a$Female_Population.mean,
        color = "blue"
      )
    
    # Print highchart -----------------------------------------------
    hc
  })
  
  output$plot3 <- renderHighchart({
    a <- checkbox_state_filter()
    hc <- highchart()  %>%
      hc_chart(type = "column") %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_xAxis(title = list(text = "States"),
               categories = a$state_name) %>%
      hc_add_series(name = "Male Graduates",
                    data = a$Male_Grads.mean,
                    color = "orange") %>%
      hc_add_series(
        name = "Female Graduates",
        data = a$Female_Grads.mean,
        color = "red"
      )
    
    # Print highchart -----------------------------------------------
    hc
  })
  
  output$plot4 <- renderHighchart({
    a <- checkbox_state_filter()
    hc <- highchart()  %>%
      hc_chart(type = "column") %>%
      hc_xAxis(title = list(text = "States"),
               categories = a$state_name) %>%
      hc_add_series(name = "Sex Ratio",
                    data = a$Sex_Ratio_mean,
                    color = "green")
    
    # Print highchart -----------------------------------------------
    hc
  })
  
  output$plot5 <- renderPlotly({
    a <- checkbox_state_filter()
    
    color <- colorize(length(a$state_name))
    plot_ly(
      a,
      x = ~ Population,
      y = ~ Male_Population.mean,
      z = ~ Female_Population.mean,
      color = ~ state_name
    ) %>%
      add_markers() %>%
      layout(scene = list(
        xaxis = list(title = 'Total Population'),
        yaxis = list(title = 'Male Popualtion'),
        zaxis = list(title = 'Female Population')
      ))
  })
  
  output$plot6 <- renderPlotly({
    a <- checkbox_state_filter()
    
    color <- colorize(length(a$state_name))
    plot_ly(
      a,
      x = ~ Graduates,
      y = ~ Male_Grads.mean,
      z = ~ Female_Grads.mean,
      color = ~ state_name
    ) %>%
      add_markers() %>%
      layout(scene = list(
        xaxis = list(title = 'Graduates'),
        yaxis = list(title = 'Male Grads'),
        zaxis = list(title = 'Female Grads')
      ))
  })
  
  output$plot7 <- renderHighchart({
    a <- df2()
    if(length(a$state_name)>0){
    hchart(
      a,
      "scatter",
      hcaes(x = effective_literacy_rate_male, y = effective_literacy_rate_female, group = state_name)
    ) %>%
      hc_add_theme(hc_theme_monokai()) %>%
      hc_xAxis(title = list(text = "Effective Literacy Rate Male")) %>%
      hc_yAxis(title = list(text = "Effective Literacy Rate Female")) %>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5,
        pointFormat = "<h3>{point.label}</h3><br><b>Literacy Rate Male</b>: {point.x}<br><b>Literacy Rate Female</b>: {point.y}"
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$plot8 <- renderHighchart({
    a <- df2()
    if(length(a$state_name)>0){
    hchart(a,
           "scatter",
           hcaes(x = male_graduates, y = female_graduates, group = state_name)) %>%
      hc_add_theme(hc_theme_monokai()) %>%
      hc_xAxis(title = list(text = "Male Graduates")) %>%
      hc_yAxis(title = list(text = "Female Graduates")) %>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5,
        pointFormat = "<h3>{point.label}</h3><br><b>Male Graduates</b>: {point.x}<br><b>Female Graduates</b>: {point.y}"
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$plot9 <- renderHighchart({
    a <- df2()
    if(length(a$state_name) >0){
    hchart(a,
           "scatter",
           hcaes(x = sex_ratio, y = child_sex_ratio, group = state_name)) %>%
      hc_add_theme(hc_theme_monokai()) %>%
      hc_xAxis(title = list(text = "Sex Ratio")) %>%
      hc_yAxis(title = list(text = "Child Sex Ratio")) %>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5,
        pointFormat = "<h3>{point.label}</h3><br><b>Sex ratio</b>: {point.x}<br><b>Child Sex Ratio</b>: {point.y}"
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$plot10 <- renderHighchart({
    a <- df2()
    if(length(a$state_name)>0){
    hchart(a,
           "scatter",
           hcaes(x = population_male, y = population_female, group = state_name)) %>%
      hc_add_theme(hc_theme_monokai()) %>%
      hc_xAxis(title = list(text = "Male Population")) %>%
      hc_yAxis(title = list(text = "Female Population")) %>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5,
        pointFormat = "<h3>{point.label}</h3><br><b>Male Population</b>: {point.x}<br><b>Female Population</b>: {point.y}"
      )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
    
  })
  
  output$plot11 <- renderHighchart({
    a <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(a$state_name, a$Population) %>%
      hc_tooltip(pointFormat = "<b>Total Population</b>: {point.y}")
    
  })
  
  output$plot12 <- renderHighchart({
    a <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(a$state_name, a$Graduates) %>%
      hc_tooltip(pointFormat = "<b>Total Graduates</b>: {point.y}")
    
  })
  
  output$plot13 <- renderHighchart({
    a <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(a$state_name, a$Literates) %>%
      hc_tooltip(pointFormat = "<b>Total Literates</b>: {point.y}")
    
  })
  
  output$plot14 <- renderHighchart({
    a <- checkbox_state_filter()
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 70,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(depth = 70)) %>%
      hc_add_series_labels_values(a$state_name, a$Sex_Ratio_mean) %>%
      hc_tooltip(pointFormat = "<b>Sex Ratio</b>: {point.y}")
    
  })
  
  output$plot15 <- renderHighchart({
    a <- df3()
    if(length(a$state_name)>0){
    hchart(a,
           type = "treemap",
           hcaes(
             x = state_name,
             value = Population,
             color = Population
           ))  %>% hc_add_theme(hc_theme_538()) %>%
      hc_colorAxis(minColor = "#FF0000", maxColor = "#008000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
      }
  })
  
  output$plot16 <- renderHighchart({
    a <- df3()
    if(length(a$state_name)>0){
    hchart(a,
           type = "treemap",
           hcaes(
             x = state_name,
             value = Literates_Total,
             color = Literates_Total
           ))  %>% hc_add_theme(hc_theme_538()) %>%
      hc_colorAxis(minColor = "#FFFF00", maxColor = "#FF0000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$plot17 <- renderHighchart({
    a <- df3()
    if(length(a$state_name)>0){
    hchart(a,
           type = "treemap",
           hcaes(
             x = state_name,
             value = Total_graduates,
             color = Total_graduates
           ))  %>% hc_add_theme(hc_theme_538()) %>%
      hc_colorAxis(minColor = "#0000FF", maxColor = "#008000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$plot18 <- renderHighchart({
    a <- df3()
    if(length(a$state_name)>0){
    hchart(a,
           type = "treemap",
           hcaes(
             x = state_name,
             value = Sex_Ratio,
             color = Sex_Ratio
           ))  %>% hc_add_theme(hc_theme_538()) %>%
      hc_colorAxis(minColor = "#FFFF00", maxColor = "#008000")
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$plot19 <- renderHighchart({
    a <- checkbox_state_filter()
    a <- a[order(a$state_name), ]
    highchart() %>%
      hc_xAxis(categories = a$state_name) %>%
      hc_add_series(name = "Total Population",
                    data = a$Population,
                    color = "green") %>%
      hc_add_series(name = "Male Population",
                    data = a$Male_Population,
                    color = "red") %>%
      hc_add_series(
        name = "Female Population",
        data = a$Female_Population,
        color = "blue"
      )
  })
  
  output$plot20 <- renderHighchart({
    a <- checkbox_state_filter()
    a <- a[order(a$state_name), ]
    highchart()  %>%
      hc_xAxis(categories = a$state_name) %>%
      hc_add_series(name = "Total Literates",
                    data = a$Literates,
                    color = "green") %>%
      hc_add_series(name = "Male Literates",
                    data = a$Male_Literates,
                    color = "red") %>%
      hc_add_series(
        name = "Female Literates",
        data = a$Female_Literates,
        color = "blue"
      )
  })
  
  output$plot21 <- renderHighchart({
    a <- checkbox_state_filter()
    a <- a[order(a$state_name), ]
    highchart()  %>%
      hc_xAxis(categories = a$state_name) %>%
      hc_add_series(name = "Total Graduates",
                    data = a$Graduates,
                    color = "green") %>%
      hc_add_series(name = "Male Grads",
                    data = a$Male_Grads,
                    color = "red") %>%
      hc_add_series(name = "Femlae Grads",
                    data = a$Female_Grads,
                    color = "blue")
  })
  
  output$plot22 <- renderHighchart({
    a <- checkbox_state_filter()
    a <- a[order(a$state_name), ]
    highchart()  %>%
      hc_xAxis(categories = a$state_name) %>%
      hc_add_series(name = "Sex Ratio",
                    data = a$Sex_Ratio_mean,
                    color = "green") %>%
      hc_add_series(name = "Child Sex Ratio",
                    data = a$Child_Sex_Ratio,
                    color = "red")
  })
  
  output$plot23 <- renderHighchart({
    a <- df2()
    if(length(a$state_name)>0){
    hcboxplot(
      x = a$population_total,
      var = a$state_name,
      name = "Population",
      color = "#2980b9",
      outliers = FALSE
    )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$plot24 <- renderHighchart({
    a <- df2()
    if(length(a$state_name)>0){
    hcboxplot(
      x = a$literates_total,
      var = a$state_name,
      name = "Total Literates",
      color = "red",
      outliers = FALSE
    )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$plot25 <- renderHighchart({
    a <- df2()
    if(length(a$state_name)>0){
    hcboxplot(
      x = a$total_graduates,
      var = a$state_name,
      name = "Total Graduates",
      color = "green",
      outliers = FALSE
    )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  output$plot26 <- renderHighchart({
    a <- df2()
    if(length(a$state_name)>0){
    hcboxplot(
      x = a$sex_ratio,
      var = a$state_name,
      name = "Sex Ratio",
      color = "orange",
      outliers = FALSE
    )
    }else{
      hc <- highchart()  %>%
        hc_chart(type = "column") %>%
        hc_plotOptions(series = list(stacking = "normal")) %>%
        hc_xAxis(title = list(text = "States"),
                 categories = a$state_name) %>%
        hc_add_series(
          name = "Male Polulation",
          data = a$Male_Population.mean,
          color = "green"
        ) %>%
        hc_add_series(
          name = "Female Polulation",
          data = a$Female_Population.mean,
          color = "blue"
        )
      
      # Print highchart -----------------------------------------------
      hc
    }
  })
  
  
  output$cities_table <- DT::renderDataTable({
    action <- DT::dataTableAjax(session, cities_dataset)
    
    DT::datatable(
      cities_dataset,
      options = list(
        searching = T,
        pageLength = 15,
        scrollX = T
      ),
      escape = FALSE
    )
  })
  
})

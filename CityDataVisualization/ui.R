

states.names <- unique(as.character(cities_dataset$state_name))

cities_dataset$state_code <- as.factor(cities_dataset$state_code)

states.codes <- unique(cities_dataset$state_code)

countries.names <-
  c(
    "Total Population",
    "Male Population",
    "Female Population",
    "Total Literates",
    "Male Literates",
    "Female Literates",
    "Sex Ratio",
    "Total Graduates",
    "Male Graduates",
    "Female Graduates"
  )

countries.values <-
  c(
    "Population",
    "Male_Population",
    "Female_Population",
    "Literates_Total",
    "Literates_Male",
    "Literates_Female",
    "Sex_Ratio",
    "Total_graduates",
    "Graduates_Male",
    "Graduates_Female"
  )

flags <- c(
  "icon1.svg",
  "icon2.svg",
  "icon3.png",
  "icon4.png",
  "icon5.jpeg",
  "icon6.jpeg",
  "icon7.jpeg",
  "icon8.png",
  "icon9.jpeg",
  "icon10.jpeg"
)


top.country.names <- c("All States View",
                       "Top States")

top.country.values <- c("all_view", "top_state")

top.icons <- c("icon12.png",
               "icon11.jpg")


navbarPage(
  "Cities Of India",
  id = "nav",
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Interactive Spatial map",
    div(
      class = "outer",
      
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      leafletOutput("map", width = "100%", height = "100%"),
      
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 80,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 490,
        height = 870,
        
        h2("Choose State"),
        
        selectInput("states", NULL, c(
          "ALL STATES" = "All",
          structure(
            by_state_order$State_Code,
            names = as.character(by_state_order$state_name)
          )
        )),
        valueBoxOutput("total_cities", width = 6),
        valueBoxOutput("total_population", width = 6),
        valueBoxOutput("male_pop_perc", width = 6),
        valueBoxOutput("female_pop_perc", width = 6),
        valueBoxOutput("literacy_rate", width = 6),
        valueBoxOutput("sex_ratio", width = 6),
        
        highchartOutput("polar_plot", height = 400)
      )
    )
  ),
  
  tabPanel(
    "India Mapper",
    dashboardPage(
      dashboardHeader(disable = T),
      dashboardSidebar(disable = T),
      dashboardBody(
        tags$script(
          'window.onload = function() {
          function fixBodyHeight() {
          var el = $(document.getElementsByClassName("content-wrapper")[0]);
          var h = el.height();
          el.css("min-height", h + 50 + "px");
          };
          window.addEventListener("resize", fixBodyHeight);
          fixBodyHeight();
          };'
),
fluidRow(
  box(
    radioButtons(
      "top_states",
      "",
      choiceNames = mapply(
        top.country.names,
        top.icons,
        FUN = function(country, flagUrl) {
          tagList(tags$img(
            src = flagUrl,
            width = 35,
            height = 35
          ),
          country)
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      ),
      choiceValues = top.country.values
    ),
    width = 2
  ),
  column(width = 1),
  column(highchartOutput("graph4", height = "800px"),
         width = 6),
  box(
    radioButtons(
      "countries",
      "",
      choiceNames = mapply(
        countries.names,
        flags,
        FUN = function(country, flagUrl) {
          tagList(tags$img(
            src = flagUrl,
            width = 70,
            height = 65
          ),
          country)
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      ),
      choiceValues = countries.values
    ),
    width = 3
  )
)
        )
    )
),

tabPanel(
  "Data explorer",
  box(
    title = "Cities Dataset",
    width = 12,
    status = "primary",
    height = "850",
    solidHeader = T,
    DT::dataTableOutput("cities_table", height = 800)
  )
),

tabPanel(
  "Plots",
  
  tags$head(tags$style(HTML(
    "
    div#checkGroup {
    font-size: 75%;
    }
    "
  ))),
  sidebarLayout(
    sidebarPanel(
      h4("Select States"),
      checkboxInput('bar', 'All/None', value = F),
      checkboxGroupInput(
        "checkGroup",
        label = NULL,
        choiceNames = as.character(by_state_order$state_name),
        choiceValues = by_state_order$State_Code,
        selected = c(35, 28, 22, 34, 3, 8, 19, 21, 20)
      )
      ,
      width = 2
    ),
    
    mainPanel(box(
      tabsetPanel(
        position = "below",
        tabPanel(
          "Bar Plots",
          fluidRow(column(
            6,
            box(
              title = "Frequency Plot of Number of cities in the states",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("graph1", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Mean Population of Males and Females in the state",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot2", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Mean No. of Graduates of Males and Females in the state",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot3", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Mean Sex Ratio Plot",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot4", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Scatter Plots",
          fluidRow(column(
            6,
            box(
              title = "Male vs Female Literacy Rate",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot7", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Male vs Female Graduate",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot8", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Sex vs Child Ratio",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot9", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Male vs Female Population",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot10", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Pie Charts",
          fluidRow(column(
            6,
            box(
              title = "Population Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot11", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Total Graduates Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot12", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Total Literates Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot13", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Sex Ratio Pie Chart",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot14", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Heat Map",
          fluidRow(column(
            6,
            box(
              title = "Population HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot15", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Total Literates HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot16", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Total Grads HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot17", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Sex Ratio HeatMap",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot18", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel(
          "Line Graphs",
          fluidRow(column(
            6,
            box(
              title = "Total vs Male vs Female Population",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot19", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Total vs Male vs Female Literates",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot20", height = "320px"),
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Total vs Male vs Female Graduates",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot21", height = 320)
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Sex Ratio vs Child Ratio",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot22", height = 320),
              width = 12
            )
          ))
        ),
        tabPanel("3D Plot",
                 fluidRow(column(
                   12,
                   box(
                     title = "3D Scatter Plot for Total vs Male vs Female Population",
                     status = "primary",
                     solidHeader = TRUE,
                     plotlyOutput("plot5", height = "320px")
                     ,
                     width = 12
                   )
                 )),
                 fluidRow(column(
                   12,
                   box(
                     title = "3D Scatter Plot for Total vs Male vs Female Graduates",
                     status = "primary",
                     solidHeader = TRUE,
                     plotlyOutput("plot6", height = "320px")
                     ,
                     width = 12
                   )
                 ))),
        tabPanel(
          "Box Plots",
          fluidRow(column(
            6,
            box(
              title = "Box Plot for States with respect to Population",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot23", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Box Plot for States with respect to Total Literates",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot24", height = "320px")
              ,
              width = 12
            )
          )),
          fluidRow(column(
            6,
            box(
              title = "Box Plot for states with respect to Total Grads",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot25", height = "320px")
              ,
              width = 12
            )
          ),
          column(
            6,
            box(
              title = "Box Plot for states with respect to Sex Ratio",
              status = "primary",
              solidHeader = TRUE,
              highchartOutput("plot26", height = "320px")
              ,
              width = 12
            )
          ))
        )
      ),
      width = 12
    ), width = 10)
  )
  ),


conditionalPanel("false", icon("crosshair"))
  )
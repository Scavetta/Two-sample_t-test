# Two-sample t-test, v2016, Dashboard - UI

dashboardPage(
  dashboardHeader(title = "t-test"),
  dashboardSidebar(
    # sidebarMenu(
    # Menu items for body    
    sidebarMenu(
      menuItem("Two-sample, Two-sided", tabName = "dashboard"),
      # menuItem("Hypothesis testing", tabName = "testing"),
      menuItem("Raw data", tabName = "rawdata"))
    
  ), # End dashboardSidebar
  
    
  dashboardBody(
    tabItems(
      
      # tab for the dashboard
      tabItem("dashboard",
              fluidRow(
                box(height = 500,
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Plot",
                  plotOutput(outputId = "main_plot", width = "100%")
                ), # End box
                box(height = 500,
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Group 1",
                  selectInput(inputId = "group1dist",
                              label = "Distribution",
                              choices = c("Normal" = "rnorm",
                                          "Uniform" = "runif"),
                              selected = "rnorm"),
                  sliderInput("samsize", "Sample size:",
                              min = 3, max = 60, value = 10, step= 1),
                  uiOutput("mainSliders"),
                  uiOutput("mainSliders2")                ), # End box
                box(height = 500,
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Group 2",
                  selectInput(inputId = "group2dist",
                              label = "Distribution",
                              choices = c("Normal" = "rnorm",
                                          "Uniform" = "runif"),
                              selected = "rnorm"),
                  sliderInput("group2samsize", "Sample size:",
                              min = 3, max = 60, value = 10, step= 1),
                  uiOutput("group2mainSliders"),
                  uiOutput("group2mainSliders2")
                  ) # End box
                
                ),
              fluidRow(
                box(height = 500,
                    width = 6, status = "info", solidHeader = TRUE,
                    title = "Results",
                    plotOutput(outputId = "testResults_plot", width = "100%")
                ), # End box for testResults_plot
                box(height = 200,
                    width = 6, status = "info", solidHeader = TRUE,
                    title = "Confidence Interval",
                    sliderInput(inputId = "setCI", label = "",
                                min = 0.800, max = .999, step = 0.001, value = 0.95)
                ),
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "t distributions",
                  plotOutput("dist_plot",
                             dblclick = "plot1_dblclick",
                             brush = brushOpts(
                               id = "plot1_brush",
                               resetOnNew = TRUE
                             ))
                )# End box
              )# End fluid row
              
      ), # End tabItem
            
      # Tab for the raw data:
      tabItem("rawdata",
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Raw Data",
              # numericInput("maxrows", "Rows to show", 25),
              # verbatimTextOutput("rawtable"),
              downloadButton('downloadData', 'Download Data Set')

                    ), # End box
                
                
                
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Input Data",
                  # verbatimTextOutput(outputId = "ttest2")
                  tableOutput('broom')
                ) # End box for tidy broom data
                
              )
              
      ) # End tabItem rawdata
      
    ) # End tabItems
  ) # End Dashboard body
)
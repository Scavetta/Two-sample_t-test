# Two-sample t-test, v2016, Dashboard - UI

dashboardPage(
  dashboardHeader(title = "Two-sample t-test"),
  dashboardSidebar(
    # sidebarMenu(

      menuItem("Group 1", icon = icon("bar-chart"),
             selectInput(inputId = "group1dist",
                         label = "Distribution",
                         choices = c("Normal" = "rnorm",
                                     "Uniform" = "runif"),
                         selected = "rnorm"),
             sliderInput("samsize", "Sample size:",
                                  min = 3, max = 60, value = 10, step= 1),
              uiOutput("mainSliders"),
               uiOutput("mainSliders2")
             ), # End Menu item for group 1
      
      menuItem("Group 2", icon = icon("bar-chart"),
             selectInput(inputId = "group2dist",
                         label = "Distribution",
                         choices = c("Normal" = "rnorm",
                                     "Uniform" = "runif"),
                         selected = "rnorm"),
             sliderInput("group2samsize", "Sample size:",
                         min = 3, max = 60, value = 10, step= 1),
               uiOutput("group2mainSliders"),
               uiOutput("group2mainSliders2")
             ), # End Menu item for group 2
    
    # Pick Hypothesis Testing
    menuItem("Hypothesis Testing", icon = icon("bar-chart"),
             checkboxInput(inputId = "onestt",    label = "Two-sample t-test"),
             #   checkboxInput(inputId = "wsrt", label = "Wilcoxon signed-rank test"),
             conditionalPanel(
               condition = "input.onestt == true",
               sliderInput(inputId = "setCI", label = "Confidence Interval",
                           min = 0.800, max = .999, step = 0.001, value = 0.95)
               ) # End conditionalPanel
             ), # End Menu item for Hypothesis testing
    # ), # End sidebar Menu
    

    # Menu items for body    
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard"),
        menuItem("Raw data", tabName = "rawdata"))

    ), # End dashboardSidebar
    
  dashboardBody(
    tabItems(
      
      # tab for the dashboard
      tabItem("dashboard",
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Input Data",
                  plotOutput(outputId = "main_plot", width = "100%", height = 600)
                ), # End box
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "t-test distributuions",
                  plotOutput("dist_plot",
                             dblclick = "plot1_dblclick",
                             brush = brushOpts(
                               id = "plot1_brush",
                               resetOnNew = TRUE
                             ))
                ) # End box
                ), # End fluid row
              
              # Second row
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Input Data",
                  conditionalPanel("input.onestt == true",
                                   p(strong("Two-sample t-test")),
                                   verbatimTextOutput(outputId = "ttest2")
                                   ) # End conditional panel
                  ) # End box
              ) # End fluid row
      ), # End tabItem
      
      # Tab for the raw data:
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton('downloadData', 'Download Data Set')
      ) # End tabItem rawdata
      
    ) # End tabItems
  ) # End Dashboard body
)
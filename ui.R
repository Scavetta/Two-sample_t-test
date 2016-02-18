# Two-sample t-test, v2016, Dashboard - UI

dashboardPage(
  dashboardHeader(title = "t-test"),
  dashboardSidebar(
    # sidebarMenu(
    # Menu items for body    
    sidebarMenu(
      menuItem("One-sample", tabName = "oneSamT"),
      menuItem("Two-sample, Two-sided", tabName = "dashboard"))
      # menuItem("Hypothesis testing", tabName = "testing"),
      # menuItem("Raw data", tabName = "rawdata"))
  
  ), # End dashboardSidebar
  
    
  dashboardBody(
    tabItems(
      ####################################
      # tab for "dashboard" for two-sided
      tabItem("oneSamT",
              fluidRow(
                box(height = 500,
                    width = 4, status = "info", solidHeader = TRUE,
                    title = "Raw data plot",
                    plotOutput(outputId = "main_plotOneSam", width = "100%")
                ), # End box
                box(height = 500,
                    width = 4, status = "info", solidHeader = TRUE,
                    title = "Data-set:",
                    
                    selectInput(inputId = "group1distOneSam",
                                label = "Distribution",
                                choices = c("Normal" = "rnorm",
                                            "Uniform" = "runif"),
                                selected = "Normal"
                    ),
                    
                    sliderInput("samsizeOneSam", "Sample size:",
                                min = 3, max = 60, value = 10, step= 1),
                    
                    uiOutput("mainSlidersOneSam"),
                    
                    uiOutput("mainSliders2OneSam")
                    ), # End box
                box(height = 570,
                    width = 4, status = "info", solidHeader = TRUE,
                    title = "Controls",

                    sliderInput("testMeanOneSam", "Hypothesised mean:",
                                min = 0, max = 25, value = 0, step= 0.5),
                    sliderInput(inputId = "setCIOneSam", label = "Confidence level:",
                                min = 0.800, max = .999, step = 0.001, value = 0.95),
                    radioButtons("OneSamRadio", label = "Area under the curve:",
                                 choices = list("Rejection region" = 1,
                                                "p-value" = 2
                                 ),
                                 selected = 1),
                    p(strong("Results")),
                    tableOutput('broomOneSam')
                    ) # End box
                
              ),
              fluidRow(
                # box(height = 500,
                #     width = 6, status = "info", solidHeader = TRUE,
                #     title = "Results",
                #     plotOutput(outputId = "testResults_plot", width = "100%")
                # ), # End box for testResults_plot
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "t distributions",
                  plotOutput("dist_plotOneSam",
                             dblclick = "plot1_dblclickOneSam",
                             brush = brushOpts(
                               id = "plot1_brushOneSam",
                               resetOnNew = TRUE
                             )
                             )
                )# End box
              ), # End fluid row
              fluidRow(
                downloadButton('downloadDataOneSam', 'Download Data Set')
                # box(
                #   width = 6, status = "info", solidHeader = TRUE,
                #   title = "Results (Table)",
                #   # verbatimTextOutput(outputId = "ttest2")
                #   tableOutput('broomOneSam')
                # ) # End box for tidy broom data
                
              )
      ), # End tabItem for one-sided
      
      ####################################
      # tab for "dashboard" for two-sided
      tabItem("dashboard",
              fluidRow(
                box(height = 500,
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Raw data plot",
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
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Results (Table)",
                  # verbatimTextOutput(outputId = "ttest2")
                  tableOutput('broom')
                ), # End box for tidy broom data
                
                # box(height = 500,
                #     width = 6, status = "info", solidHeader = TRUE,
                #     title = "Results",
                #     p("Assuming equal variance? N = No (Welch's Variant), Y = Yes"),
                #     plotOutput(outputId = "testResults_plot", width = "100%")
                # ), # End box for testResults_plot
                box(#height = 200,
                    width = 6, status = "info", solidHeader = TRUE,
                    title = "Controls",
                    sliderInput(inputId = "setCI", label = "t-test Confidence level",
                                min = 0.800, max = .999, step = 0.001, value = 0.95),
                    radioButtons("TwoSamRadio", label = "Area under the curve:",
                                 choices = list("Rejection region" = 1,
                                                "p-value" = 2
                                 ),
                                 selected = 1)
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
              ), # End fluid row
              fluidRow(
                downloadButton('downloadData', 'Download Data Set')
              )
              
              
              
              
      ) # End tabItem
            


      
            
      # # Tab for the raw data:
      # tabItem("rawdata",
      #         fluidRow(
      #           box(
      #             width = 6, status = "info", solidHeader = TRUE,
      #             title = "Raw Data",
      #         # numericInput("maxrows", "Rows to show", 25),
      #         # verbatimTextOutput("rawtable"),
      #         downloadButton('downloadData', 'Download Data Set')
      # 
      #               ), # End box
      #           
      #           
      #           
      #           box(
      #             width = 6, status = "info", solidHeader = TRUE,
      #             title = "Input Data",
      #             # verbatimTextOutput(outputId = "ttest2")
      #             tableOutput('broom')
      #           ) # End box for tidy broom data
      #           
      #         )
      #         
      # ) # End tabItem rawdata
      
    ) # End tabItems
  ) # End Dashboard body
)
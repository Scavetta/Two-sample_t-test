# Two-sample t-test, v2016, r2 - server
function(input, output) {

df <- reactive({
  # Group 1 parameters
  samsize <- input$samsize  
  group1dist <- input$group1dist
  g1Mean <- input$g1Mean 
  g1SD <- input$g1SD
  x_range <- input$x_range

  # Group 2 parameters
  group2samsize <- input$group2samsize  
  group2dist <- input$group2dist
  g2Mean <- input$g2Mean 
  g2SD <- input$g2SD
  g2x_range <- input$g2x_range
  
  if (group1dist=="runif") {
    df1 <- do.call(group1dist, list(n = samsize, min = min(x_range), max = max(x_range))) # Get uniform distributuion if asked for 
  }
  else {
    df1 <- do.call(findValues, list(n = samsize, s = g1SD, u = g1Mean)) # or else get normal dist from the dataframe
  }
  
  if (group2dist=="runif") {
    df2 <- do.call(group2dist, list(n = group2samsize, min = min(g2x_range), max = max(g2x_range)))
  }
  else {
    df2 <- do.call(findValues, list(n = group2samsize, s = g2SD, u = g2Mean))
  }
  
  #return(data.frame(group1 = df, group2 = df2))
  # return(c(df1,df2))
  
  dataset <- data.frame(value = c(df1,df2))

  dataset$variable <- c(rep(1,samsize), rep(2,group2samsize))
  
  return(dataset)
})

# ------------------------------------------------------------------
# Create main plot 

output$main_plot <- renderPlot({
  
  samsize <- input$samsize  
  group2samsize <- input$group2samsize  
  
  # df.raw <- as.data.frame(df())

  # df.raw$cat <- c(rep("group1",samsize), rep("group2",group2samsize))
  
  # names(df.raw) <-  c("value","variable")
  
  df.raw <- df()
  df.raw$variable <- as.factor(df.raw$variable)
  testMean <- 1 # place holder for now
  
  ggplot(data = df.raw, aes(x = variable, y = value, col = variable)) + 
    geom_jitter(aes(x = variable), position = position_jitter(width = .05), alpha = 0.5, size = 3) +
    scale_x_discrete(labels = c("Group 1", "Group 2")) +
    scale_colour_manual(values = c(DarkGreen, DarkOrange), guide = FALSE) +
    stat_summary(fun.y = mean, geom = "point", fill = "red", col = "black") + 
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.04, col = "black") +
    labs(y = "Values", x = "Group") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_line(colour = "black"),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(colour = "black"),
          axis.text.y = element_text(colour = "black")
          )
  
}) # end of render plot "main_plot"

# ------------------------------------------------------------------
# Create distribution plot 
ranges <- reactiveValues(x = NULL, y = NULL)

output$dist_plot <- renderPlot({
# req(input$onestt)
x <- seq(-5, 5, length=6000)
df.raw <- df()

setCI <- input$setCI
samsize <- input$samsize  
group2samsize <- input$group2samsize  
df.raw <- as.data.frame(df())
# df.raw$cat <- c(rep("group1",samsize), rep("group2",group2samsize))
names(df.raw) <-  c("value","Group")

tline <- t.test(value ~ Group, data = df.raw, conf.level = setCI)
degf <- tline$parameter
tline <- tline$statistic # Get the test statistic to draw a vertical line

tline2 <- t.test(value ~ Group, data = df.raw, conf.level = setCI, var.equal = TRUE)
degf2 <- tline2$parameter
tline2 <- tline2$statistic # Get the test statistic to draw a vertical line

# Calculate t-dist curve:
test.df <- data.frame(value = x, tdist = dt(x, degf), tdist2 = dt(x, degf2))
test.df <- melt(test.df, id.vars = "value")
names(test.df) <- c("value", "dist", "distval")

q <- ggplot(test.df, aes (x= value, y = distval, group = dist, colour = dist)) +
  geom_line() +
  scale_colour_manual("Assuming equal variance?", 
                      values = c(MidRed, MidBlue), 
                      labels = c(paste0("No (Welch's Variant), df = ", round(degf, 2)),
                                 paste0("Yes, df = ", round(degf2,2)) )) +
  scale_y_continuous("pdf(x)", expand = c(0,0)) +
  scale_x_continuous("t", expand = c(0,0)) +
  ggtitle("t Distribution") +
  coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
  theme( panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.ticks.x = element_line(colour = "black"),
         axis.ticks.y = element_line(colour = "black"),
         axis.line = element_line(colour = "black"),
         axis.text.x = element_text(colour = "black"),
         axis.text.y = element_text(colour = "black"),
         legend.position = c(0.8, 0.8),
         legend.key = element_blank(),
         legend.background = element_blank()
         
  )

    q +
      geom_ribbon(data=subset(test.df, value > qt(setCI+((1-setCI)/2), df=degf) & dist == "tdist"),
                  aes(x=value,ymax=distval),ymin=0,fill=MidRed, alpha=0.4, col = NA) +
      geom_ribbon(data=subset(test.df, value < qt((1-setCI)/2, df=degf) & dist == "tdist"),
                  aes(x=value,ymax=distval),ymin=0,fill=MidRed, alpha=0.4, col = NA) + 
      geom_vline(xintercept = tline, col = MidRed) +
      
      geom_ribbon(data=subset(test.df, value > qt(setCI+((1-setCI)/2), df=degf2) & dist == "tdist2"),
                aes(x=value,ymax=distval),ymin=0,fill=MidBlue, alpha=0.4, col = NA) +
      geom_ribbon(data=subset(test.df, value < qt((1-setCI)/2, df=degf2) & dist == "tdist2"),
                aes(x=value,ymax=distval),ymin=0,fill=MidBlue, alpha=0.4, col = NA) + 
      geom_vline(xintercept = tline2, col = MidBlue)

  

}) # end of render plot "dist_plot"


# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent(input$plot1_dblclick, {
  brush <- input$plot1_brush
  if (!is.null(brush)) {
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
    
  } else {
    ranges$x <- NULL
    ranges$y <- NULL
  }
})

# ------------------------------------------------------------------
# Redefine UI options, dependent on distribution:
# Want two sliders for rnorm, but renderUI gives an error with the comma,
# So i just make two output$ with renderUI

# Group 1 slider for mean and data range of uniform dist:
output$mainSliders <- renderUI({
     
    if (input$group1dist=="rnorm") {
      sliderInput("g1Mean", "Mean:", 
                  min = 0, max = 25, value = 0, step= 0.5)
    } else if (input$group1dist=="runif") {
      sliderInput(inputId = "x_range",
                  label = paste("Data range"),
                  min = 0, max = 25, value = c(10, 20))
    }  
  })  

# Group 1 slider for SD:
output$mainSliders2 <- renderUI({
  if (input$group1dist=="rnorm") {
      sliderInput("g1SD", "Standard Deviation",
                  min = 1, max = 15, value = 6, step= 1)
  }
})

# Group 2 slider for mean and data range of uniform dist:
output$group2mainSliders <- renderUI({
  
  if (input$group2dist=="rnorm") {
    sliderInput("g2Mean", "Mean:", 
                min = 0, max = 25, value = 10, step= 0.5)
    
  } else if (input$group2dist=="runif") {
    sliderInput(inputId = "g2x_range",
                label = paste("Data range"),
                min = 0, max = 25, value = c(10, 20))
  }  
})  

# Group 2 slider for SD:
output$group2mainSliders2 <- renderUI({

  if (input$group2dist=="rnorm") {
    sliderInput("g2SD", "Standard Deviation",
                min = 1, max = 15, value = 6, step= 1)

  }
})

# Prepare table:
output$rawtable <- renderPrint({
  orig <- options(width = 1000)
  print(tail(df(), input$maxrows))
  options(orig)
})



# ------------------------------------------------------------------
# Functions for creating models and printing summaries
output$ttest2 <- renderPrint({
  
  setCI <- input$setCI
  samsize <- input$samsize
  group2samsize <- input$group2samsize
  df.raw <- df()
  # df.raw <- as.data.frame(df.raw)
  # df.raw$cat <- c(rep("group1",samsize), rep("group2",group2samsize))
  names(df.raw) <-  c("value","Group")
  # print(df.raw$value)
  print(t.test(value ~ Group, data = df.raw, conf.level = setCI))
  print(t.test(value ~ Group, data = df.raw, conf.level = setCI, var.equal = TRUE))
  })

# Download data set:
output$downloadData <- downloadHandler(
  filename = function() { 'data.csv' },
  content = function(file) {
    write.csv(as.data.frame(df()), file, row.names = FALSE)
  }
)

}

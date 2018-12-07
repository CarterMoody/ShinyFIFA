# INSTALL SHINY DEPLOY PACKAGE
#   install.packages('rsconnect')
#
# DEPLOY APP
#   rsconnect::deployApp('C:/College/stat331/FIFA/ShinyFIFA')
#
# SHOW LOGS
#   rsconnect::showLogs(account = "cartermoody", appName = "ShinyFIFA")

library(ggplot2)
library(tidyverse)
library(mosaic)
library(magrittr)
library(RColorBrewer)
library(rsconnect)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(maps)

source("setup.R") # Load Data From RScript

ui <- navbarPage("FIFA",
                 
                 # Tab 1 INTERACTIVE
                 tabPanel("Bivariate Regression",
                          fluidPage(
                                
                             sidebarLayout(
                                sidebarPanel(
                                   selectInput('xcol', 'X Variable', FIFAVars,
                                               selected=FIFAVars[[7]]),
                                   selectInput('ycol', 'Y Variable', FIFAVars,
                                               selected=FIFAVars[[8]]),
                                   
                                # Create Checkboxes for Positions
                                checkboxGroupInput("checkGroup",
                                                   label = h3("Preferred Position"),
                                                   choices = CheckBoxChoices,
                                                   selected = CheckBoxChoices2),
                                hr(),
                                
                              # Create Slider Input
                              sliderInput("slider1", label = h3("Alpha"), min = 0,
                                          max = 1, value = .25),
                              hr(),
                              # End Slider Input
                              
                              # Create Radio Buttons for Color Selection
                              radioButtons("radio", label = h3("Color"),
                                           choices = ColorList,
                                           selected = "purple"
                              ),
                              hr()
                              

                              
                              ),
                              
                              mainPanel(
                                 # Plot Two Variables
                                 plotOutput('plot1'),
                                 # Print Summary of Linear Model
                                 verbatimTextOutput('regression')
                                 )
                              )
                          )
                 ),
                 # End Tab 1 INTERACTIVE
                 
                 # Tab 2 DOTPLOT
                 tabPanel("DotPlot",
                          fluidPage(
                             sidebarLayout(
                                sidebarPanel(
                                   selectInput('xcol2', 'X Variable', FIFAVars),
                                   
                                   # Create Checkboxes for Positions
                                   checkboxGroupInput("checkGroup2",
                                                      label = h3("Preferred Position"),
                                                      choices = CheckBoxChoices,
                                                      selected = CheckBoxChoices2),
                                   hr()
                                   
                                ),
                                mainPanel(
                                   plotOutput('DotPlot')
                                )
                             )
                          )
                  ), 
                 # End Tab 2 DOTPLOT
                 
                 # Tab 3 HISTOGRAM
                 tabPanel("Histogram",
                          fluidPage(
                             sidebarLayout(
                                sidebarPanel(
                                   selectInput('xcol3', 'X Variable', FIFAVars,
                                               selected = FIFAVars[[22]]),
                                   
                                   # Create Checkboxes for Positions
                                   checkboxGroupInput("checkGroup3",
                                                      label = h3("Preferred Position"),
                                                      choices = CheckBoxChoices,
                                                      selected = CheckBoxChoices2
                                                      ),
                                   hr(),
                                   
                                   # Create Radio Buttons for Color Selection
                                   radioButtons("radio1", label = h3("Color"),
                                                choices = ColorList
                                                ),
                                   hr()
                                   
                                ),
                                mainPanel(
                                   plotOutput('Histogram')
                                )
                             )
                          )
                 ),
                 # End Tab 3 HISTOGRAM
                 
                 # Tab 4 QQPLOT
                 tabPanel("qqPlot",
                          fluidPage(
                             sidebarLayout(
                                sidebarPanel(
                                   selectInput('xcol4', 'X Variable', FIFAVars,
                                               selected = FIFAVars[[21]]),
                                   
                                   # Create Checkboxes for Positions
                                   checkboxGroupInput("checkGroup4",
                                                      label = h3("Preferred Position"),
                                                      choices = CheckBoxChoices,
                                                      selected = CheckBoxChoices2),
                                   hr()
                                   
                                ),
                                mainPanel(
                                   plotOutput('qqPlot')
                                )
                             )
                          )
                 ),
                 
                 
                 # End Tab 4 QQPLOT

                 
                 # Tab 5 MAP
                 tabPanel("Map",
                          fluidPage(
                             
                             sidebarLayout(
                                sidebarPanel(
                                   selectInput('xcol5', 'X Variable', FIFAVars2,
                                               selected=FIFAVars2[[18]]),
                                   
                                   # Create Checkboxes for Positions
                                   checkboxGroupInput("checkGroup5",
                                                      label = h3("Preferred Position"),
                                                      choices = CheckBoxChoices,
                                                      selected = CheckBoxChoices2),
                                   hr(),
                                   
                                   # Create Slider Input
                                   sliderInput("sliderRange", label = h3("Range"), min = 0,
                                               max = 100, value = c(40,60)),
                                   hr(),
                                   # End Slider Input
                                   
                                   # Create Radio Buttons for Color Selection
                                   radioButtons("radio2", label = h3("Color"),
                                                choices = ColorList,
                                                selected = "cyan"
                                   ),
                                   hr()
                                   
                                   
                                   
                                ),
                                
                                mainPanel(
                                   # Plot Two Variables
                                   plotOutput('map')
                                )
                             )
                          )
                 ),
                 # End Tab 5 MAP
                 
                 
                 # Tab 6 POSITIONKEY
                 tabPanel("PositionKey",
                          fluidPage(
                             imageOutput("image")
                          ))
                 # End Tab 6 POSITIONKEY
         
)

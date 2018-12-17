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

# Navbar Page Object, Main Title: "FIFA", allows switching
#   between multiple tab panels
ui <- navbarPage("FIFA",
                 
                 # Tab 1 INTERACTIVE
                 tabPanel("Bivariate Regression",
                          fluidPage(
                                
                             sidebarLayout(
                                sidebarPanel(
                                   # Define SelectInput Widgets for Selecting X Variable
                                   selectInput('xcol', 'X Variable', FIFAVars,
                                               selected=FIFAVars[[7]]), # Selects "Compsure Attribute"
                                   selectInput('ycol', 'Y Variable', FIFAVars,
                                               selected=FIFAVars[[8]]), # Selects "Crossing" Attribute
                                   
                                # Create Checkboxes for Positions
                                checkboxGroupInput("checkGroup",
                                                   label = h3("Preferred Position"),
                                                   choices = CheckBoxChoices,    # List of all Positions
                                                   selected = CheckBoxChoices2), # Selects all but "GK" Position
                                hr(),
                                
                              # Create Slider Input
                              # Adjusts for opacity of points on graph
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
                              # End Radio Buttons
                                  
                              ),
                              
                              mainPanel(
                                 # Display the "plot1" Object in server.r
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
                                                      choices = CheckBoxChoices,    # List of All Positions
                                                      selected = CheckBoxChoices2), # Selects all but "GK" Position
                                   hr()
                                   
                                ),
                                # Display the "DotPlot" Object in server.r
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
                                               selected = FIFAVars[[22]]),  # Selects "SprintSpeed" Attribute
                                   
                                   # Create Checkboxes for Positions
                                   checkboxGroupInput("checkGroup3",
                                                      label = h3("Preferred Position"),
                                                      choices = CheckBoxChoices,   # List of All Positions
                                                      selected = CheckBoxChoices2  # Selects all but "GK" Position
                                                      ),
                                   hr(),
                                   
                                   # Create Radio Buttons for Color Selection
                                   radioButtons("radio1", label = h3("Color"),
                                                choices = ColorList
                                                ),
                                   hr()
                                   # End Radio Buttons
                                   
                                ),
                                # Display the "Histogram" Object in server.r
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
                                               selected = FIFAVars[[21]]),   # Selects "ShortPassing" Attribute
                                   
                                   # Create Checkboxes for Positions
                                   checkboxGroupInput("checkGroup4",
                                                      label = h3("Preferred Position"),
                                                      choices = CheckBoxChoices,    # List of All Positions
                                                      selected = CheckBoxChoices2), # Selects all but "GK" Position
                                   hr()
                                   
                                ),
                                # Display the "qqplot" Object in server.r
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
                                               selected=FIFAVars2[[18]]),   # Selects "Overall" Attribute
                                   
                                   # Create Checkboxes for Positions
                                   checkboxGroupInput("checkGroup5",
                                                      label = h3("Preferred Position"),
                                                      choices = CheckBoxChoices,    # List of All Positions
                                                      selected = CheckBoxChoices2), # Selects all but "GK" Position
                                   hr(),
                                   
                                   # Create Slider Input
                                   # Selected Attribute (xcol5) Must be within this chosen range
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
                                   # End Radio Buttons
                                ),
                                
                                # Plot the "map" Object from server.r
                                mainPanel(
                                   plotOutput('map')
                                )
                             )
                          )
                 ),
                 # End Tab 5 MAP
                 
                 # Tab 6 POSITIONKEY
                 tabPanel("PositionKey",
                          fluidPage(
                             # Display the Player Position Key/Legend "Image" Object loaded in server.r
                             imageOutput("image")
                          ))
                 # End Tab 6 POSITIONKEY
         
)

##########################################################################################
#### THE FOLLOWING ARE USEFUL COMMANDS FOR DEPLOYING THE APPLICATION TO SHINY SERVERS ####
#### THEY DO NOT PERTAIN TO ANY CODE INCLUDED IN ABOVE FILE DO NOT INCLUDE IN COMPILE ####
##########################################################################################
# INSTALL SHINY DEPLOY PACKAGE
#   install.packages('rsconnect')
#
# DEPLOY APP
#   rsconnect::deployApp('C:/College/stat331/FIFA/ShinyFIFA')
#
# SHOW LOGS
#   rsconnect::showLogs(account = "cartermoody", appName = "ShinyFIFA")
##########################################################################################

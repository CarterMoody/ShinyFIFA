library(ggplot2)
library(tidyverse)
library(mosaic)
library(magrittr)
library(RColorBrewer)
library(rsconnect)
library(stringi)
library(maps)

source("setup.R") # Load Data From RScript

function(input, output, session) {
   
   # Create Input Buttons for Each TabPanel
   output$xcol = renderPrint({ input$xcol })
   
   output$ycol = renderPrint({ input$ycol })
   
   output$xcol2 = renderPrint({ input$xcol })
   
   output$xcol3 = renderPrint({ input$xcol })
   
   output$xcol4 = renderPrint({ input$xcol })
   
   output$checkGroup = renderPrint({ input$checkGroup })
   
   output$checkGroup2 = renderPrint({ input$checkGroup })
   
   output$checkGroup3 = renderPrint({ input$checkGroup })
   
   output$checkGroup4 = renderPrint({ input$checkGroup })
   
   output$slider = renderPrint({ input$slider1})
   
   output$radio = renderPrint({ input$radio })
   
   output$radio1 = renderPrint({ input$radio })
   
   # Image for PositionKey Tab
   # Image is Key/Legend of Player Positions for those unfamiliar with Standard Soccer Positions
   output$image = renderImage({
      return(list(
         src = "playerpositions.png",
         filetype = "image/png",
         alt = "fifapos"))
   }, deleteFile = FALSE)
   
   # Map Tab Object Creation
   output$xcol5 = renderPrint({ input$xcol })               # X Variable Dynamic Input ( Player Attribute)
   output$sliderRange = renderPrint({ input$slider2 })      # Slider Variable Dynamic Input ( Attribute Range)
   output$checkGroup5 = renderPrint({ input$checkGroup })   # Check Group Dynamic Input ( Preferred Position )
   output$radio2 = renderPrint({ input$radio })             # Radio Group Dynamic Input ( Color )
   
   
   # Plot Dotplot of Two Variables, Filtered by Position
   output$plot1 = renderPlot({
      SelectedPositions1 = paste(input$checkGroup, collapse = "|")
      FIFA %>%
         # Create Temporary DataFrame, Including Only Selected Positions
         filter( grepl(SelectedPositions1, PreferredPositions) ) %>%
         ggplot(aes_string(x = input$xcol, y = input$ycol)) +
         geom_point(col = input$radio, alpha = (input$slider1) ) +
         geom_smooth(method = "lm", color = "black", se = FALSE)
      
   })
   
   # Print out Linear Model of Two Variables, Filtered by Position
   output$regression = renderPrint({
      SelectedPositions1 = paste(input$checkGroup, collapse = "|")
      # Create Temporary DataFrame, Including Only Selected Positions
      FIFA2 = FIFA %>%
         filter(grepl(SelectedPositions1, PreferredPositions))
      
      # Error Checking, No Positions are Selected
      if (length(FIFA2$X1) == 0){
         stop("No Player Data Meets Constraints")
      } else {
      summary(
         lm(formula = eval(parse(text = input$xcol)) ~ 
            eval(parse(text = input$ycol)), data = FIFA2))
      }
   })
   
   # Display Dotplot of One Variable
   output$DotPlot = renderPlot({
      SelectedPositions2 = paste(input$checkGroup2, collapse = "|")
      # Create Temporary DataFrame, Including Only Selected Positions
      FIFA3 = FIFA %>%
         filter( grepl(SelectedPositions2, PreferredPositions))
      
      dotPlot( ~eval(parse(text = input$xcol2)), data=FIFA3,
               # Display title relevant to attribute selection
               main = paste( parse(text = input$xcol2), " DotPlot"),
               xlab = input$xcol2)
   })
   
   # Display Histogram of One Variable
   output$Histogram = renderPlot({
      SelectedPositions3 = paste(input$checkGroup3, collapse = "|")
      # Create Temporary DataFrame, Including Only Selected Positions
      FIFA4 = FIFA %>%
         filter( grepl(SelectedPositions3, PreferredPositions))
      
      hist.with.normal(   pull(FIFA4[,input$xcol3]),
                                   # Display title relevant to attribute selection
                                   paste(parse(text = input$xcol3),"Histogram"), 
                          col = input$radio1)
   })
   
   # Display qqPlot of One Variable
   output$qqPlot = renderPlot({
      SelectedPositions4 = paste(input$checkGroup4, collapse = "|")
      # Create Temporary DataFrame, Including Only Selected Positions
      FIFA5 = FIFA %>%
         filter( grepl(SelectedPositions4, PreferredPositions))
      
      # Display title relevant to attribute selection
      qqplot( pull(FIFA5[,input$xcol4]),
              title = paste(parse(text = input$xcol4), "qqPlot")
              )
   })
   
   # Display Map Output
   output$map = renderPlot({
      SelectedPositions5 = paste(input$checkGroup5, collapse = "|")
      # Create Temporary DataFrame, Including Only Selected Positions
      FIFA6 = FIFA %>%
         filter( grepl(SelectedPositions5, PreferredPositions))
      # Further Adjust Temporary DataFrame, Including Only Selected Positions within range of Slider Input
      FIFA6 = FIFA6 %>%
         filter( ( eval(parse(text = input$xcol5)) > input$sliderRange[1]) & 
                 ( eval(parse(text = input$xcol5)) < input$sliderRange[2]) )
      
      #worldMapObj = map("world", plot = F)
      # Declared in Setup.R
      
      # Error Checking, No Data Meets Constraints
      if (length(FIFA6$X1) == 0){
         # Stop App and Display Error Message
         stop("No Player Data Meets Constraints")
      } else { # Data Meets Constraints

         newregions = character(length(worldMapObj$names))
      
       for( i in 1:length(FIFA6$X1)){ # For Every Data Entry in FIFA Dataset
            curRegion = paste(FIFA6$Nationality[i], sep = "")
            # Create a list of Regions from the worldMap Object which contain the curRegion string
            ind = grep(curRegion, worldMapObj$names) 
         
            if (length(ind) != 0){ # There are Regions Found
               # Insert Regions Found into MasterList of Regions which match constraints
               newregions[ind] = c(worldMapObj$names[ind])
            }
         }
         # Remove NA Values from MasterList of Regions
         newregions = newregions[newregions != ""]
         # Finally, Create Map Object with Selected Regions only within Data Constraints
         map("world", regions = newregions, col = input$radio2, fill = TRUE)
      } # END Error Check Else
      
   })
   
}

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
 

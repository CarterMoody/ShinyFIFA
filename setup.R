# Create Functions Manipulate Data
#### Data Manipulation ####

# setwd("C:\\College\\stat331\\FIFA\\ShinyFIFA")

library(stringi)

# Read in FIFA Dataset
FIFA = read_csv("completedataset.csv")

# Replace Spaces with EmptyString
names(FIFA) %<>%
   gsub(pattern = " ", replacement = "")

FIFA$Wage %<>%
   gsub(pattern = "K", replacement = "")

FIFA$Value %<>%
   gsub(pattern = "M", replacement = "")

# Transform Character Vectors into Integers
FIFA %<>%
   
   mutate(Acceleration = as.integer(Acceleration),
          Aggression = as.integer(Aggression),
          Agility = as.integer(Agility),
          Balance = as.integer(Balance),
          BallControl = as.integer(Ballcontrol),
          Composure = as.integer(Composure),
          Crossing = as.integer(Crossing),
          Curve = as.integer(Curve),
          Dribbling = as.integer(Dribbling),
          Finishing = as.integer(Finishing),
          FreeKickAccuracy = as.integer(Freekickaccuracy),
          HeadingAccuracy = as.integer(Headingaccuracy),
          Interceptions = as.integer(Interceptions),
          Jumping = as.integer(Jumping),
          LongPassing = as.integer(Longpassing),
          LongShots = as.integer(Longshots),
          Penalties = as.integer(Penalties),
          Stamina = as.integer(Stamina),
          ShortPassing = as.integer(Shortpassing),
          ShotPower = as.integer(Shotpower),
          SprintSpeed = as.integer(Sprintspeed),
          Strength = as.integer(Strength),
          Vision = as.integer(Vision),
          Wage = as.integer(
             stri_sub(str = FIFA$Wage, from = 2, to = length(FIFA$Wage))
          ),
          Worth = as.integer(
             stri_sub(str = FIFA$Value, from = 2, to = length(FIFA$Value))
          )
   )

# Change United States to USA for Map Tab
FIFA %<>%
   mutate(Nationality = case_when(
      Nationality == "United States" ~ "USA",
      Nationality == "China PR" ~ "China",
      #Nationality == "Kazakhstan" ~ "",
      Nationality != "United States" ~ Nationality
     # grepl(Nationality, "China") ~ "China" 
   ))

# Create Vector of Usable Integer Variable Names (For Selection in UI)
FIFAVars = c("Age", "Acceleration", "Aggression", "Agility", "Balance", 
             "BallControl",
             "Composure", "Crossing", "Curve", "Dribbling", "Finishing", 
             "FreeKickAccuracy","HeadingAccuracy", "Interceptions", "Jumping",
             "LongPassing", "LongShots", "Overall", "Penalties", 
             "Stamina","ShortPassing", "SprintSpeed", "Strength", "Vision",
             "Wage", "Worth")

# Create Vector of Usable Integer Variable Names (For Selection in UI)
# This one Special for Map Page (All values range between 0 and 100)
FIFAVars2 = c("Age", "Acceleration", "Aggression", "Agility", "Balance", 
             "BallControl",
             "Composure", "Crossing", "Curve", "Dribbling", "Finishing", 
             "FreeKickAccuracy","HeadingAccuracy", "Interceptions", "Jumping",
             "LongPassing", "LongShots", "Overall", "Penalties", 
             "Stamina","ShortPassing", "SprintSpeed", "Strength", "Vision")

# Set up List of Preferred Position Choices
CheckBoxChoices = list(
   "CAM" = "CAM",
   "CB" = "CB",
   "CDM" = "CDM",
   "CF" = "CF",
   "CM" = "CM",
   
   #"LAM" = "LAM",
   "LB" = "LB",
   #"LCB" = "LCB",
   #"LDM" = "LDM",
   #"LF" = "LF",
   "LM" = "LM",
   #"LS" = "LS",
   "LW" = "LW",
   "LWB" = "LWB",
   
   #"RAM" = "RAM",
   "RB" = "RB",
   #"RCB" = "RCB",
   #"RCM" = "RCM",
   #"RDM" = "RDM",
   #"RF" = "RF",
   "RM" = "RM",
   #"RS" = "RS",
   "RW" = "RW",
   "RWB" = "RWB",
   
   "ST" = "ST",
   
   "GK" = "GK"
   
)

# Removed GK From List, for Pre-Selection of Check Boxes
CheckBoxChoices2 = list(
   "CAM" = "CAM",
   "CB" = "CB",
   "CDM" = "CDM",
   "CF" = "CF",
   "CM" = "CM",
   
   #"LAM" = "LAM",
   "LB" = "LB",
   #"LCB" = "LCB",
   #"LDM" = "LDM",
   #"LF" = "LF",
   "LM" = "LM",
   #"LS" = "LS",
   "LW" = "LW",
   "LWB" = "LWB",
   
   #"RAM" = "RAM",
   "RB" = "RB",
   #"RCB" = "RCB",
   #"RCM" = "RCM",
   #"RDM" = "RDM",
   #"RF" = "RF",
   "RM" = "RM",
   #"RS" = "RS",
   "RW" = "RW",
   "RWB" = "RWB",
   
   "ST" = "ST"
   
#   "GK" = "GK"
   
)

# Set up List of Colors for Histogram
ColorList = list(
   "peachpuff" = "peachpuff",
   "lightpink" = "lightpink",
   "plum" = "plum",
   "purple" = "purple",
   "goldenrod" = "goldenrod",
   "lawngreen" = "lawngreen",
   "cyan" = "cyan",
   "darkturquoise" = "darkturquoise"
)

# Create Map Object for Map Tab
worldMapObj = map("world", plot = F)

#Create Functions

hist.with.normal <- function(x, title, col = "lightblue") {
   hist(x, freq=F, col=col, main = title, las = 1)
   meanx <- mean(x)
   sdx <- sd(x)
   curve(dnorm(x, mean = meanx, sd = sdx), col = "red", add = T)
}

ggplot.hist <- function(x, title) {
   data = as.tibble(x) # convert to tibble data frame
   data %>%
      ggplot(aes(x = value)) +
      geom_histogram(col = "lightblue", binwidth = 5) + # 5 bins is default for hist above
      ggtitle(title)
}

qqplot = function(x, col = "blue", title) {
   qqnorm(x, main = title)
   qqline(x, lty=2, col = col)
}
##
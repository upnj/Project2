
# App Deployment steps

#set up directory
dir.create("C:\\Users\\upnjo\\OneDrive\\Documents\\smartphone_app")
setwd("C:\\Users\\upnjo\\OneDrive\\Documents\\smartphone_app")
dir.create("data")
dir.create("www")

#step 2 save data
data <- read.csv("C:/Users/upnjo/OneDrive/Documents/Project2/user_behavior_dataset.csv")
write.csv(data, "C:\\Users\\upnjo\\OneDrive\\Documents\\smartphone_app\\user_behavior_dataset.csv", row.names= FALSE)

#step 3 create app

file.create("app.R")

# insatll required packages
install.packages(c("shiny", "tidyverse", "DT", "bslib", 
                   "shinycssloaders", "rsconnect"))

#step 5 confiure reconnect
library(rsconnect)
rsconnect::setAccountInfo(name='upnjoshi',
                          token='4873F413974D61414B5AEB7809D7F646',
                          secret='8zFPE+wQ9/fxUV21MZwH5Hc6o8JdypeB6fevN+4f')

#step 6 deploy
rsconnect::deployApp(
  appName = "smartphone_analysis",
  appTitle = "Smartphone Usage Analysis"
)

library(rsconnect)
rsconnect::deployApp("C:/Users/upnjo/OneDrive/Documents/smartphone_app")

################################################################################

# Create a new clean directory
app_dir <- "C:/Users/upnjo/OneDrive/Documents/smartphone_app_new"
dir.create(app_dir)
setwd(app_dir)

# Create www folder
dir.create("www")

library(rsconnect)

rsconnect::removeAccount("upnjoshi")

rsconnect::setAccountInfo(name='upnjoshi',
                          token='4873F413974D61414B5AEB7809D7F646',
                          secret='8zFPE+wQ9/fxUV21MZwH5Hc6o8JdypeB6fevN+4f')

rsconnect::deployApp(appDir = "C:/Users/upnjo/OneDrive/Documents/minimal_app",
                     appName = "minimal_test")

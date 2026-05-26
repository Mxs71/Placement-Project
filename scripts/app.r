library(shiny)
library(bslib)
library(shinydashboard)
library(tidyverse)
library(data.table)


demo_drug <- fread("../data_clean/demo_drug.csv")

source("../scripts/ui.r")
source("../scripts/server.r")

shinyApp(ui, server)


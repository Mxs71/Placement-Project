library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(lubridate)
library(R.utils)
library(fst)
library(ggprism)
library(plotly)
library(rsconnect)


source("../scripts/server.r")
source("../scripts/ui.r")


shinyApp(ui, server)

rsconnect::deployApp(appDir = "scripts/", appFiles = c("app.r", "server.r", "ui.r", "cleaning.r"))

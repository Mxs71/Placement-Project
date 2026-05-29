library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(lubridate)
library(R.utils)
library(fst)
library(ggprism)


source("../scripts/ui.r")
source("../scripts/server.r")


shinyApp(ui, server)


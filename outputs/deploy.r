rsconnect::deployApp(
  appDir = "C:/Users/Maxs1/OneDrive/Documents/GitHub/Placement-Project",
  appPrimaryDoc = "scripts/app.r",
  appFiles = c(
    "scripts/app.r",
    "scripts/server.r",
    "scripts/ui.r",
    "scripts/www/design.css",
    paste0("data_clean/", list.files("data_clean", recursive = TRUE))
  ),
  appName = "placement-project"
)

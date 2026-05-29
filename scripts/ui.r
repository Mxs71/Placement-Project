sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Reporting Trends", tabName = "reports"),
        menuItem("Reaction Profile", tabName = "reaction"),
        menuItem("Outcomes", tabName = "outcomes"),
        menuItem("Population Context", tabName = "population")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "reports",
        box(title = "Reporting Trends and Volumes", 
            plotOutput("reports")),
            fluidRow(
                column(width = 3, box(selectizeInput("selected_drugs", "Drugs", choices = NULL, multiple = TRUE, options = list(maxItems = 2)))),
                column(width = 3, box(selectInput("time_agg", "Time Aggregation", choices = c("week", "month", "quarter", "year")))),
                column(width = 3, box(checkboxInput("rolling", "Show Trend Line", value = FALSE)))
            )
        ),
        tabItem(tabName = "reaction",
        box(title = "Reaction Profiles",
            plotOutput("reactions")),
            fluidRow(
                column(width = 3, box(selectizeInput("selected_drugs_reac", "Drugs", choices = NULL, multiple = TRUE, options = list(maxItems = 2))))
            )
        ),
        tabItem(tabName = "outcomes",
        box(title = "Seriousness and Outcomes",
            plotOutput("outcomes")),
            br(),
            fluidRow(
                column(width = 3, box(selectInput("age", "Age Group", choices = c("Neonate" = "N", "Infant" = "I", "Child" = "C", "Adolescent" = "T", "Adult" = "A", "Elderly" = "E"))))
            )
        ),
        tabItem(tabName = "population",
        box(title = "Population Context",
            "Card Body")
        )
    )
)


ui <- dashboardPage(
    dashboardHeader(title = "FAERS Dashboard"),
    sidebar,
    body
)

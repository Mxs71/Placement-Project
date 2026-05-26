demo_drug <- fread("../data_clean/demo_drug.csv")

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
        card(card_header("Reporting Trends and Volumes"), 
            plotOutput("reports")),
        br(),
        fluidRow(
            column(width = 3, card(selectInput("selected_drugs", "Drugs", choices = unique(demo_drug$normalized))))),
            column(width = 3, card(selectInput("time_agg", "Time Aggregation", choices = c("week", "month", "quarter", "year")))),
            column(width = 3, card(checkboxInput("rolling", "Show Trend Line", value = FALSE)))
        )),
        tabItem(tabName = "reaction",
        card(card_header("Reaction Profiles"),
            "Card Body")),
        tabItem(tabName = "outcomes",
        card(card_header("Seriousness and Outcomes"),
            "Card Body")),
        tabItem(tabName = "population",
        card(card_header("Population Context"),
            "Card Body"))
    )


ui <- dashboardPage(
    dashboardHeader(title = "FAERS Dashboard"),
    sidebar,
    body
)

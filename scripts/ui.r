sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Reporting Trends", tabName = "reports"),
        menuItem("Reaction Profile", tabName = "reaction"),
        menuItem("Outcomes", tabName = "outcomes"),
        menuItem("Population Context", tabName = "population")
    )
)

body <- dashboardBody(
    tags$head(
        includeCSS(if (file.exists("www/design.css")) "www/design.css" else "scripts/www/design.css")
    ),
    tabItems(
        tabItem(tabName = "home",
            titlePanel("Welcome to the FAERS Dashboard"),
            fluidRow(
                div(class = "intro-text",
                    "This is a dashboard designed with data from public extracts from the FDA Adverse Event Reporting System (FAERS).
                     This tool was designed for surveillance of adverse reactions, and the drawing of association and correlations. This tool is not to be used to draw causal relationships, only to establish and suggest potential correlations.
                     This dashboard consists of 4 different modules, each with their different uses, described below."
                )
            ),
            br(),
            fluidRow(
                box(title = "Reporting Trends", "Compare time series of report counts between drugs. Evaluate patterns and trends in reporting.", width = 6),
                box(title = "Reaction Profiles", "View the top reported reactions between selected drugs. Evaluate which reactions dominate reporting, compare drug's reaction profiles.", width = 6)
            ),
            fluidRow(
                box(title = "Outcomes", "View outcome distribution over time, across different age subgroups.", width = 6),
                box(title = "Population Context", "Compare age and sex report distributions across countries and reporter types. Evaluate the demographics of reports, and infer trends in reporting", width = 6)
            )
        ),
        tabItem(tabName = "reports",
            fluidRow(
                column(width = 8,
                    box(title = "Reporting Trends and Volumes", plotlyOutput("reports"), width = 12)
                ),
                column(width = 4,
                    box(selectizeInput("selected_drugs", "Drugs", choices = NULL, multiple = TRUE, options = list(maxItems = 2)), width = 12),
                    box(selectInput("time_agg", "Time Aggregation", choices = c("week", "month", "quarter", "year")), width = 12),
                    box(checkboxInput("rolling", "Show Trend Line", value = FALSE), width = 12),
                    box(selectInput("year_from", "From", choices = available_quarters, selected = min(available_quarters)), width = 12),
                    box(selectInput("year_to", "To", choices = available_quarters, selected = max(available_quarters)), width = 12)
                )
            ),
            fluidRow(
                column(width = 12,
                    valueBoxOutput("box_total", width = 12)
                )
            ),
            fluidRow(
                column(width = 12,
                valueBoxOutput("box_change", width = 12)
                )
            )
        ),
        tabItem(tabName = "reaction",
            fluidRow(
                column(width = 8,
                    box(title = "Reaction Profiles", plotlyOutput("reactions"), width = 12)
                ),
                column(width = 4, 
                    box(selectizeInput("selected_drugs_reac", "Drugs", choices = NULL, multiple = TRUE, options = list(maxItems = 2)), width = 12)
                )
            ),
            fluidRow(
                column(width = 12,
                    valueBoxOutput("box_reaction", width = 12))
            )
        ),
        tabItem(tabName = "outcomes",
            fluidRow(
                box(title = "Seriousness and Outcomes", plotlyOutput("outcomes"), width = 7),
                box(title = "Proportion of Outcomes", plotlyOutput("serious_pie"), width = 5)
            ),
            fluidRow(
                box(selectInput("age", "Age Group", choices = c("Neonate" = "N", "Infant" = "I", "Child" = "C", "Adolescent" = "T", "Adult" = "A", "Elderly" = "E")), width = 6),
                valueBoxOutput("box_outcomes", width = 6)
            )
        ),
        tabItem(tabName = "population",
            fluidRow(
                box(title = "Population Context", plotOutput("population"), width = 7),
                box(title = "Proportion of Reporters", plotlyOutput("reporter_pie"), width = 5)
            ),
            fluidRow( 
                box(selectInput("report_type", "Reporter Type", choices = c("Physician" = "MD", "Pharmacist" = "PH", "Lawyer" = "LW", "Consumer" = "CN", "Unknown" = "NA")), width = 6),
                box(selectizeInput("country", "Country of Occurence", choices = NULL), width = 6)
            )
        )
    )
)


ui <- dashboardPage(
    title = "FAERS Dashboard",
    dashboardHeader(
        title = tags$a(
            tags$img(src = "FDA_logo.png", height = "40px", style = "margin-right: 8px; vertical-align: middle;"),
            "FAERS Dashboard",
            style = "color: white; text-decoration: none;"
        ),
        titleWidth = 280
    ),
    sidebar,
    body
)

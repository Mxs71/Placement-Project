data_dir <- if (dir.exists("../data_clean")) "../data_clean" else "data_clean"

fst_file <- file.path(data_dir, "demo_drug_reac.fst")
gz_file  <- file.path(data_dir, "demo_drug_reac.fst.gz")
if (!file.exists(fst_file) && file.exists(gz_file)) {
  R.utils::gunzip(gz_file, destname = fst_file, remove = FALSE)
}

# Drug time series: use drug-level table (not inflated by per-reaction rows)
demo_drug_slim <- read_fst(file.path(data_dir, "demo_drug_ther.fst"),
    columns = c("normalized", "fda_dt"),
    as.data.table = TRUE)
demo_drug_slim <- demo_drug_slim %>%
    select(order(colnames(demo_drug_slim)))

# Reaction butterfly: only needs drug name + reaction term
demo_reac_slim <- read_fst(file.path(data_dir, "demo_drug_reac.fst"),
    columns = c("normalized", "pt"),
    as.data.table = TRUE)
demo_reac_slim <- demo_reac_slim %>%
    select(order(colnames(demo_reac_slim)))

# Outcomes + population demographics: case-level table
demo_outc <- read_fst(file.path(data_dir, "demo_outc.fst"),
    columns = c("primaryid", "age_grp", "fda_dt", "outc_cod", "occp_cod", "reporter_country", "country_name", "sex"),
    as.data.table = TRUE)
demo_outc <- demo_outc %>% mutate(age_grp = factor(age_grp, levels = c("N", "I", "C", "T", "A", "E")))

available_quarters <- demo_drug_slim %>%
  filter(!is.na(fda_dt), nchar(as.character(fda_dt)) >= 6) %>%
  mutate(
    year = as.integer(substr(as.character(fda_dt), 1, 4)),
    month = as.integer(substr(as.character(fda_dt), 5, 6)),
    quarter = paste0(year, " Q", ceiling(month / 3))
  ) %>%
  distinct(quarter) %>%
  arrange(quarter) %>%
  pull(quarter)

gc()

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

outc_colours <- c(
    DE = okabe_ito[8],
    LT = okabe_ito[6],
    HO = okabe_ito[1],
    DS = okabe_ito[7],
    CA = okabe_ito[5],
    RI = okabe_ito[2],
    OT = okabe_ito[3]
)

server <- function(input, output, session) {
   filtered_drug <- reactive ({
    req(input$selected_drugs, input$time_agg)
    demo_drug_slim %>%
        filter(normalized %in% input$selected_drugs) %>%
        filter(!is.na(fda_dt), nchar(as.character(fda_dt)) >= 6) %>%
        mutate(
            year = as.integer(substr(as.character(fda_dt), 1, 4)),
            month = as.integer(substr(as.character(fda_dt), 5, 6)),
            quarter_label = paste0(year, " Q", ceiling(month / 3))
        ) %>%
        filter(
            quarter_label >= input$year_from,
            quarter_label <= input$year_to
        ) %>%
        mutate(period = floor_date(as.Date(as.character(fda_dt), "%Y%m%d"), input$time_agg)) %>%
        group_by(period, normalized) %>%
        summarise(n = sum(n), .groups = "drop")
    })
    filtered_reac <- reactive({
        req(input$selected_drugs_reac, length(input$selected_drugs_reac) == 2)
        demo_reac_slim %>%
            filter(normalized %in% input$selected_drugs_reac) %>%
            group_by(pt) %>%
            mutate(total = sum(n)) %>%
            ungroup() %>%
            slice_max(total, n = 20, with_ties = FALSE) %>%
            mutate(n_plot = if_else(normalized == input$selected_drugs_reac[2], -n, n)) %>%
            mutate(pt = fct_reorder(pt, total))
    })
    filtered_outc <- reactive({
        req(input$age)
        demo_outc %>%
            filter(age_grp == input$age) %>%
            mutate(period_outc = floor_date(as.Date(as.character(fda_dt), "%Y%m%d"), "quarter")) %>%
            count(period_outc, outc_cod)
    })
    filtered_serious <- reactive({
        req(input$age)
        demo_outc %>%
            filter(age_grp == input$age) %>%
            group_by(primaryid) %>%
            summarise(serious = any(!is.na(outc_cod)), .groups = "drop") %>%
            count(serious) %>%
            mutate(label = if_else(serious, "Serious", "No Serious Outcome Recorded"))
    })
    filtered_pop <- reactive({
        req(input$report_type, input$country)
        demo_outc %>%
            filter(occp_cod == input$report_type) %>%
            filter(country_name == input$country) %>%
            count(age_grp, sex, occp_cod)
    })
    kpi_total <- reactive({
        req(filtered_drug())
        filtered_drug() %>%
            summarise(total = sum(n)) %>%
            pull(total)
    })
    kpi_change <- reactive({
        req(filtered_drug())
        df <- filtered_drug() %>% arrange(period)
        n <- nrow(df)
        if (n < 2) return(NA_real_)
        mid <- floor(n / 2)
        prior <- sum(df$n[1:mid])
        curr <- sum(df$n[(mid+1):n])
        round((curr - prior) / prior * 100, 1)
    })
    kpi_reaction <- reactive({
        req(input$selected_drugs_reac)
        demo_reac_slim %>%
            filter(normalized %in% input$selected_drugs_reac) %>%
            group_by(pt) %>%
            summarise(n = sum(n), .groups = "drop") %>%
            arrange(desc(n)) %>%
            slice(1)
    })
    kpi_outcomes <- reactive({
    req(input$age)
    demo_outc %>%
        filter(age_grp == input$age) %>%
        count(outc_cod, sort = TRUE) %>%
        slice(1) %>%
        mutate(label = condition_labels[outc_cod])
    })
    kpi_reporter <- reactive({
        req(input$country)
        demo_outc %>%
            filter(country_name == input$country) %>%
            count(occp_cod, sort = TRUE) %>%
            mutate(label = c(MD = "Physician", PH = "Pharmacist", OT = "Other HCP", LW = "Lawyer", CN = "Consumer", "NA" = "Unknown")[occp_cod])
    })
    updateSelectizeInput(session, "selected_drugs",
        choices = sort(unique(demo_drug_slim$normalized)),
        server = TRUE)
    updateSelectizeInput(session, "selected_drugs_reac",
        choices = sort(unique(demo_reac_slim$normalized)),
        server = TRUE)
    updateSelectizeInput(session, "country",
        choices = unique(na.omit(demo_outc$country_name)),
        server = TRUE)
    output$reports <- renderPlotly({
        req(filtered_drug())
        shiny::validate(
            need(length(input$selected_drugs) >= 1, "Please select at least one drug"),
            need(length(input$selected_drugs) <= 2, "Please select a maximum of 2 drugs")
        )
        rep <- filtered_drug() %>%
            ggplot(aes(period, n, colour = normalized, group = normalized,
                text = paste0("Reports: ", n))) +
            geom_line() +
            geom_point() +
            labs(x = "Date", y = "Number of Reports", colour = "Drug") +
            theme_prism() +
            scale_colour_manual(values = okabe_ito, labels = \(x) str_trunc(x, 30))
        if (input$rolling) {
            rep <- rep + geom_smooth(se = FALSE, linetype = "dashed")
        }
        ggplotly(rep, tooltip = "text") %>%
            config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                              "zoomIn2d", "zoomOut2d", "autoScale2d",
                                              "resetScale2d", "hoverClosestCartesian",
                                              "hoverCompareCartesian", "toggleSpikelines"))
    })
    output$box_total <- renderValueBox({
        valueBox(
            value = scales::comma(kpi_total()),
            subtitle = "Total Reports for Selected Drugs",
            icon = icon("pills"),
            color = "light-blue"
        )
    })
    output$box_change <- renderValueBox({
        chg <- kpi_change()
        if (is.na(chg)) {
            valueBox("-", "vs prior period (insufficient data)", icon("minus"), color = "black")
        } else {
            dir <- if (chg > 0) "up" else "down"
            color <- if (chg > 0) "yellow" else "aqua"
            valueBox(
                value = paste0(if(chg > 0) "↑ " else "↓ ", abs(chg), "%"),
                subtitle = "Change vs prior period",
                icon = icon(paste0("arrow-", dir)),
                color = color
            )
        }
    })
    output$reactions <- renderPlotly({
        req(filtered_reac())
        shiny::validate(
            need(length(input$selected_drugs_reac) >= 1, "Please select at least one drug"),
            need(length(input$selected_drugs_reac) <= 2, "Please select a maximum of 2 drugs")
        )
        drugs <- input$selected_drugs_reac
        drugs_label <- str_trunc(drugs, 30)
        limit <- max(abs(filtered_reac()$n_plot))
        reac <- filtered_reac() %>%
            ggplot(aes(n_plot, pt, fill = normalized,
                text = paste0("Reports: ", n))) +
            geom_col(width = 0.7) +
            scale_x_continuous(
                limits = c(-limit, limit),
                breaks = scales::breaks_pretty(n = 6),
                labels = function(x) scales::comma(abs(x))
            ) +
            geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey40") +
            labs(x = "Number of reports", y = NULL, fill = "Drug") +
            theme_prism() +
            scale_fill_manual(values = okabe_ito, labels = \(x) str_trunc(x, 30))
        p <- ggplotly(reac, tooltip = "text")
        p$x$data <- lapply(p$x$data, function(trace) {
            trace$name <- gsub("^\\((.+), \\d+\\)$", "\\1", trace$name)
            trace$legendgroup <- gsub("^\\((.+), \\d+\\)$", "\\1", trace$legendgroup)
            trace
        })
        p %>%
            config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                              "zoomIn2d", "zoomOut2d", "autoScale2d",
                                              "resetScale2d", "hoverClosestCartesian",
                                              "hoverCompareCartesian", "toggleSpikelines"))
    })
    output$box_reaction <- renderValueBox({
        top <- kpi_reaction()
        valueBox(
            value = top$pt,
            subtitle = paste0("Most reported reaction: ", scales::comma(top$n), " reports"),
            icon = icon("exclamation-triangle"),
            color = "purple"
        )
    })
    age_labels <- c(N = "Neonate", I = "Infant", C = "Child",
                    T = "Adolescent", A = "Adult", E = "Elderly")
    condition_labels <- c(DE = "Death", LT = "Life-Threatening", HO = "Hospitalisation", 
                          DS = "Disability", CA = "Congenital Anomaly", RI = "Required Intervention", 
                          OT = "Other Serious")
    output$outcomes <- renderPlotly({
        req(filtered_outc())
        outc_data <- filtered_outc() %>%
            group_by(period_outc) %>%
            mutate(pct = round(n / sum(n) * 100, 1)) %>%
            ungroup() %>%
            mutate(outcome = coalesce(condition_labels[as.character(outc_cod)], "Unknown"))
        outc_fill <- setNames(c(outc_colours, okabe_ito[4]), c(condition_labels, "Unknown"))
        outc <- outc_data %>%
            ggplot(aes(period_outc, n, fill = outcome, text = paste0("Percentage: ", pct, "%"))) +
            geom_bar(position = "fill", stat = "identity") +
            scale_y_continuous(labels = scales::percent) +
            labs(x = "Time", y = "Percentage of Reports", fill = "Outcome",
                 subtitle = paste("Age Group:", age_labels[input$age])) +
            theme_prism() +
            scale_fill_manual(values = outc_fill)
        ggplotly(outc, tooltip = "text") %>%
            config(modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                              "zoomIn2d", "zoomOut2d", "autoScale2d",
                                              "resetScale2d", "hoverClosestCartesian",
                                              "hoverCompareCartesian", "toggleSpikelines"))
    })
    output$serious_pie <- renderPlotly({
        plot_ly(
            filtered_serious(),
            labels = ~label,
            values = ~n,
            type = "pie",
            textinfo = "label+percent",
            hovertemplate = "%{label}: %{value:,}<extra></extra>",
            showlegend = FALSE,
            marker = list(colors = okabe_ito)
        )
    })
    output$box_outcomes <- renderValueBox({
        top <- kpi_outcomes()
        valueBox(
            value = top$label,
            subtitle = paste0("Most common outcome: ", scales::comma(top$n), " reports"),
            icon = icon("hospital"),
            color = "teal"
        )
    })
    output$population <- renderPlot ({
        req(filtered_pop())
        pop <- filtered_pop() %>%
            ggplot(aes(age_grp, n, fill = sex)) +
            geom_bar(position = "dodge", stat = "identity") +
            scale_x_discrete(
                limits = c("N", "I", "C", "T", "A", "E"),
                labels = age_labels
            ) +
            labs(x = "Age Group", y = "Number of Reports", fill = "Reporter Type") +
            theme_prism() +
            scale_fill_manual(values = okabe_ito)
        pop
    })
    output$reporter_pie <- renderPlotly({
        plot_ly(
            kpi_reporter(),
            labels = ~label,
            values = ~n,
            type = "pie",
            textInfo = "label+percent",
            hovertemplate = "%{label}: %{value:,}<extra></extra>",
            showlegend = FALSE,
            marker = list(colors = okabe_ito)
        )
    })
}
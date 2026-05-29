demo_drug <- read_fst("../data_clean/demo_drug.fst", as.data.table = TRUE)
if (!file.exists("../data_clean/demo_drug_reac.fst")) {
  R.utils::gunzip("../data_clean/demo_drug_reac.fst.gz", destname = "../data_clean/demo_drug_reac.fst", remove = FALSE)
}
demo_drug_reac <- read_fst("../data_clean/demo_drug_reac.fst", as.data.table = TRUE)
demo_outc <- read_fst("../data_clean/demo_outc.fst", as.data.table = TRUE)

server <- function(input, output, session) {
   filtered_drug <- reactive ({
    req(input$selected_drugs, input$time_agg)
    demo_drug %>%
        filter(normalized %in% input$selected_drugs) %>%
        mutate(period = floor_date(as.Date(as.character(fda_dt), "%Y%m%d"), input$time_agg)) %>%
        count(period, normalized)
    })
    filtered_reac <- reactive ({
        req(input$selected_drugs_reac, length(input$selected_drugs_reac) == 2)
        demo_drug_reac %>%
            filter(normalized %in% input$selected_drugs_reac) %>%
            count(normalized, pt) %>%
            group_by(pt) %>%
            mutate(total = sum(n)) %>%
            ungroup() %>%
            slice_max(total, n = 20, with_ties = FALSE) %>%
            mutate(n_plot = if_else(normalized == input$selected_drugs_reac[2], -n, n)) %>%
            mutate(pt = fct_reorder(pt, total))
    })
    filtered_outc <- reactive ({
        req(input$age)
        demo_outc %>%
            filter(age_grp == input$age) %>%
            mutate(period_outc = floor_date(as.Date(as.character(fda_dt), "%Y%m%d"), "quarter")) %>%
            count(period_outc, outc_cod)
    })
    updateSelectizeInput(session, "selected_drugs",
        choices = demo_drug$normalized,
        server = TRUE)
    updateSelectizeInput(session, "selected_drugs_reac",
        choices = demo_drug_reac$normalized,
        server = TRUE)
    output$reports <- renderPlot({
        req(filtered_drug())
        shiny::validate(
            need(length(input$selected_drugs) >= 1, "Please select at least one drug"),
            need(length(input$selected_drugs) <= 2, "Please select a maximum of 2 drugs")
        )
        rep <- filtered_drug() %>%
            ggplot(aes(period, n, colour = normalized)) +
            geom_line() +
            geom_point() +
            labs(x = "Date", y = "Number of Reports", colour = "Drug") +
            theme_prism()
        if (input$rolling) {
            rep <- rep + geom_smooth(se = FALSE, linetype = "dashed")
        }
        rep
    })
    output$reactions <- renderPlot({
        req(filtered_reac())
        shiny::validate(
            need(length(input$selected_drugs_reac) >= 1, "Please select at least one drug"),
            need(length(input$selected_drugs_reac) <= 2, "Please select a maximum of 2 drugs")
        )
        drugs <- input$selected_drugs_reac
        limit <- max(abs(filtered_reac()$n_plot))
        reac <- filtered_reac() %>%
            ggplot(aes(n_plot, pt, fill = normalized)) +
            geom_col(width = 0.7) +
            scale_x_continuous(
                limits = c(-limit, limit),
                breaks = scales::breaks_pretty(n = 6),
                labels = function(x) scales::comma(abs(x))
            ) +
            geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey40") +
            annotate("text", x = -limit * 0.98, y = Inf,
                label = drugs[2], hjust = 0, vjust = 1.5,
                size = 3.5, colour = "#D85A30", fontface = "bold") +
            annotate("text", x =  limit * 0.98, y = Inf,
                label = drugs[1], hjust = 1, vjust = 1.5,
                size = 3.5, colour = "#378ADD", fontface = "bold") +
            scale_fill_manual(values = c(
                setNames("#378ADD", drugs[1]),
                setNames("#D85A30", drugs[2])
            )) +
            labs(x = "Number of reports", y = NULL) +
            theme_prism()
        reac
    })
    output$outcomes <- renderPlot({
        req(filtered_outc())
        outc <- filtered_outc() %>%
            ggplot(aes(period_outc, n, fill = outc_cod)) +
            geom_bar(position = "fill", stat = "identity") +
            theme_prism()
        outc
    })
}
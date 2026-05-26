server <- function(input, output, session) {
   filtered_drug <- reactive ({
    req(input$selected_drugs, input$time_agg, input$rolling)
    demo_drug %>%
        filter(normalized %in% input$selected_drugs) %>%
        mutate(period = floor_date(as.Date(fda_dt, "%Y%m%d"), input$time_agg)) %>%
        count(period, normalized)
    })
    output$reports <- renderPlot({
        rep <- filtered_drug() %>%
            ggplot(aes(period, n, colour = normalized)) +
            geom_line(stat = "identity") +
            geom_point(stat = "identity") +
            labs(x = "Date", y = "Number of Reports", colour = "Drug")
        if (input$rolling) {
            rep <- rep + geom_smooth(se = FALSE, linetype = "dashed")
        }
        rep
    })
}
library(dplyr)

shinyServer(function(input, output, session) {

    loadedData <- reactive({

        req(input$file_load)

        readr::read_csv(input$file_load$datapath) %>%
            convert_price_cols() %>%
            filter(price <= 1000) %>%
            find_located_flat(distance_center)


    })
    output$server_ui <- renderUI({

        req(loadedData())

        max_price <- max(loadedData()$price)
        div(
            sliderInput(
                "price_range",
                "Select a flat price:",
                min = 0, max = max_price, value = c(0,max_price)
            ),
            selectInput(
                "room_type",
                "Select room type:",
                choices = unique(loadedData()$room_type)
            )
        )
    })

    filteredData <- reactive({

        loadedData() %>%
            find_priced_flat(rooms = input$room_type, price = input$price_range)
    })
    output$hex_plot <- renderPlot({
        ggplot2::ggplot(filteredData(), ggplot2::aes(distance_center)) +
            ggplot2::geom_histogram() +
            ggplot2::ylab("Number of flats") +
            ggplot2::xlab("Distance from the city center")
    })
})

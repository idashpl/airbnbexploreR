library(shiny)

# First download this file (run in a console):
# download_airbnb_data(dir = "../airbnb_data/", city = "Berlin", data_to_load = "listings", format = "summary")

options(shiny.maxRequestSize = 30*1024^2)

fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput(
                inputId = "file_load",
                label = "Load AirBnB listings dataset:",
                accept = "listings.csv"
            ),
            uiOutput("server_ui")
        ),
        mainPanel(
            plotOutput("hex_plot")
        )
    )

)

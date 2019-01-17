#' Show airbnb Shiny app
#'
#' Lore ipsum
#' @export
#'

airbnbApp <- function() {

    shiny::shinyAppDir(system.file('app', package='airbnbexploreR', mustWork=TRUE))
}


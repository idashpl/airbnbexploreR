#' Find reviewed flat
#'
#' @description Modify a \code{listings} data.frame by defining
#' average bayesian rating for each flat.
#' @export
#' @inheritParams find_your_place
#' @return A data.frame with bayesian rating column.
#' @examples
#' \dontrun{
#' listings %>%
#'     find_reviewed_flat(bayes_review, certain = 30, prior = 95) %>%
#'     ggplot(aes(bayes_review)) + geom_histogram() +
#'     scale_x_continuous(breaks = seq(85, 100, 5))
#' }

find_reviewed_flat <- function(data, name, certain = 30, prior = 90) {

    expr_name <- rlang::enquo(name)

    key_cols <- c("number_of_reviews", "review_scores_rating")
    if (columns_exist(data, key_cols))
        stop("The data does not have `number_of_reviews` and `review_scores_rating` columns. Try again.", call. = F)

    df <- data %>%
        mutate(!!expr_name :=
                   calc_bayesian_mean(certain, prior, number_of_reviews, review_scores_rating)
        ) %>%
        arrange(-!!expr_name)

    return(df)
}

#' Calculate bayesian rating
#' @description Calculate bayesian rating based on \url{https://fulmicoton.com/posts/bayesian_rating/}.
#' @keywords internal
#' @inheritParams find_your_place
#' @param num_review A list with number of reviews for each flat.
#' @param rating A list with rating for each flat.
#' @return A list with average bayesian rating

calc_bayesian_mean <- function(certain, prior, num_reviews, rating) {

    (certain * prior + (num_reviews * rating)) / (certain + num_reviews)

}


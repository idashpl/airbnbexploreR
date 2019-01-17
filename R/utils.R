
columns_exist <- function(data, columns) {
    df_columns <- colnames(data)
    return(!all(columns %in% df_columns))
}

is_defined <- function(x) {
    !is.null(x)
}

defined_filter <- function(data, col, condition) {
    expr_cond <- rlang::enquo(condition)

    if (is_defined(col)) filter(data, !!expr_cond) else data
}

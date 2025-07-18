#' Compute descriptive statistics for a variable in the metabolic data set
#'
#' @param dataset the dataset to analyze
#' @param variable character scalar giving the variable name to summarize
#' @param group character scalar giving an optional grouping variable for the
#'   summary
#'
#' @return a data frame of formatted summary statistics
#'
#' @export
#' @examples
#' data(ex_data, package = "PAutilities")
#' ex_data$group_var <- rep(
#'  c("One", "Two", "Three"),
#'  each = ceiling(nrow(ex_data)/3)
#' )[seq(nrow(ex_data))]
#' descriptives(ex_data, "Axis1", "group_var")
#'
descriptives <- function(dataset, variable, group = NULL) {
  # dataset <- total_metabolic
  # variable <- "Age"
  # group <- "age_group"

  if (!is.null(group)) {
    dataset %<>% dplyr::group_by(!! as.name(group))
  }

  data.frame(
    dataset %>%
      dplyr::summarise(
      min := min(!! as.name(variable), na.rm = TRUE),
      !!as.name("q1") := quantile(!! as.name(variable), probs = .25, na.rm = TRUE),
      median := median(!! as.name(variable), na.rm = TRUE),
      mean := mean(!! as.name(variable), na.rm = TRUE),
      !!as.name("q3") := quantile(!! as.name(variable), probs = .75, na.rm = TRUE),
      max := max(!! as.name(variable), na.rm = TRUE),
      sd := sd(!! as.name(variable), na.rm = TRUE),
      n = n(),
      !!as.name("NAs") := sum(is.na(!! as.name(variable)))
    )
  )
}

#' Compute the mean and standard deviation of a vector, returning a formatted
#' string containing the values as `M +/- SD`
#'
#' @param x numeric vector of values to summarize
#' @param MoreArgs named list of arguments to pass to \code{mean} and \code{sd}
#' @param give_df logical. Should mean, sd, and summary string be returned in a
#'   data frame?
#' @param ... additional arguments passed to \code{format}
#' @param mean_x an already-calculated mean value for \code{x}
#' @param sd_x an already-calculated sd value for \code{x}
#'
#' @return either a formatted character scalar (if \code{give_df == FALSE}), or
#'   else a data frame containing columns for the mean value, standard
#'   deviation, and formatted character string combining the two.
#'
#' @export
#' @examples
#' mean_sd(rnorm(100, 50))
#'
mean_sd <- function(
  x = NULL, MoreArgs = NULL, give_df = TRUE, ...,
  mean_x = NULL, sd_x = NULL
) {
  UseMethod("mean_sd", x)
}

#' @rdname mean_sd
#' @export
mean_sd.default <- function(
  x = NULL, MoreArgs = NULL, give_df = TRUE, ...,
  mean_x = NULL, sd_x = NULL
) {

  if (!is.null(x)) {
    mean_x <- do.call(mean, c(x = list(x), MoreArgs))
    sd_x <- do.call(sd, c(x = list(x), MoreArgs))
  }

  sum_string <- paste(
    format(mean_x, ...),
    format(sd_x, ...),
    sep = " \u00B1 "
  )

  if (!give_df) return(sum_string)

  data.frame(
    mean = mean_x,
    sd = sd_x,
    sum_string = sum_string,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

}

#' @rdname mean_sd
#' @export
mean_sd.data.frame <- function(
  x = NULL, MoreArgs = NULL, give_df = TRUE, ...,
  mean_x = NULL, sd_x = NULL
) {

  stopifnot(sapply(x, is.numeric))

  result <- sapply(
    x, mean_sd.default, MoreArgs, give_df,
    ...,  mean_x = mean_x, sd_x = sd_x,
    simplify = FALSE
  )

  if (give_df){
    do.call(rbind, result) %>%
    {within(., {variable = row.names(.)})} %>%
    df_reorder("variable", "mean") %>%
    df_reorder("mean", "variable") %>%
    structure(row.names = seq(nrow(.)))
  } else {
    c(result, use.names = FALSE) %>%
    do.call(c, .)
  }

}

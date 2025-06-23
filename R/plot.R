#' Plot White-winged Dove survey data
#'
#' Create a box plot of non-zero White-winged Dove days hunted, retrieved, or unretrieved.
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom stringr str_to_title
#' @importFrom stringr str_to_sentence
#' @importFrom stringr str_replace
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 coord_cartesian
#' @importFrom rlang .data
#' @importFrom rlang sym
#' 
#' @param season_df Season data tibble
#' @param variable Variable to plot; should be one of "days_hunted", "retrieved" or "unretrieved"
#' @param y_limits If "auto", \code{ggplot2::coord_cartesian} \code{ylim} supplied as \code{c(0, 50)} for days_hunted and retrieved, \code{c(0, 20)} for unretrieved; otherwise, provide custom limits. 
#' 
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}
#' 
#' @export

boxPlotWWDO <-
  function (season_df, variable, y_limits = "auto") {
    
    stopifnot(
      "`variable` should be 'days_hunted', 'retrieved', or 'unretrieved'." = 
        variable %in% c("days_hunted", "retrieved", "unretrieved"))
    
    if (y_limits == "auto") {
      if (variable == "days_hunted") {
        y_limits <- c(0, 50)
      } else if (variable == "retrieved") {
        y_limits <- c(0, 50)
      } else if (variable == "unretrieved") {
        y_limits <- c(0, 20)
      }
    } 
    
    season_df |>
      left_join(
        REF_STATES_WWDO_DF |> select(-"state"), by = "sampled_state") |>
      filter(.data$sp_group_estimated == "White-Winged Dove" & 
               !!sym(variable) != 0) |>
      ggplot(aes(x = .data$sampled_state, y = !!sym(variable))) +
      geom_boxplot(aes(color = .data$wwdo_state_status)) +
      labs(
        title = 
          paste("WWDO", str_to_title(stringr::str_replace(variable, "_", " ")), 
                "(non-zero)"),
        x = "State",
        y = str_to_sentence(stringr::str_replace(variable, "_", " ")),
        color = "State category",
        caption = paste("Note: ylim =", max(y_limits))
      ) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      coord_cartesian(ylim = y_limits)
    
  }

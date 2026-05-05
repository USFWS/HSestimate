#' Estimate harvest
#'
#' Estimate harvest, given a species group.
#'
#' @param totals_df Data tibble
#' @param spp_counts Count data tibble
#' @param type Type of calculation to run, may be one of: 'Ducks', 'Geese',
#'   'Brant', 'SeaDucks', 'MODO', 'WWDO', 'SACR', 'Woodcock', 'Snipe', 'Coots',
#'   'Rails', 'Gallinules', 'BTPI'.
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#'
#' @export

estimate <-
  function(totals_df, spp_counts, type) {

    # Get stats for all licensed hunters
    licenseSumMeanVar <- calcSumMeanVar(totals_df, type)

    # Get number of hunters by state and stratum
    licenseFreq <- calcFreq(totals_df, type)

    # Combine all stratum-level hunter stats
    all_stats <- calcAllStats(licenseSumMeanVar, licenseFreq, spp_counts, type)

    # Calculate variances
    strat_est <- calcVar(all_stats, type)

    # Sum the weighted stratum means
    est_tot <- summarizeByState(strat_est, type)

    return(est_tot)
  }

#' Calculate sum, mean, and variance
#'
#' The internal \code{calcSumMeanVar} function the calculates sum, mean, and
#' variance of data, given a species group.
#'
#' @importFrom rlang .data
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom stats var
#' @importFrom rlang sym
#'
#' @param totals_df Data tibble
#' @param type Type of calculation to run, may be one of: 'Ducks', 'Geese',
#'   'Brant', 'SeaDucks', 'MODO', 'WWDO', 'SACR', 'Woodcock', 'Snipe', 'Coots',
#'   'Rails', 'Gallinules', 'BTPI'.
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

calcSumMeanVar <-
  function(totals_df, type) {
    failtype(type)

    totals_df |>
      summarize(
        across(
          c(paste0("days_hunted_", type),
            paste0("retrieved_", type),
            paste0("unretrieved_", type),
            paste0("active_", type, "_hunter"),
            paste0("successful_", type, "_hunter")),
          list(
            sum = \(x) sum(x, na.rm = TRUE),
            mean = \(x) mean(x, na.rm = TRUE),
            var = \(x) var(x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"),
        .by = c("state", !!sym(paste0("estimation_stratum_", type)))
        )
  }

#' Calculate frequency
#'
#' The internal \code{calcFreq} function calculates the number of hunters by
#' state and stratum.
#'
#' @importFrom dplyr count
#' @importFrom dplyr rename
#' @importFrom rlang sym
#'
#' @param totals_df Data tibble
#' @param type Type of calculation to run, may be one of: 'Ducks', 'Geese',
#'   'Brant', 'SeaDucks', 'MODO', 'WWDO', 'SACR', 'Woodcock', 'Snipe', 'Coots',
#'   'Rails', 'Gallinules', 'BTPI'.
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

calcFreq <-
  function(totals_df, type) {
    failtype(type)

    totals_df |>
      count(.data$state, !!sym(paste0("estimation_stratum_", type))) |>
      rename(n_hunters = "n")
  }

#' Calculate all stats
#'
#' The internal \code{calcAllStats} function calculates all stats using the
#' products of \code{\link{calcSumMeanVar}} and \code{\link{calcFreq}}.
#'
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @importFrom dplyr arrange
#' @importFrom rlang .data
#' @importFrom rlang sym
#'
#' @param licenseSumMeanVar Product of \code{calcSumMeanVar}
#' @param licenseFreq \code{calcFreq}
#' @param spp_counts Count data tibble
#' @param type Type of calculation to run, may be one of: 'Ducks', 'Geese',
#'   'Brant', 'SeaDucks', 'MODO', 'WWDO', 'SACR', 'Woodcock', 'Snipe', 'Coots',
#'   'Rails', 'Gallinules', 'BTPI'.
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

calcAllStats <-
  function(licenseSumMeanVar, licenseFreq, spp_counts, type) {
    reduce(
      list(
        licenseSumMeanVar,
        licenseFreq,
        spp_counts),
      \(x, y) {
        full_join(
          x,
          y,
          by = c("state", paste0("estimation_stratum_", type)))
      }
    ) |>
      arrange(.data$state, !!sym(paste0("estimation_stratum_", type)))
  }

#' Calculate variance
#'
#' The internal \code{calcVar} function calculates variance and other values
#' using the product of \code{\link{calcAllStats}}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom rlang .data
#'
#' @param alldata_df Product of \code{calcAllStats}
#' @param type Type of calculation to run, may be one of: 'Ducks', 'Geese',
#'   'Brant', 'SeaDucks', 'MODO', 'WWDO', 'SACR', 'Woodcock', 'Snipe', 'Coots',
#'   'Rails', 'Gallinules', 'BTPI'.
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

calcVar <-
  function(alldata_df, type) {
    failtype(type)

    a_mean <- paste0("active_", type, "_hunter_mean")
    s_mean <- paste0("successful_", type, "_hunter_mean")
    u_mean <- paste0("unretrieved_", type, "_mean")

    alldata_df |>
      mutate(
        # fpcf: finite population correction factor
        fpcf = 1 - (.data$n_hunters / .data$stratum_count),
        # q for active hunter sample variance
        q_active = 1 - !!sym(a_mean),
        # q for successful hunter sample variance
        q_successful = 1 - !!sym(s_mean),
        # Day/license hunter variance
        var_lday =
          (!!sym(paste0("days_hunted_", type, "_var")) / .data$n_hunters) * .data$fpcf,
        # Bag/license hunter variance
        var_lbag =
          (!!sym(paste0("retrieved_", type, "_var")) / .data$n_hunters) * .data$fpcf,
        # Down/license hunter variance
        var_ldwn =
          (!!sym(paste0("unretrieved_", type, "_var")) / .data$n_hunters) * .data$fpcf,
        # Variance of proportion of active hunters
        var_prop_active_hunters =
          (!!sym(a_mean) * .data$q_active * .data$fpcf) / (.data$n_hunters - 1),
        # Variance of proportion of successful hunters
        var_prop_successful_hunters =
          (!!sym(s_mean) * .data$q_successful * .data$fpcf) / (.data$n_hunters - 1),
        # Total days by stratum
        t_days_hunted =
          !!sym(paste0("days_hunted_", type, "_mean")) * .data$stratum_count,
        # Total bag by stratum
        t_retrieved =
          !!sym(paste0("retrieved_", type, "_mean")) * .data$stratum_count,
        # Count squared
        count_squared = .data$stratum_count ** 2,
        # Numerator of state day variance
        var_t_days_hunted = .data$count_squared * .data$var_lday,
        # Numerator of state bag variance
        var_t_retrieved = .data$count_squared * .data$var_lbag,
        # Active hunters by stratum
        t_p_active_hunters = .data$stratum_count * !!sym(a_mean),
        # Note: estimates for unretrieved harvest should be expanded by active
        # hunters per stratum
        # Total down by stratum
        t_unretrieved = !!sym(u_mean) * .data$t_p_active_hunters,
        # Successful hunters by stratum
        t_p_successful_hunters = .data$stratum_count * !!sym(s_mean),
        # Numerator of active hunters var by stratum
        var_t_p_active_hunters =
          .data$count_squared * .data$var_prop_active_hunters,
        # Numerator of successful hunters var by stratum
        var_t_p_successful_hunters =
          .data$count_squared * .data$var_prop_successful_hunters,
        # Added to get variance of unretrieved
        # Add variance of downed mean and variance of estimated active hunters
        var_t_unretrieved =
          ((!!sym(u_mean) * !!sym(u_mean) * .data$var_prop_active_hunters) +
             (!!sym(a_mean) * !!sym(a_mean) * .data$var_ldwn)) *
          .data$count_squared
      ) |>
      select(
        c(
          "state",
          "SNo",
          !!sym(paste0("estimation_stratum_", type)),
          "bign",
          "stratum_count",
          "n_hunters",
          !!sym(paste0("active_", type, "_hunter_sum")),
          !!sym(paste0("successful_", type, "_hunter_sum")),
          !!sym(paste0("days_hunted_", type, "_sum")),
          "var_lday",
          !!sym(paste0("retrieved_", type, "_sum")),
          "var_lbag",
          !!sym(paste0("unretrieved_", type, "_sum")),
          "var_ldwn",
          prop_active_hunters = !!sym(a_mean),
          "var_prop_active_hunters",
          prop_successful_hunters = !!sym(s_mean),
          "var_prop_successful_hunters",
          "t_days_hunted",
          "t_retrieved",
          "t_unretrieved",
          "count_squared",
          "var_t_days_hunted",
          "var_t_retrieved",
          "var_t_unretrieved",
          "t_p_active_hunters",
          "t_p_successful_hunters",
          "var_t_p_active_hunters",
          "var_t_p_successful_hunters"
        )
      )
  }

#' Summarize estimates by state
#'
#' The internal \code{summarizeByState} function summarizes harvest estimates by
#' state using the product of \code{\link{calcVar}}.
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr rename
#'
#' @param strat_est_df Stratum estimates tibble, the product of \code{calcVar}
#' @param type Type of calculation to run, may be one of: 'Ducks', 'Geese',
#'   'Brant', 'SeaDucks', 'MODO', 'WWDO', 'SACR', 'Woodcock', 'Snipe', 'Coots',
#'   'Rails', 'Gallinules', 'BTPI'.
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

summarizeByState <-
  function(strat_est_df, type) {
    failtype(type)

    strat_est_df |>
      summarize(
        across(
          c(
            "stratum_count",
            paste0("active_", type, "_hunter_sum"),
            paste0("successful_", type, "_hunter_sum"),
            "t_days_hunted",
            "t_retrieved",
            "t_unretrieved",
            "var_t_days_hunted",
            "var_t_retrieved",
            "var_t_unretrieved",
            "t_p_active_hunters",
            "t_p_successful_hunters",
            "var_t_p_active_hunters",
            "var_t_p_successful_hunters"
          ),
          \(x) sum(x, na.rm = TRUE)),
        .by = "state") |>
      rename(BigN = "stratum_count")
  }

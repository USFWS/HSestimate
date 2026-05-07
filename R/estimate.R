#' Estimate harvest for a species group
#'
#' Estimate species group harvest; sum the weighted stratum means.
#'
#' @importFrom dplyr rename
#' @importFrom rlang sym
#' @importFrom rlang :=
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

speciesEstimate <-
  function(totals_df, spp_counts, type) {

    if (type %in% c("Snipe", "Coots")) {
      spp_counts <-
        spp_counts |>
        rename(
          !!sym(paste0("estimation_stratum_", type)) := "estimation_stratum_SC")

    } else if (type %in% c("Rails", "Gallinules")) {
      spp_counts <-
        spp_counts |>
        rename(
          !!sym(paste0("estimation_stratum_", type)) := "estimation_stratum_RG")

    }

    # Get stats for all registered/licensed hunters
    licenseSumMeanVar <- calcSumMeanVar(totals_df, type)

    # Get number of hunters by state and stratum
    licenseFreq <- calcFreq(totals_df, type)

    # Combine all stratum-level hunter stats
    all_stats <- calcAllStats(licenseSumMeanVar, licenseFreq, spp_counts, type)

    # Calculate variances
    strat_est <- calcVar(all_stats, type)

    # Sum the weighted stratum means
    est_tot <- stateTotals(strat_est, type)

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

#' Calculate all statistics
#'
#' The internal \code{calcAllStats} function combines all stratum-level hunter
#' statistics using the products of \code{\link{calcSumMeanVar}} and
#' \code{\link{calcFreq}}.
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
#' The internal \code{stateTotals} function summarizes harvest estimates by
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

stateTotals <-
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

#' Assign flyway
#'
#' The internal \code{assignFlyway} function assigns flyway abbreviations and
#' flyway numbers to estimation data. To assign flyways to sea duck or brant
#' estimation data, see \code{\link{assignFlywaySDBR}}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr relocate
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

assignFlyway <-
  function(data) {
    data |> 
      # Define flyway units
      mutate(
        flyway = 
          case_when(
            .data$state %in% REF_STATES_AF ~ "AF",
            .data$state %in% REF_STATES_MF ~ "MF",
            .data$state %in% REF_STATES_CF ~ "CF",
            .data$state %in% REF_STATES_PF ~ "PF",
            .data$state == "AK" ~ "AK",
            TRUE ~ NA_character_),
        flyNo = 
          case_when(
            .data$flyway == "AF" ~ 1,
            .data$flyway == "MF" ~ 2,
            .data$flyway == "CF" ~ 3,
            .data$flyway == "PF" ~ 4,
            .data$flyway == "AK" ~ 5,
            TRUE ~ NA_integer_)) |> 
      relocate(.data$flyway, .before = "state") |> 
      relocate(.data$flyNo, .after = "flyway")
  }

#' Assign flyway to sea duck and brant estimates
#'
#' The internal \code{assignFlywaySDBR} function assigns flyway abbreviations
#' and flyway numbers to sea duck and brant estimation data.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr relocate
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

assignFlywaySDBR <-
  function(data) {
    data |> 
      # Define flyway units
      mutate(
        flyway = 
          case_when(
            .data$state %in% REF_STATES_SD_AF ~ "AF",
            .data$state %in% c("CA", "OR", "WA") ~ "PF",
            .data$state == "AK" ~ "AK",
            TRUE ~ NA_character_),
        flyNo = 
          case_when(
            .data$flyway == "AF" ~ 1,
            .data$flyway == "PF" ~ 4,
            .data$flyway == "AK" ~ 5,
            TRUE ~ NA_integer_)) |> 
      relocate(.data$flyway, .before = "state") |> 
      relocate(.data$flyNo, .after = "flyway")
  }

#' Assign management units
#'
#' The internal \code{assignMgmtUnit} function assigns management unit abbreviations
#' and management unit numbers to estimation data.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr relocate
#' @importFrom rlang .data
#'
#' @param data Estimation data
#' @param management_units Tibble of management units
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

assignMgmtUnit <-
  function(data, management_units) {
    data |> 
      # Define management units
      left_join(management_units, by = "state") |> 
      mutate(
        mu_no = 
          case_when(
            .data$mu_abbr == "EMU" ~ 1, 
            .data$mu_abbr == "CMU" ~ 2, 
            .data$mu_abbr == "WMU" ~ 3,
            TRUE ~ NA_integer_)) |> 
      relocate(.data$mu_abbr, .before = "state") |> 
      relocate(.data$mu_no, .before = "mu_abbr")
  }

#' Hunter estimates
#'
#' The internal \code{hunterEstimates} function calculates variables for
#' state-level estimates.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

hunterEstimates <-
  function(data) {
    data |>
      mutate(
        # Denominator for variance of state proportions
        bign_squared = 
          .data$BigN^2,
        # State proportion of active hunters
        s_t_p_active_hunters = 
          .data$t_p_active_hunters / .data$BigN,
        # State proportion of successful hunters
        s_t_p_successful_hunters = 
          .data$t_p_successful_hunters / .data$BigN,
        # Variance of state proportion of active hunters
        var_s_t_p_active_hunters = 
          .data$var_t_p_active_hunters / .data$bign_squared,
        # Variance of state proportion of successful hunters
        var_s_t_p_successful_hunters = 
          .data$var_t_p_successful_hunters / .data$bign_squared
      )
  }

#' Calculate harvest estimation fields
#'
#' The internal \code{harvestEstimates} function calculates variables for estimates.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

harvestEstimates <-
  function(data) {
    data |>
      mutate(
        # Bag per active hunter
        retrieved_per_active_hunter = 
          .data$t_retrieved / .data$t_p_active_hunters,
        # Variance for bag per active hunter
        var_retrieved_per_active_hunter =
          (.data$var_t_retrieved +
              (.data$t_retrieved * 
                 .data$t_retrieved * 
                 .data$var_t_p_active_hunters) /
              (.data$t_p_active_hunters * .data$t_p_active_hunters)) /
          (.data$t_p_active_hunters * .data$t_p_active_hunters),
        # Bag per hunter standard error
        se_retrieved_per_active_hunter = 
          sqrt(.data$var_retrieved_per_active_hunter),
        # Bag per hunter 95% confidence interval
        ci_retrieved_per_active_hunter = 
          1.96 * .data$se_retrieved_per_active_hunter,
        # Bag per hunter +/- % 95%
        pct_ci_retrieved_per_active_hunter = 
          100 * (.data$ci_retrieved_per_active_hunter / 
                   .data$retrieved_per_active_hunter),
        # Days hunted standard error
        se_t_days_hunted = sqrt(.data$var_t_days_hunted),
        # Days hunted 95% confidence interval
        ci_t_days_hunted  = 1.96 * .data$se_t_days_hunted,
        # Days hunted +/- % 95%
        pct_ci_t_days_hunted = 
          100 * (.data$ci_t_days_hunted / .data$t_days_hunted),
        # Retrieved standard error
        se_t_retrieved = sqrt(.data$var_t_retrieved),
        # Retrieved 95% confidence interval
        ci_t_retrieved = 1.96 * .data$se_t_retrieved,
        # Retrieved +/- % 95%
        pct_ci_t_retrieved = 
          100 * (.data$ci_t_retrieved / .data$t_retrieved),
        # Unretrieved standard error
        se_t_unretrieved = sqrt(.data$var_t_unretrieved),
        # Unretrieved 95% confidence interval
        ci_t_unretrieved = 1.96 * .data$se_t_unretrieved,
        # Unretrieved +/- % 95%
        pct_ci_t_unretrieved = 
          100 * (.data$ci_t_unretrieved / .data$t_unretrieved),
        # Proportion of active hunters standard error
        pact_se = sqrt(.data$var_s_t_p_active_hunters),
        # Proportion of active hunters 95% confidence interval
        ci_pact = 1.96 * .data$pact_se,
        # Proportion of active hunters +/- % 95%
        percCIActHunt = 100 * (.data$ci_pact / .data$s_t_p_active_hunters),
        # Proportion of successful Hunters standard error
        psuc_se = sqrt(.data$var_s_t_p_successful_hunters),
        # Proportion of successful hunters 95% confidence interval
        ci_psuc = 1.96 * .data$psuc_se,
        # Proportion of successful hunters +/- % 95%
        cipers = 100 * (.data$ci_psuc / .data$s_t_p_successful_hunters)
      )
  }

#' Select fields for state level estimates
#'
#' The internal \code{stateSelect} function selects desired state level
#' estimate data fields.
#'
#' @importFrom dplyr select
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

stateSelect <-
  function(data) {
    data |>
      select(
        c(
          "flyway",
          "flyNo",
          "state",
          "BigN",
          "t_days_hunted",
          "var_t_days_hunted",
          "pct_ci_t_days_hunted",
          "t_retrieved",
          "var_t_retrieved",
          "pct_ci_t_retrieved",
          "t_unretrieved",
          "var_t_unretrieved",
          "pct_ci_t_unretrieved",
          "t_p_active_hunters",
          "var_t_p_active_hunters",
          "percCIActHunt",
          "retrieved_per_active_hunter",
          "var_retrieved_per_active_hunter",
          "pct_ci_retrieved_per_active_hunter"
        )
      )
  }

#' Select fields for management unit level estimates
#'
#' The internal \code{mgmtUnitSelect} function selects desired management unit level
#' estimate data fields.
#'
#' @importFrom dplyr select
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

mgmtUnitSelect <-
  function(data) {
    data |>
      select(
        c(
          "mu_abbr",
          "mu_no",
          "state",
          "BigN",
          "t_days_hunted",
          "var_t_days_hunted",
          "pct_ci_t_days_hunted",
          "t_retrieved",
          "var_t_retrieved",
          "pct_ci_t_retrieved",
          "t_unretrieved",
          "var_t_unretrieved",
          "pct_ci_t_unretrieved",
          "t_p_active_hunters",
          "var_t_p_active_hunters",
          "percCIActHunt",
          "retrieved_per_active_hunter",
          "var_retrieved_per_active_hunter",
          "pct_ci_retrieved_per_active_hunter"
        )
      )
  }

#' Flyway estimates
#'
#' The internal \code{flywayTotals} function summarizes estimates to the flyway
#' level.
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

flywayTotals <-
  function(data) {
    data |>
      summarize(
        across(
          c(
            "BigN",
            "t_days_hunted",
            "var_t_days_hunted",
            "t_retrieved",
            "var_t_retrieved",
            "t_unretrieved",
            "var_t_unretrieved",
            "t_p_active_hunters",
            "var_t_p_active_hunters",
            "retrieved_per_active_hunter",
            "var_retrieved_per_active_hunter"
          ), 
          \(x) sum(x, na.rm = T)),
        .by = "flyway")
  }

#' Management unit estimates
#'
#' The internal \code{mgmtUnitTotals} function summarizes estimates to the management
#' unit level.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

mgmtUnitTotals <-
  function(data){
    data |>
      filter(!is.na(.data$mu_abbr)) |> 
      summarize(
        across(
          c(
            "BigN",
            "t_days_hunted",
            "var_t_days_hunted",
            "t_retrieved",
            "var_t_retrieved",
            "t_unretrieved",
            "var_t_unretrieved",
            "t_p_active_hunters",
            "var_t_p_active_hunters",
            "retrieved_per_active_hunter",
            "var_retrieved_per_active_hunter"
          ), 
          \(x) sum(x, na.rm = T)),
        .by = c("mu_abbr", "mu_no"))
  }

#' Final flyway estimates
#'
#' The internal \code{flywayFinal} function produces the final flyway harvest
#' estimates. For the final estimates for sea ducks and brant, see
#' \code{\link{flywayFinalSDBR}}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

flywayFinal <-
  function(data) {
    data |> 
      mutate(
        sday = sqrt(.data$var_t_days_hunted),
        ciDay = 1.96 * .data$sday,
        pct_ci_t_days_hunted = 100 * .data$ciDay / .data$t_days_hunted,
        sbag = sqrt(.data$var_t_retrieved),
        ciBag = 1.96 * .data$sbag,
        flyway_pct_ci_t_retrieved = 100 * .data$ciBag / .data$t_retrieved,
        sdown = sqrt(.data$var_t_unretrieved),
        ciDown = 1.96 * .data$sdown,
        fpct_ci_t_unretrieved = 100 * .data$ciDown / .data$t_unretrieved,
        # Not redundant
        flyNo =
          case_when(
            .data$flyway == "AF" ~ 1,
            .data$flyway == "MF" ~ 2,
            .data$flyway == "CF" ~ 3,
            .data$flyway == "PF" ~ 4,
            .data$flyway == "AK" ~ 5,
            TRUE ~ NA_integer_
          ),
        fstate = .data$flyway
      ) |> 
      select(
        c(
          "flyway",
          "flyNo",
          state = "fstate",
          "BigN",
          "t_days_hunted",
          "var_t_days_hunted",
          "pct_ci_t_days_hunted",
          "t_retrieved",
          "var_t_retrieved",
          pct_ci_t_retrieved = "flyway_pct_ci_t_retrieved",
          "t_unretrieved",
          "var_t_unretrieved",
          pct_ci_t_unretrieved = "fpct_ci_t_unretrieved",
          "t_p_active_hunters"
        )
      ) 
  }

#' Final flyway estimates for sea duck and brant
#'
#' The internal \code{flywayFinalSDBR} function produces the final flyway
#' harvest estimates for sea ducks and brant.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

flywayFinalSDBR <-
  function(data) {
    data |>
      mutate(
        sday = sqrt(.data$var_t_days_hunted),
        ciDay = 1.96 * .data$sday,
        fpercCIDay = 100 * .data$ciDay / .data$t_days_hunted,
        sbag = sqrt(.data$var_t_retrieved),
        ciBag = 1.96 * .data$sbag,
        flyway_pct_ci_t_retrieved = 100 * .data$ciBag / .data$t_retrieved,
        sdown = sqrt(.data$var_t_unretrieved),
        ciDown = 1.96 * .data$sdown,
        fpct_ci_t_unretrieved = 100 * .data$ciDown / .data$t_unretrieved,
        # Not redundant
        flyNo =
          case_when(
            .data$flyway == "AF" ~ 1,
            .data$flyway == "PF" ~ 4,
            .data$flyway == "AK" ~ 5,
            TRUE ~ NA_integer_
          ),
        fstate = .data$flyway
      ) |>
      select(
        c(
          "flyway",
          "flyNo",
          state = "fstate",
          "BigN",
          "t_days_hunted",
          "var_t_days_hunted",
          pct_ci_t_days_hunted = "fpercCIDay",
          "t_retrieved",
          "var_t_retrieved",
          pct_ci_t_retrieved = "flyway_pct_ci_t_retrieved",
          "t_unretrieved",
          "var_t_unretrieved",
          pct_ci_t_unretrieved = "fpct_ci_t_unretrieved",
          "t_p_active_hunters"
        )
      ) 
  }

#' Final management unit estimates
#'
#' The internal \code{mgmtUnitFinal} function produces the final management unit
#' harvest estimates.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename_with
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

mgmtUnitFinal <-
  function(data) {
    data |> 
      mutate(
        state = .data$mu_abbr,
        sday = sqrt(.data$var_t_days_hunted),
        ciDay = 1.96 * .data$sday,
        pct_ci_t_days_hunted = 100 * .data$ciDay / .data$t_days_hunted,
        sbag = sqrt(.data$var_t_retrieved),
        ciBag = 1.96 * .data$sbag,
        mu_pct_ci_t_retrieved = 100 * .data$ciBag / .data$t_retrieved,
        sdown = sqrt(.data$var_t_unretrieved),
        ciDown = 1.96 * .data$sdown,
        mu_pct_ci_t_unretrieved = 100 * .data$ciDown / .data$t_unretrieved
      ) |> 
      select(
        c(
          "mu_abbr",
          "mu_no",
          "state",
          "BigN",
          "t_days_hunted",
          "var_t_days_hunted",
          "pct_ci_t_days_hunted",
          "t_retrieved",
          "var_t_retrieved",
          "mu_pct_ci_t_retrieved",
          "t_unretrieved",
          "var_t_unretrieved",
          "mu_pct_ci_t_unretrieved",
          "t_p_active_hunters"
        )
      ) |> 
      rename_with(\(x) str_replace(x, "^mu\\_p", "p"))
  }

#' Final United States estimates
#'
#' The internal \code{usFinal} function produces the final national harvest
#' estimates. For the final estimates for doves, see \code{\link{usFinalDV}}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

usFinal <-
  function(data) {
    data |> 
      mutate(flyNo = 6) |>
      group_by(.data$flyNo) |>
      summarize(
        across(
          c(
            "BigN",
            "t_days_hunted",
            "var_t_days_hunted",
            "t_retrieved",
            "var_t_retrieved",
            "t_unretrieved",
            "var_t_unretrieved",
            "t_p_active_hunters"
          ), 
          \(x) sum(x, na.rm = TRUE))
      ) |> 
      mutate(
        ussDay = sqrt(.data$var_t_days_hunted),
        usciDay = 1.96 * .data$ussDay,
        pct_ci_t_days_hunted = 100 * .data$usciDay / .data$t_days_hunted,
        ussBag = sqrt(.data$var_t_retrieved),
        usciBag = 1.96 * .data$ussBag,
        pct_ci_t_retrieved = 100 * .data$usciBag / .data$t_retrieved,
        ussDown = sqrt(.data$var_t_unretrieved),
        usciDown = 1.96 * .data$ussDown,
        pct_ci_t_unretrieved = 100 * .data$usciDown / .data$t_unretrieved,
        flyway = "US",
        state = "US"
      ) |> 
      relocate(.data$flyway, .before = "flyNo") |>
      relocate(.data$state, .after = "flyNo") |>
      relocate(.data$pct_ci_t_days_hunted, .after = "var_t_days_hunted") |>
      relocate(.data$pct_ci_t_retrieved, .after = "var_t_retrieved") |>
      relocate(.data$pct_ci_t_unretrieved, .after = "var_t_unretrieved") |>
      select(
        -c("ussDay", "usciDay", "ussBag", "usciBag", "ussDown", "usciDown"))
  }

#' Final United States estimates for doves
#'
#' The internal \code{ususFinalDVFinal} function produces the final national
#' harvest estimates for Mourning Doves and White-winged Doves.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @param data Estimation data
#'
#' @family estimation functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

usFinalDV <-
  function(data) {
    data |> 
      mutate(mu_abbr = "US") |>
      group_by(.data$mu_abbr) |>
      summarize(
        across(
          c(
            "BigN",
            "t_days_hunted",
            "var_t_days_hunted",
            "t_retrieved",
            "var_t_retrieved",
            "t_unretrieved",
            "var_t_unretrieved",
            "t_p_active_hunters"
          ), 
          \(x) sum(x, na.rm = TRUE))
      ) |> 
      mutate(
        ussDay = sqrt(.data$var_t_days_hunted),
        usciDay = 1.96 * .data$ussDay,
        pct_ci_t_days_hunted = 100 * .data$usciDay / .data$t_days_hunted,
        ussBag = sqrt(.data$var_t_retrieved),
        usciBag = 1.96 * .data$ussBag,
        pct_ci_t_retrieved = 100 * .data$usciBag / .data$t_retrieved,
        ussDown = sqrt(.data$var_t_unretrieved),
        usciDown = 1.96 * .data$ussDown,
        pct_ci_t_unretrieved = 100 * .data$usciDown / .data$t_unretrieved,
        state = "US",
        mu_no = 6
      ) |> 
      relocate(.data$state, .after = "mu_abbr") |> 
      relocate(.data$pct_ci_t_days_hunted, .after = "var_t_days_hunted") |> 
      relocate(.data$pct_ci_t_retrieved, .after = "var_t_retrieved") |> 
      relocate(.data$pct_ci_t_unretrieved, .after = "var_t_unretrieved") |> 
      select(
        -c("ussDay", "usciDay", "ussBag", "usciBag", "ussDown", "usciDown"))
  }


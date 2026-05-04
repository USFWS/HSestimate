# HSestimate (dev version)

## Major changes & new features

-   `Imports`
    -   Add `{purrr}`
-   Refactor checking functions in `R/check.R`
    -   Create more internal helper functions and move most of the internal helpers back in to `R/check.R`; other files like `R/errorIDs.R` and `R/overbag.R` were deleted (see [e6656cf](https://github.com/USFWS/HSestimate/commit/e6656cf2c2560bed2732792ef858c8f70893a0bd)).
    -   Create `surveyCheck()` (previously named `editCheck()`), the exported checking function which runs internal functions depending on species group.
    -   Internal helpers:
        -   `audit()` and `auditDV()` create the list of daily corrected, daily audit, season corrected, and season audit tibbles
        -   `checkWF()` is used for waterfowl instead of a combination of `surveyCheck()` and `audit()`, since waterfowl must have SD and BR harvest converted to DK and GS, respectively, in a special step.
        -   Check daily data
            -   `checkDailyWF()` is used for waterfowl
            -   `checkDailyDV()` is used for doves
            -   `checkDailySCRGWKCR()` is used for snipe, coot, rails, gallinules, woodcock, and cranes
        -   Check season data
            -   `checkSeasonDV()` is used for doves
            -   `checkSeasonCR()` is used for cranes
            -   `checkSeasonWFSCRGWK()` is used for waterfowl, snipe, coot, rails, gallinules, and woodcock
        -   Error checking
            -   Daily
                -   `partyHuntFinder()` assigns `error1` (see [R/party.R](<https://github.com/USFWS/HSestimate/blob/main/R/party.R>))
                -   `dailyOverBagWWMO()` assigns `error2` for doves
                -   `dailyOverBagDKSD()` assigns `error2` for waterfowl
                -   `dailyOverBag()` assigns `error3` for all species (including doves and waterfowl)
            -   Season
                -   `naDaysHunted()` assigns `error1`
                -   `tooManyDaysHunted()` assigns `error2` for all species groups except cranes
                    -   `tooManyDaysHuntedCR()` assigns `error2` for cranes
                -   `seasonOverBag()` assigns `error3` for all species groups except doves
                    -   `seasonOverBagDV()` assigns `error3` for doves
                -   `seasonDNH()` assigns `error4`
    -   Checking season data
        -   Use `checkSeasonWFSCRGWK()` on waterfowl, snipe, coot, rail, gallinule, and woodcock
        -   Use `checkSeasonCR()` on cranes
-   Add estimation functions to `R/estimate.R`, including:
    -   `calcAllStats()`
    -   `calcFreq()`
    -   `calcSumMeanVar()`
    -   `calcVar()`
    -   `estimate()`
    -   `summarizeByState()`
-   Modify internal failure functions in `R/fails.R`:
    -   Delete `fails()`
    -   Create:
        -   `failtype()`
        -   `failStateCount()`
        -   `failNARetrieved()`
        -   `failNADaysHunted()`
-   Update `test-check.R`

## Minor changes / bug fixes

-   Fixed bugs in `convertWWDO()`
    -   `case_when()` not compatible with using `type` in conditional statements, so moved `errors_df` creation.
-   Renamed `variables.R` to `constants.R`
    -   Set `REF_DAY_LIMIT_WK` to `63`.
    -   Reordered and alphabetized vectors.
    -   Added `REF_STATES_NO_COOTS`, `REF_STATES_NO_GALLS`, and `REF_STATES_NO_RAILS`.

# HSestimate 2.0.0

-   Renamed to `HSestimate`
    -   In addition to checking Harvest Survey data, the package will now also generate totals files and calculate harvest estimates.

# migbirdHS 1.0.0

-   Renamed to `migbirdHS`
-   Added a `NEWS.md` file to track changes to the package.
-   Edited `proofHS()` function to show additional informative decision-making columns in output.

# migbirdMBHS 0.3.0

-   New updates for 2022-2023 HS data checks.
-   Overhaul of `add_seaducks()`
-   Removed `correctHS()`
-   Deprecated `read_dhs()`
-   Added internal function `wrangle_ref()` and used it to replace redundant code across multiple exported functions.
-   The `%>%` pipe was replaced with `|>` when possible.
-   Vignette updated.

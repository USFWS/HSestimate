# HSestimate (dev version)

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

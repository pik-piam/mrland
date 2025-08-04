#' @title calcTrademportSupplyStdDev
#'
#' @description Calculates regional imports to supply ratios
#' often termed "Import Dependency Ratio"
#' @return Self import to dupply ratio
#' @author David M Chen
#' @seealso \code{\link{calcOutput}}, \code{\link{calcFAOmassbalance}}
#' @examples
#' \dontrun{
#' a <- calcTrademportSupplyStdDev()
#' }
#' @importFrom dplyr %>% inner_join
#' @importFrom rlang .data :=

calcTradeImportSupplyStdDev <- function() {

  ratio <- calcOutput("TradeImportSupplyRatio",# yearly = FALSE,
                      aggregate = FALSE)

  ratiodf <- as.data.frame(collapseNames(ratio), rev = 2)

  # get s.d. function for standard deviation of data observed within certain amount of years
  .getsd <- function(dataIn, years) {
    meanCol <- paste0("meansd", years)
    maxCol  <- paste0("maxsd", years)
    minCol  <- paste0("minsd", years)

    out <- tibble::as_tibble(dataIn) %>%
      dplyr::arrange(.data$Year) %>%
      dplyr::group_by(.data$ex, .data$im, .data$ItemCodeItem) %>%
      dplyr::mutate(rollsd = zoo::rollapply(.data$.value, years, sd, fill = NA)) %>%
      dplyr::summarise(
        !!meanCol := mean(.data$rollsd, na.rm = TRUE),
        !!maxCol  := max(.data$rollsd, na.rm = TRUE),
        !!minCol  := min(.data$rollsd, na.rm = TRUE)
      )

    return(out)
  }

  ratio5 <- .getsd(ratiodf, 5)

  ratio10 <- .getsd(ratiodf, 10)

  ratio15 <- .getsd(ratiodf, 15)

  ratio510 <- inner_join(ratio5, ratio10)
  ratiosd <- inner_join(ratio510, ratio15)
  ratiosd <- pivot_longer(ratiosd, cols = c(4:12), names_to = "ratio")
  ratiosd <- as.magpie(ratiosd, spatial = c(1, 2), temporal = NULL, tidy = TRUE)

  mb <- calcOutput("FAOmassbalance", aggregate = FALSE)[, , "dm", drop = TRUE][
                                                                               , , "domestic_supply", drop = TRUE]
  cyears <- intersect(getYears(ratiosd), getYears(mb))
  citems <- intersect(getItems(ratiosd, dim = 3), getItems(mb, dim = 3))
  weight <- mb[, cyears, citems][, , "dm"][, , "domestic_supply"]


  return(list(x = ratiosd,
              weight = weight,
              unit = "ratio",
              description = "countries' historical standard deviations observed in import supply ratio."))
}

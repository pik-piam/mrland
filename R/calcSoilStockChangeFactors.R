#' @title calcSoilStockChangeFactors
#' @description calculates and merges information on stock change factors
#'
#' @seealso
#' [readIPCC()]
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilStockChangeFactors")
#' }
#'
#' @return MAgPIE object of yields
#' @author Kristine Karstens

calcSoilStockChangeFactors <- function() {

  scfLu2019 <- readSource("IPCC", subtype = "SCF_LU2019", convert = FALSE)

  kcr <- findset("kcr")
  lu  <- getNames(scfLu2019, dim = 2)
  lu2kcr <- new.magpie(names = as.vector(outer(lu, kcr, paste, sep = ".")),
                       fill  = 0, sets = c("regions", "t", "LU.kcr"))

  annCrop <- "Long_term_cultivated"
  perTree <- "Perennial_Tree_Crop"
  kcrInAnnCrop <- setdiff(kcr, c("betr", "begr", "oilpalm", "others", "rice_pro", "sugr_cane"))
  kcrInPerTree  <- c("betr", "begr", "oilpalm")
  kcrInMix      <- c("others", "rice_pro")
  lu2kcr[, , annCrop][, , kcrInAnnCrop] <- 1
  lu2kcr[, , perTree][, , kcrInPerTree] <- 1

  othersPerShare <- calcOutput("LUH2MAgPIE", aggregate = FALSE,
                               share = "LUHofMAG")[, "y1995", "others"]
  noShare        <- which(round(dimSums(othersPerShare, dim = 3.1)) != 1)
  othersPerShare <- dimSums(othersPerShare[, , c("c3per", "c4per")], dim = 3.1)
  othersPerShare[noShare] <- 0.62 # default assumption 62% perennials in others
                                  # (as 62% of total others production is perennial)
  riceNonFlood      <- calcOutput("Ricearea", share = TRUE, aggregate = FALSE)[, "y1995", ]

  lu2kcrIso      <- magpie_expand(lu2kcr, riceNonFlood)
  lu2kcrIso[, , "others"][, , perTree]        <- othersPerShare
  lu2kcrIso[, , "others"][, , annCrop]        <- 1 - othersPerShare

  # Due to mismatch in LUH data rice share for non flooded is disabled and only paddy rice assumed # nolint
  # lu2kcrIso[, , "rice_pro"][, , annCrop]      <- riceNonFlood                                    # nolint
  # lu2kcrIso[, , "rice_pro"][, , "Paddy_rice"] <- 1 - riceNonFlood                                # nolint
  lu2kcrIso[, , "rice_pro"][, , annCrop]      <- 0
  lu2kcrIso[, , "rice_pro"][, , "Paddy_rice"] <- 1

  cShare <- collapseDim(dimSums(scfLu2019 * lu2kcrIso, dim = 3.2))
  weight <- collapseDim(calcOutput("Croparea", aggregate = FALSE, physical = TRUE)[, "y1995", ])
  weight[, , setdiff(kcr, kcrInMix)] <- 1


  return(
    list(x            = cShare,
         weight       = weight + 1e-10,
         min          = 0,
         unit         = "tC per tC",
         description  = "Stock change factors for first 30 cm of the soil profile"))
}

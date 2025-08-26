#' @title calcFAOTradeKastner
#' @description Implements Kastner et al. 2011 (DOI: 10.1016/j.ecolecon.2011.01.012) on the FAO
#' trade bilateral matrix. Adjusts the trade matrix based on a assumption of proportionality such
#' that intermediate trade partners (importing to re-export) are removed from the matrix.
#'
#' @param datasource FAO by default, no other trade matrix currently available
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' Note that the results returned here is a bilateral matrix of 'net demand'
#' (in this case prod + exports - imports)
#' @author David M Chen
#' @importFrom tidyr pivot_longer
#' @importFrom MASS ginv

calcTradeKastner <- function(datasource = "FAO") {
  # get production from mass balance
  prod <- collapseNames(calcOutput("FAOmassbalance_pre",
                                   aggregate = FALSE)[, , "production.dm"])
  # get bilateral trade matrix
  trade <- calcOutput("FAOBilateralTrade", output = "qty",
                      products = "kcr", harmonize = FALSE, fiveYear = FALSE, aggregate = FALSE)
  tradel  <- calcOutput("FAOBilateralTrade", output = "qty",
                        products = "kli", harmonize = FALSE, fiveYear = FALSE, aggregate = FALSE)
  tradeo <- calcOutput("FAOBilateralTrade", output = "qty",
                       products = "kothers", harmonize = FALSE, fiveYear = FALSE, aggregate = FALSE)
  tmfo <- calcOutput("TimberTradeBilateral", aggregate = FALSE)
  trade <- mbind(trade, tradel)
  trade <- mbind(trade, tradeo)
  rm(tradel, tradeo)
  ## hold the timber bilat constant into past and future, as they just missing a few years
  tmfo <- toolHoldConstant(tmfo,
                           years = utils::tail(getYears(tmfo, as.integer = TRUE), n = 1):
                            utils::tail(getYears(trade, as.integer = TRUE), n = 1))
  tmfo <- toolHoldConstant(tmfo, years = 1995:1996)
  tmfo[, 1995, ] <- tmfo[, 1997, ]
  tmfo[, 1996, ] <- tmfo[, 1997, ]

  trade <- mbind(trade, tmfo)

  mb <- calcOutput("FAOmassbalance_pre", aggregate = FALSE)

  tmi <- dimSums(trade, dim = 1.2)

  mbi <- mb[, , "import"][, , "dm", drop = TRUE]

  cyears <- intersect(getYears(trade), getYears(mb))
  citems <- intersect(getItems(trade, dim = 3), getItems(mb, dim = 3.1))
  missing <- setdiff(findset("k_trade"), citems)
  # distillers grain, ethanol, scp, fish are now still  missing completely from the bilateral trade matrix
  # the first 3 are made up products so don't exist in FAO either. Need to include fish at some point


  # here we look at difference between trade matrix and mass balance and scale the trade matrix to mb

  imDiff <- tmi[, cyears, citems] / mbi[, cyears, citems]

  imDiff[is.na(imDiff)] <- 0

  imDiff[is.infinite(imDiff)] <- 0

  # scale up imports

  # rename exporters so that we scale up  the Wimporters and not exporters
  getItems(trade, dim = 1.2) <- paste0(getItems(trade, dim = 1.2), "2")

  tmS  <- trade[, cyears, citems] / imDiff
  tmS[is.infinite(tmS)] <- 0
  tmS[is.na(tmS)] <- 0
  # for cases where there is no tmi but trade reported in mbi we still want to match mbi
  # so as assumption split the mbi imports across the bilateral patterns of all products
  allPRatio <- dimSums(tmS, dim = 3) / dimSums(tmS, dim = c(1.2, 3))
  allPRatio[is.na(allPRatio)] <- 0

  mbiS <-  mbi[, cyears, citems] * allPRatio[, cyears, ]
  tmS2 <- ifelse(dimSums(tmS[, cyears, citems], dim = 1.2) == 0 & mbiS[, cyears, citems] > 0,
                 mbiS,
                 tmS)

  tmS2i <- dimSums(tmS2, dim = 1.2)
  zz <- tmS2i[, cyears, citems] - mbi[, cyears, citems]

  tmS2 <- round(tmS2, 6)
  trade <- tmS2
  getItems(trade, dim = 1.2) <- gsub("[0-9]+", "", getItems(trade, dim = 1.2))

  # In order to do matrix algebra, simpler for now to
  # subset by product year with a for loop

  citems <- intersect(getItems(prod, dim = 3), getItems(trade, dim = 3))
  cyears <- intersect(getItems(prod, dim = 2), getItems(trade, dim = 2))

  # to re-assign to new object
  kastnerF <- new.magpie(cells_and_regions = getItems(trade, dim = 1),
                         years = cyears,
                         names = citems)


  for (i in citems) {
    for (t in cyears) {

      ### create trade matrix ###
      tradeS <- trade[, t, i]
      tradeM <- t(matrix(tradeS, nrow = length(getItems(tradeS, dim = 1.1))))

      ### create production matrix diagonal #########
      prodS <- prod[, t, i]

      pM <- diag(as.vector(prodS))
      ### calculate total imports and exports
      imM <- dimSums(tradeS, dim = 1.2)
      exM <- dimSums(tradeS, dim = 1.1)

      ### calculate x ( production + imports) as a diagonal vector
      xM <- (dimSums(prodS, dim = 3) + imM)
      xM <- diag(as.vector(xM))

      ### calculate export shares A as matrix multiplication of trade matrix %*% reciprocal diagonal x
      x1 <- 1 / xM
      x1[is.infinite(x1)] <- 0

      matrixA <- tradeM %*% x1

      ## make diagonal production matrix  and multiply by inverse of (IdMatrix - exportShare)
      matrixR <- ginv(diag(nrow(matrixA)) - matrixA) %*% pM

      # get consumption share (prod + imports - exports)/(prod+imports)
      matrixC <- (xM - diag(as.vector(exM))) / xM
      matrixC[is.na(matrixC)] <- 0
      matrixC[is.infinite(matrixC)] <- 0
      # multiply consumption by matrix R to get apparent consumption
      matrixRb <-  t(t(matrixR)  %*% (matrixC))

      rownames(matrixRb) <- getItems(tradeS, dim = 1)
      colnames(matrixRb) <- getItems(tradeS, dim = 1)

      matrixRb <- as.data.frame(matrixRb)
      matrixRb <- cbind(rownames(matrixRb), data.frame(matrixRb, row.names = NULL))
      matrixRb <- pivot_longer(matrixRb, cols = 2:ncol(matrixRb), names_to = "ex", values_to = "value")
      colnames(matrixRb)[1] <- "im"

      matrixRb <- as.magpie(matrixRb, spatial = c(1, 2), temporal = 2010, tidy = TRUE)

      # assign to kastnerF
      kastnerF[, t, i] <- matrixRb

      # double check that matrixRb matches prod
      if (!(round(dimSums(matrixRb, dim = 1)) == round(dimSums(prodS, dim = 1)))) {
        vcat(0, paste0("production doesn't match re-adjusted total trade+production for ", i, t))
      }

    }
  }

  return(kastnerF)
}

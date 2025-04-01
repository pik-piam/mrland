#' @title readREMIND
#' @description Reads in a reporting mif file from REMIND
#'
#' @param subtype A string composed of three items: unit, revision and indicator.
#' Unit can be either "intensive" or "extensive", revision is the input data
#' revision, and indicator is the name of thre REMIND indicator
#' @return MAgPIE object with regional aggregation of REMIND H12
#' @author David Klein
#' @seealso
#' \code{\link{readSource}}
#' @examples
#'
#' \dontrun{
#' readSource("REMIND",aggregate=FALSE)
#' }
#' @importFrom magclass read.report

readREMIND <- function(subtype) {

  .readAndRename <- function(fileList, pattern, replacement, indicator) {
    x <- NULL
    for (f in fileList) {
      # select REMIND only since newer coupled REMIND reportings also contain MAgPIE
      x <- mbind(x, read.report(f,
                                as.list = FALSE,
                                showSeparatorWarning = FALSE)[, , "REMIND"][, , indicator])
    }
    # remove model and variable name
    x <- collapseNames(x)
    # shorten names of the REMIND scenarios
    getNames(x) <- gsub(pattern, replacement, getNames(x))
    return(x)
  }

  if (grepl("_", subtype)) {

    subtype  <- strsplit(subtype, split = "_")
    revision <- numeric_version(unlist(subtype)[2])
    indicator <- unlist(subtype)[3]

    # Please refer to the 2019-R2M41/readme.txt for the source of the data
    fileList <- c("2019-R2M41/REMIND_generic_r8473-trunk-C_Budg600-rem-5.mif",
                  "2019-R2M41/REMIND_generic_r8473-trunk-C_Budg950-rem-5.mif",
                  "2019-R2M41/REMIND_generic_r8473-trunk-C_Budg1300-rem-5.mif",
                  "2019-R2M41/REMIND_generic_r8473-trunk-C_NDC-rem-5.mif",
                  "2019-R2M41/REMIND_generic_r8473-trunk-C_NPi-rem-5.mif")

    out <- .readAndRename(fileList = fileList,
                          pattern = "r8473-trunk-C_",
                          replacement = "R2M41-SSP2-",
                          indicator = indicator)

    if (revision > 4.58) {
      # Please refer to the 2021-R21M42/readme.txt for the source of the data
      fileList <- c("2021-R21M42/REMIND_generic_C_SDP-NPi-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SDP-PkBudg900-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SDP-PkBudg1000-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SDP-PkBudg1100-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP1-NPi-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP1-PkBudg900-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP1-PkBudg1100-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP1-PkBudg1300-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP2-NPi-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP2-PkBudg900-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP2-PkBudg1100-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP2-PkBudg1300-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP5-NPi-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP5-PkBudg900-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP5-PkBudg1100-rem-5.mif",
                    "2021-R21M42/REMIND_generic_C_SSP5-PkBudg1300-rem-5.mif")
      if (revision > 4.103) {
        fileList <- c(fileList,
                      "2021-R21M42/REMIND_generic_C_SDP-NDC-rem-5.mif",
                      "2021-R21M42/REMIND_generic_C_SSP1-NDC-rem-5.mif",
                      "2021-R21M42/REMIND_generic_C_SSP2-NDC-rem-5.mif",
                      "2021-R21M42/REMIND_generic_C_SSP5-NDC-rem-5.mif")
      }

      out <- mbind(out, .readAndRename(fileList = fileList,
                                       pattern = "C_(SDP|SSP)",
                                       replacement = "R21M42-\\1",
                                       indicator = indicator))
    }

    if (revision >= 4.96) {
      # Please refer to the 2023-R32M46/readme.txt for the source of the data
      fileList <- c("2023-R32M46/REMIND_generic_C_SDP_MC-NDC-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SDP_MC-NPi-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SDP_MC-PkBudg650-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP1-NDC-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP1-NPi-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP1-PkBudg1050-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP1-PkBudg650-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP2EU-NDC-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP2EU-NPi-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP2EU-PkBudg1050-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP2EU-PkBudg650-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP5-NDC-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP5-NPi-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP5-PkBudg1050-rem-5.mif",
                    "2023-R32M46/REMIND_generic_C_SSP5-PkBudg650-rem-5.mif")

      out <- mbind(out, .readAndRename(fileList = fileList,
                                       pattern = "C_(SDP_MC|SSP)",
                                       replacement = "R32M46-\\1",
                                       indicator = indicator))
    }

    if (revision >= 4.118) {
      # Please refer to the 2025-R34M49/readme.txt for the source of the data
      fileList <- c("2025-R34M49/REMIND_generic_C_SSP1-NPi2025-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP1-PkBudg1000-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP1-PkBudg650-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP2_lowEn-NPi2025-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP2_lowEn-PkBudg1000-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP2_lowEn-PkBudg650-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP2-NPi2025-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP2-PkBudg1000-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP2-PkBudg650-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP3-NPi2025-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP3-PkBudg1000-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP3-rollBack-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP5-NPi2025-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP5-PkBudg1000-rawluc-rem-5.mif",
                    "2025-R34M49/REMIND_generic_C_SSP5-PkBudg650-rawluc-rem-5.mif")

      out <- mbind(out, .readAndRename(fileList = fileList,
                                       pattern = "C_SSP",
                                       replacement = "R34M49-\\1",
                                       indicator = indicator))
    }
  }

  # shorten names of the REMIND scenarios
  getNames(out) <- gsub("-rem-5", "", getNames(out))

  return(out)

}

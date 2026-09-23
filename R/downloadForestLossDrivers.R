#' @title downloadForestLossDrivers
#'
#' @description Stages the ForestLossDrivers source files shipped with the package
#' (\code{inst/extdata/ForestLossDrivers}) into the madrat source folder: Table 1 of Curtis et al.
#' (2018), tree cover loss 2001-2015 by region and dominant driver, and the mapping of countries
#' to the source regions. Table 1 has no machine-readable release and is transcribed by hand.
#'
#' @author Michael Crawford
#' @importFrom utils bibentry person
#' @seealso \code{\link{readForestLossDrivers}}
#' @examples
#' \dontrun{
#' madrat::downloadSource("ForestLossDrivers")
#' }
downloadForestLossDrivers <- function() {

  extdata <- system.file("extdata", "ForestLossDrivers", package = "mrland")
  if (extdata == "") {
    stop("ForestLossDrivers package data not found. Is mrland installed correctly?")
  }

  staged <- file.copy(list.files(extdata, full.names = TRUE), ".", overwrite = TRUE)
  if (!all(staged)) {
    stop("Could not stage the ForestLossDrivers source files into the source folder.")
  }

  return(list(
    title = "Classifying drivers of global forest loss (Table 1)",
    description = paste("Tree cover loss 2001-2015 by region and dominant driver,",
                        "transcribed from Table 1 of Curtis et al. (2018), together",
                        "with the country to source-region mapping used to",
                        "disaggregate the regional totals."),
    author = c(person(c("Philip", "G."), "Curtis"),
               person(c("Christy", "M."), "Slay"),
               person(c("Nancy", "L."), "Harris"),
               person("Alexandra", "Tyukavina"),
               person(c("Matthew", "C."), "Hansen")),
    doi = "10.1126/science.aau3445",
    url = "https://www.science.org/doi/10.1126/science.aau3445",
    license = paste("Table transcribed from the article for use as a model parameter;",
                    "the underlying gridded product is CC BY 4.0."),
    version = "Science 361(6407):1108-1111, Table 1 (2001-2015)",
    unit = "Mha (2001-2015 total) and per cent of regional loss",
    reference = bibentry(
      "Article",
      title   = "Classifying drivers of global forest loss",
      author  = c(person(c("Philip", "G."), "Curtis"),
                  person(c("Christy", "M."), "Slay"),
                  person(c("Nancy", "L."), "Harris"),
                  person("Alexandra", "Tyukavina"),
                  person(c("Matthew", "C."), "Hansen")),
      year    = "2018",
      journal = "Science",
      volume  = "361",
      number  = "6407",
      pages   = "1108-1111",
      doi     = "10.1126/science.aau3445"
    )
  ))
}

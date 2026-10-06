#' @title downloadForestGrassi2023
#' @description Downloads the 0.5 degree map of intact and non-intact forest area of Grassi et al. (2023)
#' from their background data on Zenodo.
#' @author Florian Humpenöder
#' @seealso \code{\link{readForestGrassi2023}}
#' @examples
#' \dontrun{
#' downloadSource("ForestGrassi2023")
#' }
#' @importFrom utils download.file bibentry person

downloadForestGrassi2023 <- function() {

  url <- "https://zenodo.org/records/7650360/files/IntactAndNonIntactForest_0.5deg.nc"
  download.file(url, destfile = "IntactAndNonIntactForest_0.5deg.nc", mode = "wb",
                quiet = requireNamespace("testthat", quietly = TRUE) && testthat::is_testing())

  return(list(
    title = paste("Harmonising the land-use flux estimates of global models and national inventories",
                  "for 2000-2020: background data"),
    description = paste("Intact and non-intact forest area at 0.5 degree. Intact forest from Potapov et al. (2017)",
                        "for 2013, masks for Canada, Brazil and Russia updated from national information;",
                        "forest from Hansen et al. (2013)."),
    url = url,
    doi = "10.5281/zenodo.7650360",
    unit = "m2",
    version = "2023-02-17",
    release_date = "2023-02-17",
    author = person("Giacomo", "Grassi"),
    license = "CC BY 4.0",
    reference = bibentry(
      "Article",
      title = paste("Harmonising the land-use flux estimates of global models and national inventories",
                    "for 2000-2020"),
      author = c(person("Giacomo", "Grassi"), person("Clemens", "Schwingshackl"), person("Thomas", "Gasser"),
                 person("Richard A.", "Houghton"), person("Stephen", "Sitch"), person("Josep G.", "Canadell"),
                 person("Alessandro", "Cescatti"), person("Philippe", "Ciais"), person("Sandro", "Federici"),
                 person("Pierre", "Friedlingstein"), person("Werner A.", "Kurz"), person("Maria J.", "Sanz Sanchez"),
                 person("Ra\u00fal", "Abad Vi\u00f1as"), person("Ramdane", "Alkama"), person("Selma", "Bultan"),
                 person("Guido", "Ceccherini"), person("Stefanie", "Falk"), person("Etsushi", "Kato"),
                 person("Daniel", "Kennedy"), person("J\u00fcrgen", "Knauer"), person("Anu", "Korosuo"),
                 person("Joana", "Melo"), person("Matthew J.", "McGrath"), person("Julia E. M. S.", "Nabel"),
                 person("Benjamin", "Poulter"), person("Anna A.", "Romanovskaya"), person("Simone", "Rossi"),
                 person("Hanqin", "Tian"), person("Anthony P.", "Walker"), person("Wenping", "Yuan"),
                 person("Xu", "Yue"), person("Julia", "Pongratz")),
      year = "2023",
      journal = "Earth System Science Data",
      volume = "15",
      pages = "1093-1114",
      url = "https://doi.org/10.5194/essd-15-1093-2023",
      doi = "10.5194/essd-15-1093-2023"
    )
  ))
}

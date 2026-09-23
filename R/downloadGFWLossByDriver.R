#' @title downloadGFWLossByDriver
#'
#' @description Downloads tree cover loss by country, year and dominant driver from the Global
#' Forest Watch data API, table \code{gadm__tcl__iso_change}: the UMD/Hansen annual tree cover loss
#' cross-tabulated with the 1 km dominant-driver map of Sims et al. (2025). Tree cover extent in
#' 2000 at the same canopy threshold is taken from \code{gadm__tcl__iso_summary}.
#'
#' @author Michael Crawford
#' @importFrom utils download.file URLencode read.csv bibentry person
#' @importFrom withr local_options
#' @seealso \code{\link{readGFWLossByDriver}}
#' @examples
#' \dontrun{
#' madrat::downloadSource("GFWLossByDriver")
#' }

downloadGFWLossByDriver <- function() {

  # Dataset versions are immutable, so the pinned version fixes the download. The driver map behind
  # this version is the annually updated wri_google_tree_cover_loss_drivers v1.13 (2001-2025), not
  # the 2001-2022 map published with the paper, so regional driver shares differ from the paper's
  # tables.
  # Versions are listed under data.versions in the API response; is_latest is false on all of
  # them, so a re-pin has to pick by date rather than by that flag.
  gfwDataset <- "gadm__tcl__iso_change"          # annual loss by country and driver
  gfwVersion <- "v20260424"
  gfwSummaryDataset <- "gadm__tcl__iso_summary"  # tree cover extent 2000 by country
  gfwSummaryVersion <- "v20260424"
  # canopy density in 2000 (per cent) defining forest, GFW's convention; the total is sensitive to
  # it (global loss 2001-2025 is 647 Mha at 0 per cent and 334 Mha at 75)
  gfwCanopyThreshold <- 30

  dims <- paste("iso, umd_tree_cover_loss__year AS year,",
                "umd_tree_cover_density_2000__threshold AS threshold")
  where <- paste("FROM data WHERE umd_tree_cover_density_2000__threshold =",
                 gfwCanopyThreshold)

  # loss by driver, the same loss without the driver split (the read function checks that the two
  # agree) and the 2000 extent
  queries <- list(
    "loss_by_driver.csv" = paste("SELECT", dims,
                                 ", wri_google_tree_cover_loss_drivers__driver AS driver,",
                                 "SUM(umd_tree_cover_loss__ha) AS loss_ha", where,
                                 "GROUP BY iso, year, threshold, driver"),
    "loss_totals.csv" = paste("SELECT", dims,
                              ", SUM(umd_tree_cover_loss__ha) AS loss_ha", where,
                              "GROUP BY iso, year, threshold"),
    "extent_2000.csv" = paste("SELECT iso, umd_tree_cover_density_2000__threshold AS threshold,",
                              "SUM(umd_tree_cover_extent_2000__ha) AS extent_ha", where,
                              "GROUP BY iso, threshold")
  )
  datasets <- c("loss_by_driver.csv" = paste0(gfwDataset, "/", gfwVersion),
                "loss_totals.csv" = paste0(gfwDataset, "/", gfwVersion),
                "extent_2000.csv" = paste0(gfwSummaryDataset, "/", gfwSummaryVersion))
  headers <- c(
    "loss_by_driver.csv" = "\"iso\",\"year\",\"threshold\",\"driver\",\"loss_ha\"",
    "loss_totals.csv" = "\"iso\",\"year\",\"threshold\",\"loss_ha\"",
    "extent_2000.csv" = "\"iso\",\"threshold\",\"extent_ha\""
  )

  # a timeout warning would be an error, downloadSource() runs this with warn = 2
  local_options(timeout = max(3e6, getOption("timeout")))

  for (f in names(queries)) {
    # the open download route honours the full SQL; only the query route needs an API key
    url <- paste0("https://data-api.globalforestwatch.org/dataset/", datasets[[f]],
                  "/download/csv?sql=", URLencode(queries[[f]], reserved = TRUE))
    download.file(url, f, quiet = TRUE, mode = "wb")

    # download.file() errors on HTTP 400/500; any other non-CSV body is caught here
    header <- readLines(f, n = 1, warn = FALSE)
    if (!identical(header, unname(headers[f]))) {
      stop("GFWLossByDriver: the endpoint did not return the expected CSV for ", f,
           ". First line was: ", substr(header, 1, 200))
    }

    got <- unique(read.csv(f, stringsAsFactors = FALSE)$threshold)
    if (!identical(got, as.integer(gfwCanopyThreshold))) {
      stop("GFWLossByDriver: asked for canopy threshold ", gfwCanopyThreshold, " but ", f,
           " carries ", toString(got), ".")
    }
  }

  authors <- c(person(c("Michelle", "J."), "Sims"),
               person("Radost", "Stanimirova"),
               person("Anton", "Raichuk"),
               person("Maxim", "Neumann"),
               person("Jessica", "Richter"),
               person("Forrest", "Follett"),
               person("James", "MacCarthy"),
               person("Kristine", "Lister"),
               person("Christopher", "Randle"),
               person("Lindsey", "Sloat"),
               person("Elizabeth", "Esipova"),
               person("Jaelah", "Jupiter"),
               person("Charlotte", "Stanton"),
               person("Drew", "Morris"),
               person(c("Christy", "M."), "Slay"),
               person("Drew", "Purves"),
               person("Nancy", "Harris"))

  description <- paste("Annual UMD/Hansen tree cover loss 2001-2025, in hectares,",
                       "cross-tabulated by country and by the WRI/Google DeepMind 1 km",
                       "dominant-driver class, at a", gfwCanopyThreshold, "per cent canopy",
                       "density threshold, plus UMD tree cover extent in 2000 at the same",
                       "threshold (extent_2000.csv). Eight driver classes are present: the seven",
                       "of",
                       "Sims et al. (2025) plus 'Unknown'. NOTE: the driver map underlying",
                       "this extract is the annually updated 2001-2025 version",
                       "(wri_google_tree_cover_loss_drivers v1.13), not the 2001-2022",
                       "version published with the paper, so regional driver shares do not",
                       "reproduce the paper's tables.")

  reference <- bibentry("Article",
                        title = "Global drivers of forest loss at 1 km resolution",
                        author = authors,
                        year = "2025",
                        journal = "Environmental Research Letters",
                        volume = "20",
                        number = "7",
                        pages = "074027",
                        doi = "10.1088/1748-9326/add606")

  base <- paste0("https://data-api.globalforestwatch.org/dataset/", gfwDataset, "/",
                 gfwVersion)

  return(list(title = paste("Tree cover loss by country, year and dominant driver",
                            "(GFW gadm__tcl__iso_change)"),
              description = description,
              author = authors,
              doi = "10.1088/1748-9326/add606",
              url = base,
              license = "CC BY 4.0",
              version = paste0(gfwDataset, " ", gfwVersion, " (drivers map ",
                               "wri_google_tree_cover_loss_drivers v1.13, 2001-2025)"),
              unit = paste("ha of tree cover loss per country, year and driver;",
                           "ha of tree cover extent 2000 per country"),
              reference = reference))
}

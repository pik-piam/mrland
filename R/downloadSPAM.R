#' @title downloadSPAM
#' @description download SPAM 2010 v2.0 Global Data
#' @param subtype Type of SPAM data to be downloaded. Available are "harvestedArea" and "physicalArea".
#' @author David Hoetten
#' @importFrom utils download.file bibentry person
#' @importFrom madrat toolSubtypeSelect

downloadSPAM <- function(subtype) {

  urls <- c(harvestedArea =
              "https://s3.amazonaws.com/mapspam/2010/v2.0/geotiff/spam2010v2r0_global_harv_area.geotiff.zip",
            physicalArea =
              "https://s3.amazonaws.com/mapspam/2010/v2.0/geotiff/spam2010v2r0_global_phys_area.geotiff.zip")

  url <- toolSubtypeSelect(subtype, urls)
  name <- strsplit(url, "/")
  name <- name[[1]][8]
  download.file(url, name)
  download.file("https://s3.amazonaws.com/mapspam/2010/v1.1/readme_v1r1_global.txt",
                "readme_v1r1_global.txt") # download readme

  meta <- list(
    title = "SPAM 2010 v2.0 Global Data",
    url = url,
    doi = "10.7910/DVN/PRFF8V",
    description = paste(
      "SPAM (Spatial Production Allocation Model) data. \n",
      "\"A variety of information sources are used to generate plausible, \n
                        disaggregated estimates of crop distribution, \n
                        which are useful for understanding production and land use patterns.\" \n",
      "https://www.mapspam.info/about/"
    ),
    unit = "ha",
    version = "v2.0",
    author  = person("Yu", "Qiangyi"),
    license = "CC BY 4.0",
    reference =
      bibentry(
        "Article",
        title = "A cultivated planet in 2010 - Part 2: The global gridded agricultural-production maps",
        author = c(
          person("Yu", "Qiangyi"),
          person("You", "Liangzhi"),
          person("Wood-Sichra", "Ulrike"),
          person("Ru", "Yating"),
          person("Joglekar", "Alison K. B."),
          person("Fritz", "Steffen"),
          person("Xiong", "Wei"),
          person("Lu", "Miao"),
          person("Webin", "Wu"),
          person("Yang", "Peng")
        ),
        year = "2020",
        journal = "Earth System Science Data",
        url = "https://essd.copernicus.org/articles/12/3545/2020/essd-12-3545-2020.pdf",
        doi = "10.5194/essd-12-3545-2020")
  )

  return(meta)
}

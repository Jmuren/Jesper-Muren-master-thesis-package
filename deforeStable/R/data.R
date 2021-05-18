#' RGB images of forests, cities and farms.
#'
#' A list containing aeral images of 3 types of places: forests, farmlands and cities. All of them are from Scandinavia. There are 27 forest, 15 city and 7 farmland pictures. A table, describing the images are saved in a table named geoimages_desc.
#'
#' @format A list of 49 objects of an S4 class RasterStack containing RGB 0-1 images.  Every element of the list has colour intensities wrapped into variable
#' \describe{
#'   \item{layers}{1 is for intensity of red, 2- green, 3- of blue channel}
#' }
#' @source QGIS google plugin
"geoimages"

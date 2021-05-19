
#' Import a jpeg image
#'
#' read_data is to read jpeg images and return a 3-column data.frame with pixels in rows and
#' red, green, blue in columns. read_data_matrix reads jpeg images and return 3 matrices for
#' each of red, green and blue colors. read_data_raster imports jpeg as a raster object.
#'
#' @param filename name of the jpeg file to import
#' @param dir the directory where the image is located
#' @examples
#' dd<-read_data(filename, dir)
#' hist(dd[,1])
#'
#' @export
#' @export
read_data <- function(filename, dir){

  fil <- paste(dir, filename, sep = "")
  obj <- jpeg::readJPEG(source=fil)

  red <- as.vector(obj[,,1])
  green <- as.vector(obj[,,2])
  blue <- as.vector(obj[,,3])

  res <- data.frame(red, green, blue)
  return(res)
}


# 'Import a jpeg image
#' @examples
#' dd<-read_data_matrix(filename, dir)
#' jpeg::writeJPEG(image=dd[[1]], target='ex.jpeg')
#'
#' @describeIn read_data returns three matrices
#' @export
read_data_matrix <- function(filename, dir){

  fil <- paste(dir, filename, sep = "")
  obj <- jpeg::readJPEG(source=fil, native = FALSE)

  red <- obj[,,1]
  green <- obj[,,2]
  blue <- obj[,,3]


  return(list(red, green, blue))
}


#' Import jpeg as a raster object
#' @examples
#' dd<-read_data_raster(filename, dir)
#' plotRGB(dd, scale=1, asp=1)
#' @describeIn read_data returns a RasterStack object
#' @export
read_data_raster <- function(filename, dir){

  fil <- paste(dir, filename, sep = "")
  obj <- jpeg::readJPEG(source=fil)
  dims <- dim(obj)

  r1 <- r2 <- r3 <- raster(nrow=dims[1], ncol=dims[2])
  values(r1) <- as.vector(t(obj[,,1]))
  values(r2) <- as.vector(t(obj[,,2]))
  values(r3) <- as.vector(t(obj[,,3]))

  res <- stack(r1, r2, r3)
  return(res)
}









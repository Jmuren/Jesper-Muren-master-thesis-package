% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Classifiers_estimators.R
\name{Nonparam_classifier}
\alias{Nonparam_classifier}
\title{Classification of an image using the non parametric model based on the
squared Mahalanobis distance.}
\usage{
Nonparam_classifier(rastData, n_pts, references, thres, parallel = TRUE)
}
\arguments{
\item{rastData}{RasterStack as returned by function read_data_raster.}

\item{n_pts}{numeric for sub-image side length.}

\item{references}{list of data frames with same format as the one produced by
function read_data.}

\item{thres}{numeric threshold for squared Mahalanobis distance}

\item{parallel}{logical for activating parallel computing.}
}
\value{
A matrix of same dimension as input image, with element values 0 or
  1. Element value 0 indicates that pixel does not contain reference terrain,
  1 indicates the opposite.
}
\description{
Predict whether parts of a given image contains the terrains of provided
reference images by splitting the given image into smaller sub-images and
testing these against reference images. The argument rastData is the image to
be classified on the RasterStack format. Argument n_pts the side length of
square sub-images the image is split into. Argument references is a list of
data frames with colour intensities of reference images. Argument thres is a
threshold value for the squared Mahalanobis distance, deciding if a sub-image
belongs to reference terrain or not. The reference images and threshold
should be obtained from the NonParamCv function. Parallel decides if
computations should be parallelized.
}
\examples{

library(deforeStable)
library(raster)
library(doParallel)
data("geoimages")

obj <- geoimages[[28]]
ref <- geoimages[1:5]

reflist <- lapply(ref, as.matrix)


NonparClass <- Nonparam_classifier(rastData = obj, n_pts = 7, references = reflist, thres = 25, parallel = TRUE)
jpeg::writeJPEG(image = NonparClass, target = 'NonparClass.jpeg')
}

#' Anderson darling tester with argument for parameter.
#'
#' @importFrom goftest ad.test
#' @importFrom stabledist pstable
#' @importFrom StableEstim KoutParametersEstim
#' @import raster
#'
#'
#' @param params data.frame with stable distribution parameters
#' @param dataset data.frame a data frame with colour intensities corresponding
#'   to red, green and blue channels of the input image.
#' @return a data frame with statistics of AD tests corresponding to red, green
#'   and blue channels of the input image.
#' @examples
#' library(deforeStable)
#' library(raster)
#' data("geoimages")
#'
#' obj <- geoimages[[2]]
#' plotRGB(obj, scale=1, asp=1)
#'
#' mtrx <- as.matrix(obj)
#' pars <- Koutparams(mtrx)
#'
#' dd_ad<-Forest_Tester_AD(params=pars, dataset=mtrx)
#' @export
Forest_Tester_AD <- function(params,dataset){

  #Anderson-Darling tests using null hypothesis normal with parameters estimated from "Forest1" picture
  ad_red <- goftest::ad.test(as.vector(dataset[,1]), null = 'pstable',
                             alpha = params['red', 'alpha'], beta = params['red', 'beta'],
                             gamma = params['red', 'gamma'], delta = params['red', 'delta'])

  ad_green <- goftest::ad.test(as.vector(dataset[,2]), null = 'pstable',
                               alpha = params['green', 'alpha'], beta = params['green', 'beta'],
                               gamma = params['green', 'gamma'], delta = params['green', 'delta'])

  ad_blue <- goftest::ad.test(as.vector(dataset[,3]), null = 'pstable',
                              alpha = params['blue', 'alpha'], beta = params['blue', 'beta'],
                              gamma = params['blue', 'gamma'], delta = params['blue', 'delta'])

  data.frame(t(c(ad_red[[1]][[1]], ad_green[[1]][[1]], ad_blue[[1]][[1]])), fix.empty.names = FALSE)
}

#' Cramer von Mises tester with argument for parameter
#'
#' Performs Cramer-von Mises goodness-of-fit tests of empirical distributions of
#' colour intensities in the three channels against pre-specified stable
#' distributions. It is designed to be used in conjunction with Koutparams, and
#' argument params is a data frame returned by the latter. dataset is a data
#' frame with colour intensities of the image. It has the same format as the one
#' produced by read_data: observations are in rows and each of the 3 columns
#' correspond to red, green or blue colour.
#'
#' @param params data.frame with stable distribution parameters
#' @param dataset data.frame a data frame with colour intensities corresponding
#'   to red, green and blue channels of the input image.
#' @return a data frame with statistics of CvM tests corresponding to red, green
#'   and blue channels of the input image.
#' @examples
#'
#' library(deforeStable)
#' library(raster)
#' data("geoimages")
#'
#' obj <- geoimages[[2]]
#' plotRGB(obj, scale=1, asp=1)
#'
#' mtrx <- as.matrix(obj)
#' pars <- Koutparams(mtrx)
#'
#' dd_cvm <- Forest_Tester_CVM(params=pars, dataset=mtrx)
#' dd_cvm
#' @export
Forest_Tester_CVM <- function(params, dataset){

  cvm_red <- goftest::cvm.test(as.vector(dataset[,1]), null = 'pstable',
                               alpha = params['red', 'alpha'], beta = params['red', 'beta'],
                               gamma = params['red', 'gamma'], delta = params['red', 'delta'])

  cvm_green <- goftest::cvm.test(as.vector(dataset[,2]), null = 'pstable',
                                 alpha = params['green', 'alpha'], beta = params['green', 'beta'],
                                 gamma = params['green', 'gamma'], delta = params['green', 'delta'])

  cvm_blue <- goftest::cvm.test(as.vector(dataset[,3]), null = 'pstable',
                                alpha = params['blue', 'alpha'], beta = params['blue', 'beta'],
                                gamma = params['blue', 'gamma'], delta = params['blue', 'delta'])

  data.frame(t(c(cvm_red[[1]][[1]], cvm_green[[1]][[1]], cvm_blue[[1]][[1]])), fix.empty.names = FALSE)
}

#' Do multiple parametric tests and return best one
#'
#' Performs multiple hypothesis tests according to function func of empirical
#' data data against a list of parameters params. func and params must be
#' compatible, apart from that there are no specific restrictions imposed on
#' them.
#'
#' @param data a data frame with colour intensities corresponding to red, green
#'   and blue channels of the input image.
#' @param params a list of data frames with parameter values
#' @param func a function that performs a test and returns its statistic
#' @return data frame with smallest statistic.
#' @examples
#' library(deforeStable)
#' library(raster)
#' data("geoimages")
#' obj <- geoimages[[10]]
#' plotRGB(obj, scale=1, asp=1)
#'
#' mtrx <- as.matrix(obj)
#' pars <- Koutparams(mtrx)
#'
#' mpt <- MultipleTester(data=mtrx, params=list(pars, pars), func=Forest_Tester_CVM)
#' mpt
#' @export
MultipleTester <- function(data, params, func){

  test <- plyr::ldply(params, func, dataset = data,
                      .parallel = TRUE, .paropts = list(.packages = 'stabledist'))

  if(length(test)>1){

    test <- cbind(test, sum = rowSums(test))
    res <- test[which.min(test$sum),]
  } else{
    res <- min(test)
  }

  res
}

#' Koutrouvelis parameter estimation of image data
#'
#' In data, there are three columns and each column corresponds to the color
#' intensity of one channel: red, green and blue correspondingly. The four
#' parameters: alpha, beta, gamma and delta, of the stable distribution is
#' estimated for each of these channels using the Koutrouvelis regressions-type
#' technique.
#' @param data matrix or data frame with color intensities of red, green and
#'   blue for an image.
#' @return a data frame with columns alpha, beta, gamma, delta and rows red,
#'   green and blue.
#' @examples
#' library(deforeStable)
#' library(raster)
#' data("geoimages")
#'
#' obj <- geoimages[[2]]
#' plotRGB(obj, scale=1, asp=1)
#'
#' mtrx <- as.matrix(obj)
#' pars <- Koutparams(mtrx)
#' pars
#' @export
Koutparams <- function(data){

  r_m <- mean(data[,1])
  g_m <- mean(data[,2])
  b_m <- mean(data[,3])

  theta0r <- c(1.72, 0, 0.04, r_m)
  theta0g <- c(1.65, 0, 0.04, g_m)
  theta0b <- c(1.81, 0, 0.038, b_m)

  red <- KoutParametersEstim(x=data[,1], theta0 = NULL, spacing = "Kout",
                             pm = 0, tol = 0.05, NbIter = 10, PrintTime = FALSE)$Estim$par
  red[4] <- r_m

  green <-  KoutParametersEstim(x=data[,2], theta0 = NULL, spacing = "Kout",
                                pm = 0, tol = 0.05, NbIter = 10, PrintTime = FALSE)$Estim$par
  green[4] <- g_m

  blue <-  KoutParametersEstim(x=data[,3], theta0 = NULL, spacing = "Kout",
                               pm = 0, tol = 0.05, NbIter = 10, PrintTime = FALSE)$Estim$par
  blue[4] <- b_m

  dd <- rbind(red,green,blue)
  colnames(dd) <- c('alpha', 'beta', 'gamma', 'delta')
  as.data.frame(dd)
}


#' Squared Mahalanobis distance between two samples
#'
#' Calculates the squared Mahalanobis distance between two distributions of
#' colour intensities. Argument params is a data frame with the reference
#' disitribution to test against and argument dataset is a data frame with the
#' distribution to test. Both have the same format as the one produced by
#' read_data: observations are in rows and each of the 3 columns correspond to
#' red, green or blue colour.
#'
#'
#' @param params data.frame with colour intensities corresponding to red, green
#'   and blue channels of the input image.
#' @param dataset data.frame with colour intensities corresponding to red, green
#'   and blue channels of the input image.
#' @return a numeric with value of squared Mahalanobis distance.
#' @examples
#'
#' library(deforeStable)
#' library(raster)
#' data("geoimages")
#'
#' obj1 <- geoimages[[2]]
#' obj2 <- geoimages[[11]]
#' plotRGB(obj1, scale=1, asp=1)
#' plotRGB(obj2, scale=1, asp=1)
#'
#' mtrx <- as.matrix(obj1)
#' pars <- as.matrix(obj2)
#'
#' dd_Sqmahala <- Mahala_dist(params=pars, dataset=mtrx)
#' dd_Sqmahala
#' @export
Mahala_dist <- function(params, dataset) {

  diff <- as.vector(colMeans(params) - colMeans(dataset[,1:3]))
  cov1 <- cov(params)
  cov2 <- cov(dataset[,1:3])
  nrow1 <- nrow(params)
  nrow2 <- nrow(dataset[,1:3])
  S <- as.matrix((1/(nrow1 + nrow2 - 2)) * (((nrow1 - 1) * cov1) + ((nrow2 - 1) * cov2)))
  res <- t(diff) %*% chol2inv(chol(S)) %*% diff
  res[[1]]
}


#' @export
Group_stats <- function(matdata, n_pts, fun=Forest_Tester_CVM,
                        progress = "text", parallel = TRUE, paropts=NULL, ...){

  # number of groups
  r <- nrow(matdata[[1]]) %/% n_pts
  c <- ncol(matdata[[1]]) %/% n_pts

  # capture only one channel, small size
  m_df <- as.array(matdata)
  data_ch1 <- m_df[[1]]
  data_ch2 <- m_df[[2]]
  data_ch3 <- m_df[[3]]

  # Data truncation
  data_ch1 <- data_ch1[1:(r*n_pts), 1:(c*n_pts)]
  data_ch2 <- data_ch2[1:(r*n_pts), 1:(c*n_pts)]
  data_ch3 <- data_ch3[1:(r*n_pts), 1:(c*n_pts)]

  # Vectorization
  data_ch1_vec <- as.vector(data_ch1)
  data_ch2_vec <- as.vector(data_ch2)
  data_ch3_vec <- as.vector(data_ch3)

  # indexes of groups
  r1 <- rep(1:r, each = n_pts)
  rr <- rep(r1, times = c*n_pts)

  cc <- rep(1:c, each=r*n_pts^2)

  # data with indexes
  data_merged <- cbind(data_ch1_vec,
                       data_ch2_vec,
                       data_ch3_vec,
                       rr, cc)

  data_merged <- as.data.frame(data_merged)

  # result
  splitter_plyr <- utils::getFromNamespace("splitter_d", "plyr")
  data_split <- splitter_plyr(data_merged, plyr::.(rr, cc), drop = FALSE)
  data_l <- plyr::ldply(.data=data_split, .fun=fun, .paropts=paropts,
                        .progress = progress, .parallel = parallel, .inform=FALSE, ...)

  data_l

}

#' @export
classifier_green <- function(data, thres){

  data.frame(
    cbind(data[,1:2], as.numeric(data[,3] < thres)),
    fix.empty.names=FALSE)
}

#' @export
classifier_all <- function(data, thres){

  data.frame(
    cbind(data[,1:2], as.numeric(data[,3] < thres[1] & data[,4] < thres[2] & data[,5] < thres[3])),
    fix.empty.names=FALSE)
}

#' @export
clsfd_matr_restor <- function(c, n_pts, data){

  test <- data[,1:3]
  test2 <- plyr:::splitter_d(test, plyr::.(rr), drop = FALSE)


  test3 <- lapply(test2, function(x) x[,3])
  test3 <- lapply(test3, function(x) rep(x, times=n_pts, each=n_pts))

  test4 <- lapply(test3, function(x) matrix(x, nrow = n_pts, ncol = c*n_pts, byrow = TRUE))
  test4 <- do.call(rbind, test4)
  test4
}



#' Cross-validation for the Non-parametric model using the squared Mahalanobis
#' distance
#'
#' Performs k-fold cross-validation to find the optimal threshold for the
#' squared Mahalanobis distance to use for the non parametric model implemented
#' in function Nonparam_classifier. This is done by splitting images into
#' smaller sub-images and and for each sub-image the squared Mahalanobis
#' distance is computed with all forest images in the data set and the smallest
#' distance is chosen. These distances are compared to a range of thresholds and
#' the threshold which produces the best classification result according to the
#' accuracy is returned. As input it takes a path to a directory containing
#' forest images, a path to a directory contain non-forest images, the side
#' length of the sub-images the images are split into and the number of folds to
#' use for the k-fold cross-validation. Computations are parallelized.
#'
#'
#' @param forestdir character string with path to directory containing forest
#'   images.
#' @param Nonforestdir character string with path to directory containing
#'   non-forest images.
#' @param n_pts numeric for sub-image side length, default=7.
#' @param nrfolds numeric for number of folds in k-fold cross-validation,
#'   default=5.
#' @return list with first element: numeric with best accuracy and threshold
#'   which produced it. Second element: data frame with all tested thresholds
#'   and corresponding accuracy and other performance metrics. Third element:
#'   List of data frames with color intensities of forest images used to train.
#' @examples
#'
#' library(deforeStable)
#' library(raster)
#' library(doParallel)
#'
#' forestdir <- "forest image directory path"
#' Nonforestdir <- "Non-forest image directory path"
#' NonparCV <- NonParamCV(forestdir = forestdir, Nonforestdir = Nonforestdir,
#'                        n_pts = 7, nrfolds = 5)
#'
#' test_image <- read_data_raster(filename, dir)
#'
#' Nonpar_pred <- Nonparam_classifier(test_image, n_pts = 7, references = NonparCV[[3]],
#'                           thres = NonparCV[[1]][[1]])
#' jpeg::writeJPEG(image=Nonpar_pred, target='Nonpartest_im.jpeg')
#'
#'
#'
#' @export
NonParamCV <- function(forestdir, Nonforestdir, n_pts=7, nrfolds = 5){
  #Read image names
  Forest_list <- list.files(path=forestdir)

  Non_Forest_list <- list.files(path=Nonforestdir)

  #Combine image names into matrix with Forest and Not Forest labels
  dataset <- rbind(cbind(Im = Forest_list, label =rep("Forest", length(Forest_list))),
                   cbind(Im = Non_Forest_list, label = rep("Not Forest", length(Non_Forest_list))))


  nrfold <- nrfolds
  n_pts = n_pts
  #Repeated crossvalidation


  #Data randomly shuffled
  dataset<-dataset[sample(nrow(dataset)),]

  #Split data into k folds

  folds <- cut(seq(1,nrow(dataset)),breaks = nrfold ,labels=FALSE)

  #Compute test on all folds
  result_list <- list()
  #Perform k fold cross validation
  for(i in 1:nrfold){
    #Get holdout and training indices
    holdout_indexes <- which(folds==i, arr.ind=TRUE)
    holdout_data <- dataset[holdout_indexes, ]
    training_data <- dataset[-holdout_indexes, ]

    #Read holdout data
    if(sum(holdout_data[,2]=="Forest")>1){
      rawholdoutfor <- plyr::llply(as.list(holdout_data[holdout_data[,2]=="Forest",][,1]),
                                   read_data_matrix, dir = forestdir)
      rhfnames <- as.list(holdout_data[holdout_data[,2]=="Forest",][,1])
    } else if (sum(holdout_data[,2]=="Forest")==1){
      rawholdoutfor <- list(read_data_matrix(holdout_data[holdout_data[,2]=="Forest",][1], dir = forestdir))
      rhfnames <- list(holdout_data[holdout_data[,2]=="Forest",][1])
    } else {
      rawholdoutfor <- list()
      rhfnames <- list()
    }

    if(sum(holdout_data[,2]=="Not Forest")>1){
      rawholdoutnonfor <- plyr::llply(as.list(holdout_data[holdout_data[,2]=="Not Forest",][,1]),
                                      read_data_matrix, dir = Nonforestdir)
      rhnfnames <- as.list(holdout_data[holdout_data[,2]=="Not Forest",][,1])
    } else if (sum(holdout_data[,2]=="Not Forest")==1){
      rawholdoutnonfor <- list(read_data_matrix(holdout_data[holdout_data[,2]=="Not Forest",][1], dir = Nonforestdir))
      rhnfnames <- list(holdout_data[holdout_data[,2]=="Not Forest",][1])
    } else {
      rawholdoutnonfor <- list()
      rhnfnames <- list()
    }

    rawholdout <- c(rawholdoutfor, rawholdoutnonfor)
    honames <- c(rhfnames, rhnfnames)
    #Read training data
    rawtraining <- plyr::llply(as.list(training_data[which(training_data[,2] == "Forest"), ][,1]),
                               read_data_matrix, dir = forestdir)
    trainset <- plyr::llply(rawtraining, function(x) cbind(as.vector(x[[1]]), as.vector(x[[2]]), as.vector(x[[3]])))
    minrows <- min(unlist(plyr::llply(trainset, nrow)))
    trainset <- plyr::llply(trainset, function(x) x[sample(nrow(x), minrows, replace = FALSE),])

    #Run test on all images in hold out set
    tested <- foreach(dat = rawholdout, .packages = c("deforeStable")) %dopar% {
      Group_stats(matdata=dat, n_pts=n_pts, fun=MultipleTester, progress = "text",
                  parallel = FALSE, paropts=list(.packages = c("deforeStable")), params=trainset,
                  func = function(params,dataset) Mahala_dist(params,dataset))
    }



    names(tested) <- honames
    #Save tests to list for all holdout sets
    result_list[[i]] <- tested
  }



  #List of threshold values to test
  Thresholds <- as.list(seq(from = 0, to = n_pts*3, length.out=500))


  acc_list <- plyr::ldply(Thresholds, CompAcc, data = result_list, dataset = dataset,
                          fun = classifier_green, .parallel = TRUE)

  names(acc_list) <- c("Threshold", "Accuracy", "tp", "fp", "fn", "tn")

  bestavg <- acc_list[which.max(acc_list$Accuracy), 1:2]

  rawtraining <- plyr::llply(Forest_list,
                             read_data_matrix, dir = forestdir)
  trainset <- plyr::llply(rawtraining, function(x) cbind(as.vector(x[[1]]), as.vector(x[[2]]), as.vector(x[[3]])))
  minrows <- min(unlist(plyr::llply(trainset, nrow)))
  trainset <- plyr::llply(trainset, function(x) x[sample(nrow(x), minrows, replace = FALSE),])



  return(list(bestavg, acc_list, trainset))


}

#' Cross-validation for the parametric model using the Cramér-von Mises
#' statistic and stable distributions.
#'
#' Performs k-fold cross-validation to find the optimal threshold for the
#' Cramér-von Mises statistic to use for the parametric model implemented in
#' function Param_classifier. This is done by splitting images into smaller
#' sub-images and and for each sub-image the Cramér-von Mises statistics for
#' each of the three color intensities red, green and blue is computed with all
#' forest images in the data set and the smallest sum of statistics is chosen.
#' These statistics are compared to a range of thresholds and the threshold
#' which produces the best classification result according to the accuracy is
#' returned. As input it takes a path to a directory containing forest images, a
#' path to a directory contain non-forest images, the side length of the
#' sub-images the images are split into and the number of folds to use for the
#' k-fold cross-validation. Further, it takes a logical deciding if clustering
#' of parameter sets should be included, to reduce computational time. Lastly,
#' it takes the max threshold for the range checked against when trying to find
#' the optimal Cramér-von Mises threshold. Computations are parallelized.
#'
#'
#' @param forestdir character string with path to directory containing forest
#'   images.
#' @param Nonforestdir character string with path to directory containing
#'   non-forest images.
#' @param n_pts numeric for sub-image side length, default=7.
#' @param nrfolds numeric for number of folds in k-fold cross-validation,
#'   default=5.
#' @param clustering logical for deciding if clustering of forest image
#'   parameter sets should be included, default=TRUE.
#' @param maxt numeric for the max threshold range that the CvM statistic is
#'   checked against to maximize accuracy, default=14.
#' @return list with first element: numeric with best accuracy and threshold
#'   which produced it. Second element: data frame with all tested thresholds
#'   and corresponding accuracy and other performance metrics. Third element:
#'   List of data frames with parameter sets for the stable distributions of the
#'   color intensities of forest images used to train.
#'
#' @examples
#'
#' library(deforeStable)
#' library(raster)
#' library(doParallel)
#'
#' forestdir <- "forest image directory path"
#' Nonforestdir <- "Non-forest image directory path"
#' ParCV <- ParamCV(forestdir = forestdir, Nonforestdir = Nonforestdir,
#'                     n_pts = 7, nrfolds = 5, clustering = TRUE,
#'                     maxt = 14)
#'
#' test_image <- read_data_raster(filename, dir)
#'
#' Par_pred <- Param_classifier(test_image, n_pts = 7, pars = ParCV[[3]],
#'                           thresh = ParCV[[1]][1:3])
#' jpeg::writeJPEG(image=Par_pred, target='Partest_im.jpeg')
#'
#' @export
ParamCV <- function(forestdir, Nonforestdir, n_pts=7, nrfolds = 5, clustering=TRUE, maxt = 14){
  #Read image names
  Forest_list <- list.files(path=forestdir)

  Non_Forest_list <- list.files(path=Nonforestdir)

  #Combine image names into matrix with Forest and Not Forest labels
  dataset <- rbind(cbind(Im = Forest_list, label =rep("Forest", length(Forest_list))),
                   cbind(Im = Non_Forest_list, label = rep("Not Forest", length(Non_Forest_list))))

  Forest_data <- plyr::llply(Forest_list, read_data, dir = forestdir)

  Forest_params <- plyr::llply(Forest_data, Koutparams, .parallel = TRUE)
  names(Forest_params) <- Forest_list

  pstable <- stabledist::pstable


  nrfold <- nrfolds
  n_pts = n_pts
  #Repeated crossvalidation

  dataset<-dataset[sample(nrow(dataset)),]

  #Split data into k folds
  folds <- cut(seq(1,nrow(dataset)),breaks = nrfold ,labels=FALSE) # !!!!!!!!!!!!!!!!

  #Compute test on all folds
  result_list_para <- list()
  for(i in 1:nrfold){
    #Get holdout and training indices
    holdout_indexes <- which(folds==i, arr.ind=TRUE)
    holdout_data <- dataset[holdout_indexes, ]
    training_data <- dataset[-holdout_indexes, ]
    training_forests <-  training_data[training_data[,2]== "Forest",][,1]
    train_pars <- plyr::llply(training_forests, function(x) Forest_params[[x]])
    names(train_pars) <- training_forests

    #Read holdout data
    if(sum(holdout_data[,2]=="Forest")>1){
      rawholdoutfor <- plyr::llply(as.list(holdout_data[holdout_data[,2]=="Forest",][,1]),
                                   read_data_matrix, dir = forestdir)
      rhfnames <- as.list(holdout_data[holdout_data[,2]=="Forest",][,1])
    } else if (sum(holdout_data[,2]=="Forest")==1){
      rawholdoutfor <- list(read_data_matrix(holdout_data[holdout_data[,2]=="Forest",][1], dir = forestdir))
      rhfnames <- list(holdout_data[holdout_data[,2]=="Forest",][1])
    } else {
      rawholdoutfor <- list()
      rhfnames <- list()
    }

    if(sum(holdout_data[,2]=="Not Forest")>1){
      rawholdoutnonfor <- plyr::llply(as.list(holdout_data[holdout_data[,2]=="Not Forest",][,1]),
                                      read_data_matrix, dir = Nonforestdir)
      rhnfnames <- as.list(holdout_data[holdout_data[,2]=="Not Forest",][,1])
    } else if (sum(holdout_data[,2]=="Not Forest")==1){
      rawholdoutnonfor <- list(read_data_matrix(holdout_data[holdout_data[,2]=="Not Forest",][1], dir = Nonforestdir))
      rhnfnames <- list(holdout_data[holdout_data[,2]=="Not Forest",][1])
    } else {
      rawholdoutnonfor <- list()
      rhnfnames <- list()
    }

    rawholdout <- c(rawholdoutfor, rawholdoutnonfor)
    honames <- c(rhfnames, rhnfnames)

    #read forest pictures
    rtrainforest <- plyr::llply(as.list(training_forests), read_data_matrix, dir = forestdir)
    trainforest <- plyr::llply(rtrainforest, function(x) cbind(as.vector(x[[1]]), as.vector(x[[2]]), as.vector(x[[3]])))
    names(trainforest) <- training_forests

    #Mahala cluster
    if(clustering == TRUE){
      trainparams <- mahalaclust(ForestData = trainforest, clusters = 7, Forest_params = Forest_params)
    }else{
      trainparams <- Forest_params
    }

    #Run test on all images in hold out set
    tested <- foreach(dat = rawholdout, .packages = c("deforeStable")) %dopar% {
      Group_stats(matdata=dat, n_pts=n_pts, fun=MultipleTester, progress = "text",
                  parallel = FALSE, paropts=list(.packages = c("deforeStable")), params=unname(trainparams),
                  func = function(params,dataset) Forest_Tester_CVM(params,dataset))
    }


    names(tested) <- honames
    #Save tests to list for all holdout sets
    result_list_para[[i]] <- tested

  }



  #List of threshold values to test
  Thresholds <- as.list(seq(from = 0 , to = maxt, length.out=35))
  Thresholds <- expand.grid(Thresholds,Thresholds,Thresholds)
  Thresholds <- as.list(as.data.frame(t(Thresholds)))
  Thresholds <- plyr::llply(Thresholds, function(x) as.numeric(x))

  acc_list <- plyr::ldply(Thresholds, CompAcc, data = result_list_para, dataset = dataset,
                          fun = classifier_all, .parallel = TRUE)

  names(acc_list) <- c("id","Thres red","Thres green","Thres blue", "Accuracy", "tp", "fp", "fn", "tn")

  bestavg <- acc_list[which.max(acc_list$Accuracy),]

  rtrainforest <- plyr::llply(Forest_list, read_data_matrix, dir = forestdir)
  trainforest <- plyr::llply(rtrainforest, function(x) cbind(as.vector(x[[1]]), as.vector(x[[2]]), as.vector(x[[3]])))
  names(trainforest) <- Forest_list

  trainparams <- mahalaclust(ForestData = trainforest, clusters = 7, Forest_params = Forest_params)


  return(list(bestavg[2:5], acc_list[,-1], trainparams))
}


#' @export
CompAcc <- function(data, thres, fun, dataset){

  #Combine and check all validation sets against threshold
  results <- plyr::ldply(data, function(x) plyr::ldply(x, fun, thres = thres))
  #rename prediction column and make DF for confusion matrix
  colnames(results)[4] <- "pred"

  ConfMatData <- merge(x = results, y = dataset, by.x= ".id", by.y = "Im" )
  ConfMatData$pred[ConfMatData$pred==1]<-"Forest"
  ConfMatData$pred[ConfMatData$pred==0]<-"Not Forest"

  #Confusion matrix
  CM <- table(lapply(ConfMatData[,4:5], factor, levels = c("Forest", "Not Forest")))
  #Calculate overall accuracy
  acc <- sum(diag(CM))/sum(CM)
  tp <- CM[1,1]
  fp <- CM[1,2]
  fn <- CM[2,1]
  tn <- CM[2,2]


  return(c(thres, acc, tp, fp, fn, tn))

}

#' Clusters images of forest
#'
#' Clusters images of forest using hierarchical clustering and the squared
#' Mahalanobis distance in order to reduce the number of parameter sets used for
#' testing in the parametric model implemented in Param_classifier. Images that
#' are clustered together are assumed to be from the same distribution and have
#' their estimated stable distribution parameters averaged and returned. Inputs
#' are a list of forest image data on matrix form, the number of clusters to
#' cluster the data into and a list of data frames containing the estimated
#' stable distribution parameters for the provided forest images.
#'
#'
#' @param ForestData list of named matrices containing data of forest image
#'   color intensities, as returned by read_data.
#' @param clusters a numeric giving the number of clusters to divide the forest
#'   data into
#' @param Forest_params list of named data frames with stable distribution
#'   parameters, as retuned by function Koutparams.
#' @return list of data frames with averaged stable distribution parameter set
#'   for each cluster.
#' @examples
#' library(deforeStable)
#' library(raster)
#' data("geoimages")
#' data("geoimages_desc")
#'
#' data <- geoimages[1:5]
#'
#' datalist <- lapply(data, as.matrix)
#' parlist <- lapply(datalist, Koutparams)
#' names(datalist) <- as.list(geoimages_desc[1:5,1])
#' names(parlist) <- as.list(geoimages_desc[1:5,1])
#'
#' clustpars <- mahalaclust(ForestData = datalist, clusters = 2, Forest_params = parlist)
#' clustpars
#' @export
mahalaclust <- function(ForestData, clusters, Forest_params){

  #Compute distance matrix & cluster
  dmat <- as.dist(outer(ForestData, ForestData, Vectorize(Mahala_dist)))
  clust <- hclust(d = dmat)
  clustdf <- as.data.frame(cutree(clust, k = clusters))
  colnames(clustdf)[1] <- "cluster"
  if(is.null(Forest_params)){
    #STACK
  }else{
    g <-  lapply(Forest_params, function(x) rbind(x, colnames(x)))
    g1 <- lapply(g,unname)
    gdf <- t(as.data.frame(g1))
    names <- gsub('.{2}$', '', rownames(gdf))
    gn <- cbind(gdf, names)
    gm <- merge(gn, clustdf, by.x = "names", by.y = 0)
    gm2 <- gm[2:6]
    gm2[,1:3] <- sapply(gm2[,1:3], as.numeric )
    colnames(gm2)[4] <- "parameters"
    gg <- aggregate(.~parameters + cluster, data = gm2, FUN = mean)
    ggs <- split(gg, gg$cluster)
    ggl <- plyr::llply(ggs, function(x) as.data.frame(t(x[3:5])))
    res <- lapply(ggl, setNames, c("alpha", "beta", "gamma", "delta"))
  }

  return(res)
}


#' Classification of an image using the non parametric model based on the
#' squared Mahalanobis distance.
#'
#' Predict whether parts of a given image contains the terrains of provided
#' reference images by splitting the given image into smaller sub-images and
#' testing these against reference images. The argument rastData is the image to
#' be classified on the RasterStack format. Argument n_pts the side length of
#' square sub-images the image is split into. Argument references is a list of
#' data frames with colour intensities of reference images. Argument thres is a
#' threshold value for the squared Mahalanobis distance, deciding if a sub-image
#' belongs to reference terrain or not. The reference images and threshold
#' should be obtained from the NonParamCv function. Parallel decides if
#' computations should be parallelized.
#'
#' @param rastData RasterStack as returned by function read_data_raster.
#' @param n_pts numeric for sub-image side length.
#' @param references list of data frames with same format as the one produced by
#'   function read_data.
#' @param thres numeric threshold for squared Mahalanobis distance
#' @param parallel logical for activating parallel computing.
#' @return A matrix of same dimension as input image, with element values 0 or
#'   1. Element value 0 indicates that pixel does not contain reference terrain,
#'   1 indicates the opposite.
#' @examples
#'
#' library(deforeStable)
#' library(raster)
#' library(doParallel)
#' data("geoimages")
#'
#' obj <- geoimages[[28]]
#' ref <- geoimages[1:5]
#'
#' reflist <- lapply(ref, as.matrix)
#'
#'
#' NonparClass <- Nonparam_classifier(rastData = obj, n_pts = 7, references = reflist, thres = 25, parallel = TRUE)
#' jpeg::writeJPEG(image = NonparClass, target = 'NonparClass.jpeg')
#' @export
Nonparam_classifier <- function(rastData, n_pts, references, thres, parallel=TRUE){


  thres <- as.numeric(thres)
  matdata <- list(as.matrix(rastData[[1]]), as.matrix(rastData[[2]]), as.matrix(rastData[[3]]))
  Gst <- Group_stats(matdata=matdata, n_pts=n_pts, fun=MultipleTester, progress = "text",
                     parallel = TRUE, paropts=list(.packages = c("deforeStable")), params=references,
                     func = function(params,dataset) Mahala_dist(params,dataset))
  Gst_clfd <- classifier_green(data=Gst, thres=thres)

  cc <- max(Gst$cc)
  data_rest <- clsfd_matr_restor(c=cc, n_pts=n_pts, data=Gst_clfd)
  data_rest
}


#' Classification of an image using the parametric model based on the Cramér-von
#' Mises statistics
#'
#' Predict whether parts of a given image contains the terrains of provided
#' reference stable distributions by splitting the given image into smaller
#' sub-images and testing these against reference disitributions. The argument
#' rastData is the image to be classified on the RasterStack format. Argument
#' n_pts is the side length of square sub-images the image is split into.
#' Argument pars is a list of data frames with parameters for the stable
#' distributions of colour intensities of the reference images. Argument thres
#' is threshold values for the squared Cramér-von Mises statistics, deciding if
#' a sub-image belongs to reference terrain or not. Parameter sets and threshold
#' should be obtained from ParamCV function. Parallel decides if computations
#' should be parallelized.
#'
#' @param rastData RasterStack as returned by function read_data_raster.
#' @param n_pts numeric for sub-image side length.
#' @param references list of data frames with stable distribution parameters, as
#'   produced by function Koutparams.
#' @param thres numeric thresholds for Cramér-von Mises statistics
#' @param parallel logical for activating parallel computing.
#' @return A matrix of same dimension as input image, with element values 0 or
#'   1. Element value 0 indicates that pixel does not contain reference terrain,
#'   1 indicates the opposite.
#' @examples
#'
#' library(deforeStable)
#' library(raster)
#' library(doParallel)
#' data("geoimages")
#'
#' obj <- geoimages[[28]]
#' ref <- geoimages[1:5]
#'
#' reflist <- lapply(ref, as.matrix)
#' parlist <- lapply(reflist, Koutparams)
#'
#' parClass <- Param_classifier(rastData = obj, n_pts = 7, pars = parlist, thres = c(15,15,15), parallel = TRUE)
#' jpeg::writeJPEG(image = parClass, target = 'parClass.jpeg')
#' @export
Param_classifier <- function(rastData, n_pts, pars, thres, parallel=TRUE){

  pars <- unname(pars)
  thres <- as.numeric(thres)
  matdata <- list(as.matrix(rastData[[1]]), as.matrix(rastData[[2]]), as.matrix(rastData[[3]]))
  Gst <- Group_stats(matdata=matdata, n_pts=n_pts, fun=MultipleTester, progress = "text",
                     parallel = TRUE, paropts=list(.packages = c("deforeStable")), params=pars,
                     func = function(params,dataset) Forest_Tester_CVM(params,dataset))
  Gst_clfd <- classifier_all(data=Gst, thres=thres)

  cc <- max(Gst$cc)
  data_rest <- clsfd_matr_restor(c=cc, n_pts=n_pts, data=Gst_clfd)
  data_rest
}


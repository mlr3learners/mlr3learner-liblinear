#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  # get mlr_learners dictionary from the mlr3 namespace
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  # add the learner to the dictionary
  x$add("classif.liblinearl2l1svc", LearnerClassifLiblineaRL2L1SVC)
  x$add("classif.liblinearl1l2svc", LearnerClassifLiblineaRL1L2SVC)
  x$add("classif.liblinearl2l2svc", LearnerClassifLiblineaRL2L2SVC)
  x$add("classif.liblinearl1logreg", LearnerClassifLiblineaRL1LogReg)
  x$add("classif.liblinearl2logreg", LearnerClassifLiblineaRL2LogReg)
  x$add("classif.liblinearmulticlasssvc", LearnerClassifLiblineaRMultiClassSVC)
} # nocov end

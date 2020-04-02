#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

# nocov start
register_mlr3 = function(libname, pkgname) {
  # get mlr_learners dictionary from the mlr3 namespace
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  # add the learner to the dictionary
  x$add("classif.liblinearl2l1svc", LearnerClassifLiblineaRL2L1SVC)
  x$add("classif.liblinearl1l2svc", LearnerClassifLiblineaRL1L2SVC)
  x$add("classif.liblinearl2l2svc", LearnerClassifLiblineaRL2L2SVC)
  x$add("classif.liblinearl1logreg", LearnerClassifLiblineaRL1LogReg)
  x$add("classif.liblinearl2logreg", LearnerClassifLiblineaRL2LogReg)
  x$add("classif.liblinearmulticlasssvc", LearnerClassifLiblineaRMultiClassSVC)
  x$add("regr.liblinearl2l1svr", LearnerRegrLiblineaRL2L1SVR)
  x$add("regr.liblinearl2l2svr", LearnerRegrLiblineaRL2L2SVR)
}

.onLoad = function(libname, pkgname) { # nolint
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(),
    action = "append")
}

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3learners.liblinear"],
    action = "replace")
}
# nocov end

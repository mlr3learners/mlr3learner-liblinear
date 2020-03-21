#' @title L2-Regularized L2-Loss Support Vector Classification Learner
#'
#' @name mlr_learners_classif.liblinearl2l2svc
#'
#' @description
#' L2-Regularized L2-Loss support vector classification learner.
#' Calls [LiblineaR::LiblineaR()] (`type = 1` or `type = 2`) from package \CRANpkg{LiblineaR}.
#'
#' @note
#' If number of records > number of features `type = 2` is faster than `type =
#' 1` (Hsu et al. 2016).\cr If `epsilon` is missing and `type = 1` (default),
#' `epsilon` is set to `0.1`. If `type = 2`, `epsilon` is set to `0.01`.
#'
#' @templateVar id classif.liblinearl2l2svc
#' @template section_dictionary_learner
#'
#' @references
#' C-W. Hsu, Chang C-C., Lin and C-J (2016)\cr
#' A Practical Guide to Support Vector Classification\cr
#' \url{https://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf}\cr
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifLiblineaRL2L2SVC = R6Class("LearnerClassifLiblineaRL2L2SVC",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "cost", default = 1, lower = 0, tags = "train"),
          ParamDbl$new(id = "epsilon", default = NULL, special_vals = list(NULL), lower = 0, tags = "train"), # Package default depends on the type parameter
          ParamDbl$new(id = "bias", default = 1, tags = "train"),
          ParamFct$new(id = "type", default = "1", levels = c("1", "2"), tags = "train"),
          ParamInt$new(id = "cross", default = 0L, lower = 0L, tags = "train"),
          ParamLgl$new(id = "verbose", default = FALSE, tags = "train"),
          ParamUty$new(id = "wi", default = NULL, tags = "train")
        )
      )

      super$initialize(
        id = "classif.liblinearl2l2svc",
        packages = "LiblineaR",
        feature_types = "numeric",
        predict_types = "response",
        param_set = ps,
        properties = c("twoclass", "multiclass")
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      train = data[, task$feature_names, with = FALSE]
      target = data[, task$target_names, with = FALSE]

      if (is.null(pars$type)) {
        type = 1
      } else {
        type = as.numeric(pars$type)
      }
      pars = pars[names(pars) != "type"]

      invoke(LiblineaR::LiblineaR, data = train, target = target, type = type, .args = pars)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      p = invoke(predict, self$model, newx = newdata)
      PredictionClassif$new(task = task, response = p$predictions)
    }
  )
)

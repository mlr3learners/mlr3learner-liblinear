#' @title L2-Regularized L1-Loss Support Vector Regression
#'
#' @name mlr_learners_regr.liblinearl2l1svr
#'
#' @description
#' A [mlr3::LearnerRegr] for a L2-Regularized L1-Loss Support Vector Regression implemented in [LiblineaR::LiblineaR()] (`type = 13`) from package \CRANpkg{LiblineaR}.
#'
#' @templateVar id regr.liblinearl2l1svr
#' @template section_dictionary_learner
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrLiblineaRL2L1SVR = R6Class("LearnerRegrLiblineaRL2L1SVR",
  inherit = LearnerRegr,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "cost", default = 1, lower = 0, tags = "train"),
          ParamDbl$new(id = "epsilon", default = 0.1, lower = 0, tags = "train"),
          ParamDbl$new(id = "bias", default = 1, tags = "train"),
          ParamDbl$new(id = "svr_eps", default = 0.1, lower = 0, tags = "train"), # Package default is NULL but produces warning and sets value to 0.1
          ParamInt$new(id = "cross", default = 0L, lower = 0L, tags = "train"),
          ParamLgl$new(id = "verbose", default = FALSE, tags = "train")
        )
      )

      super$initialize(
        id = "regr.liblinearl2l1svr",
        packages = "LiblineaR",
        feature_types = c("integer", "numeric"),
        predict_types = "response",
        param_set = ps
      )
    }
  ),
  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      train = data[, task$feature_names, with = FALSE]
      target = data[, task$target_names, with = FALSE]

      if (is.null(pars$svr_eps)) {
        pars$svr_eps = 0.1
      }

      invoke(LiblineaR::LiblineaR, data = train, target = target, type = 13L, .args = pars)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      p = invoke(predict, self$model, newx = newdata)
      PredictionRegr$new(task = task, response = p$predictions)
    }
  )
)

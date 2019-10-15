#' @title Support Vector Classification
#'
#' @aliases mlr_learners_classif.liblinearmulticlasssvc
#' @format [R6::R6Class] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' A [mlr3::LearnerClassif] for a Support Vector Classification implemented in [LiblineaR::LiblineaR()] in package \CRANpkg{LiblineaR}.
#'
#' @export
LearnerClassifLiblineaRMultiClassSVC = R6Class("LearnerClassifLiblineaRMultiClassSVC", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamDbl$new(id = "cost", default = 1, lower = 0, tags = "train"),
          ParamDbl$new(id = "epsilon", default = 0.1, lower = 0, tags = "train"),
          ParamDbl$new(id = "bias", default = 1, tags = "train"),
          ParamInt$new(id = "cross", default = 0L, lower = 0L, tags = "train"),
          ParamLgl$new(id = "verbose", default = FALSE, tags = "train"),
          ParamUty$new(id = "wi", default = NULL, tags = "train")
        )
      )

      super$initialize(
        id = "classif.liblinearmulticlasssvc",
        packages = "LiblineaR",
        feature_types = "numeric",
        predict_types = c("response"),
        param_set = ps,
        properties = c("twoclass", "multiclass")
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      train = data[,task$feature_names, with=FALSE]
      target = data[,task$target_names, with=FALSE]

      invoke(LiblineaR::LiblineaR, data = train, target = target, type =4L, .args = pars)
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)

      p = invoke(predict, self$model, newx = newdata)
      PredictionClassif$new(task = task, response = p$predictions)
    }
  )
  )

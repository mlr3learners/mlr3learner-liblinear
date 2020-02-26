#' @title Support Vector Classification
#'
#' @name mlr_learners_classif.liblinearmulticlasssvc
#'
#' @description
#' A [mlr3::LearnerClassif] for a Support Vector Classification implemented in [LiblineaR::LiblineaR()] (`type = 4`) from package \CRANpkg{LiblineaR}.
#'
#' @templateVar id classif.liblinearmulticlasssvc
#' @template section_dictionary_learner
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerClassifLiblineaRMultiClassSVC = R6Class("LearnerClassifLiblineaRMultiClassSVC",
  inherit = LearnerClassif,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
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
    }
  ),
  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      data = task$data()
      train = data[, task$feature_names, with = FALSE]
      target = data[, task$target_names, with = FALSE]

      invoke(LiblineaR::LiblineaR, data = train, target = target, type = 4L, .args = pars)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      p = invoke(predict, self$model, newx = newdata)
      PredictionClassif$new(task = task, response = p$predictions)
    }
  )
)

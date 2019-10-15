#' @title L1-Regularized Logistic Regression
#'
#' @aliases mlr_learners_classif.liblinearl1logreg
#' @format [R6::R6Class] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' A [mlr3::LearnerClassif] for a L1-Regularized Logistic Regression implemented in [LiblineaR::LiblineaR()] in package \CRANpkg{LiblineaR}.
#'
#' @export
LearnerClassifLiblineaRL1LogReg = R6Class("LearnerClassifLiblineaRL1LogReg", inherit = LearnerClassif,
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
       id = "classif.liblinearl1logreg",
       packages = "LiblineaR",
       feature_types = "numeric",
       predict_types = c("response", "prob"),
       param_set = ps,
       properties = c("twoclass", "multiclass")
     )
   },

   train_internal = function(task) {
     pars = self$param_set$get_values(tags = "train")
     data = task$data()
     train = data[,task$feature_names, with=FALSE]
     target = data[,task$target_names, with=FALSE]

     invoke(LiblineaR::LiblineaR, data = train, target = target, type = 6L, .args = pars)
   },

   predict_internal = function(task) {
     newdata = task$data(cols = task$feature_names)

     if(self$predict_type == "response") {
       p = invoke(predict, self$model, newx = newdata)
       PredictionClassif$new(task = task, response = p$predictions)
     } else {
       p = invoke(predict, self$model, newx = newdata, proba = TRUE)
       PredictionClassif$new(task = task, prob = p$probabilities)
     }
   }
 )
)

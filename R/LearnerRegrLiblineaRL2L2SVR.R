#' @title L2-Regularized L2-Loss Support Vector Regression
#'
#' @aliases mlr_learners_regr.liblinearl2l2svr
#' @format [R6::R6Class] inheriting from [mlr3::LearnerRegr].
#'
#' @description
#' A [mlr3::LearnerRegr] for a L2-Regularized L2-Loss Support Vector Regression implemented in [LiblineaR::LiblineaR()] in package \CRANpkg{LiblineaR}.
#'
#' @note
#' If `epsilon` is missing and `type = 11` (default), `epsilon` is set to `0.01`. If `type = 12`, `epsilon` is set to `0.1`.
#'
#'
#' @export
LearnerRegrLiblineaRL2L2SVR= R6Class("LearnerRegrLiblineaRL2L2SVR", inherit = LearnerRegr,
 public = list(
   initialize = function() {
     ps = ParamSet$new(
       params = list(
         ParamDbl$new(id = "cost", default = 1, lower = 0, tags = "train"),
         ParamDbl$new(id = "epsilon", default = NULL, special_vals = list(NULL), lower = 0, tags = "train"), # Package default depends on the type parameter
         ParamDbl$new(id = "bias", default = 1, tags = "train"),
         ParamFct$new(id = "type", default = "11", levels = c("11", "12"), tags = "train"),
         ParamDbl$new(id = "svr_eps", default = 0.1, lower = 0, tags = "train"), # Package default is NULL but produces warning and sets value to 0.1
         ParamInt$new(id = "cross", default = 0L, lower = 0L, tags = "train"),
         ParamLgl$new(id = "verbose", default = FALSE, tags = "train")
       )
     )

     super$initialize(
       id = "regr.liblinearl2l1svr",
       packages = "LiblineaR",
       feature_types = c("integer" ,"numeric"),
       predict_types = "response",
       param_set = ps
     )
   },

   train_internal = function(task) {
     pars = self$param_set$get_values(tags = "train")
     data = task$data()
     train = data[,task$feature_names, with=FALSE]
     target = data[,task$target_names, with=FALSE]

     if(is.null(pars$svr_eps)) {
       pars$svr_eps = 0.1
     }

     if(is.null(pars$type)) {
       type = 11
     } else {
       type = as.numeric(pars$type)
     }
     pars = pars[names(pars) != "type"]

     invoke(LiblineaR::LiblineaR, data = train, target = target, type = type, .args = pars)
   },

   predict_internal = function(task) {
     newdata = task$data(cols = task$feature_names)

     p = invoke(predict, self$model, newx = newdata)
     PredictionRegr$new(task = task, response = p$predictions)
   }
 )
)

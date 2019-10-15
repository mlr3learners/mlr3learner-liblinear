#' @title L2-Regularized L1-Loss Support Vector Regression
#'
#' @aliases mlr_learners_regr.liblinearl2l1svr
#' @format [R6::R6Class] inheriting from [mlr3::LearnerRegr].
#'
#' @description
#' A [mlr3::LearnerRegr] for a L2-Regularized L1-Loss Support Vector Regression implemented in [LiblineaR::LiblineaR()] in package \CRANpkg{LiblineaR}.
#'
#' @export
LearnerRegrLiblineaRL2L1SVR= R6Class("LearnerRegrLiblineaRL2L1SVR", inherit = LearnerRegr,
  public = list(
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

     invoke(LiblineaR::LiblineaR, data = train, target = target, type =13L, .args = pars)
   },

   predict_internal = function(task) {
     newdata = task$data(cols = task$feature_names)

     p = invoke(predict, self$model, newx = newdata)
     PredictionRegr$new(task = task, response = p$predictions)
   }
  )
)

#' @title L2-Regularized L2-Loss Support Vector Classification
#'
#' @aliases mlr_learners_classif.liblinearl2l2svc
#' @format [R6::R6Class] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' A [mlr3::LearnerClassif] for a L2-Regularized L2-Loss Support Vector Classification implemented in [LiblineaR::LiblineaR()] in package \CRANpkg{LiblineaR}.
#'
#' @note
#' If number of records > number of features `type = 2` is faster than `type = 1` (Hsu et al. 2016)
#'
#' @references
#' C-W. Hsu, Chang C-C., Lin and C-J (2016)\cr
#' A Practical Guide to Support Vector Classification\cr
#' \url{https://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf}\cr
#'
#'
#' @export
LearnerClassifLiblineaRL2L2SVC = R6Class("LearnerClassifLiblineaRL2L2SVC", inherit = LearnerClassif,
 public = list(
   initialize = function() {
     ps = ParamSet$new(
       params = list(
         ParamDbl$new(id = "cost", default = 1, lower = 0, tags = "train"),
         ParamDbl$new(id = "epsilon", default = 0.1, lower = 0, tags = "train"),
         ParamDbl$new(id = "bias", default = 1, tags = "train"),
         ParamFct$new(id = "type", default = "1", levels = c("1", "2"), tags = "train")
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
   },

   train_internal = function(task) {
     pars = self$param_set$get_values(tags = "train")
     data = task$data()
     train = data[,task$feature_names, with=FALSE]
     target = data[,task$target_names, with=FALSE]

     if(is.null(pars$type)) {
       type = 1
     } else {
       type = as.numeric(pars$type)
     }
     pars = pars[names(pars) != "type"]

     invoke(LiblineaR::LiblineaR, data = train, target = target, type = type, .args = pars)
   },

   predict_internal = function(task) {
     newdata = task$data(cols = task$feature_names)

     p = invoke(predict, self$model, newx = newdata, type = type)
     PredictionClassif$new(task = task, response = p$predictions)
   }
 )
)

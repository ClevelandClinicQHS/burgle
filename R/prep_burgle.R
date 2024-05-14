# prep_burgle <- function(coef, cov = NULL, formula = NULL, xlevels = NULL){
#
#   if(!is.null(cov)){
#     warning("Covariance Matrix is missing. It will be set to a matrix of all 0's")
#     cov <- matrix(rep(0, length(coef)^2))
#   }
#   if(!is.null(xlevels)){
#     warning("xlevels are missing. Will be set to an empty list")
#     xlevels <- list()
#     names(xlevels) <- character(0)
#   }
#   if(!is.list(xlevels)){
#     if(is.null(names(xlevels))){
#       stop("xlevels is not a named list")
#     }
#   }
#   if(!is.matrix(cov)){
#     if(!is.numeric(cov)){
#     stop("cov must be a numeric matrix")
#     }
#   }
#   if(!is.numeric(coef)){
#     stop("coef must be numeric vector")
#   }
#   if(!is.character(formula)){
#     stop("formulas must be a vector of charatacter terms. If an intercept only model, please set 'formulas = '1''")
#   }
#
#
#   l <- list("coef" = coef,
#             "cov" = cov,
#             "xlevels" = xlevels,
#             "forumla" = formula)
#
#   class(l) <- "burgle_list"
#
# }
#
# predict.burgle_list <- function(object, newdata, original = FALSE, draws = 1, sims = 1, type = "lp"){
#
#
#   if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
#
#   if(!all(c('coef', 'cov', 'xlevels', 'formulas') %in% names(object))){
#     if(!'xlevels' %in% names(object)){
#       object$xlevels <- list()
#       warning("xlevels is not present will be sent to an empty list")
#     }
#     # if()
#
#   }
#
#
#   type <- match.arg(tolower(type), c("lp", "response", "link"))
#   nl <- names(object$xlevels)
#   ck0 <- nl %in% colnames(newdata)
#   if(!all(ck0)) stop(paste(nl[!ck0], "is not present in newdata"))
#   ulv <- lapply(nl, function(x) unique(newdata[,x]))
#   ck1s <- mapply(function(x, y) (y %in% x), object$xlevels, ulv, SIMPLIFY = FALSE)
#   ck1 <- sapply(ck1s, all)
#
#   if(length(ck1) > 0L){
#     if(!all(ck1)){
#       obs <- min(which(!ck1))
#       stop(
#         paste0("varaible ", names(object$xlevels)[obs], " has new level(s) of ", paste(ulv[[obs]][!ck1s[[obs]]], collapse = ","))
#       )
#     }
#   }
#
#   if(original & draws >1){
#     stop("Can only have one draw from the original model")
#   }
#   if(original){
#     models <- object$coef
#   }else{
#     models <- MASS::mvrnorm(n = draws, mu = object$coef, Sigma = object$cov)
#   }
#
# }

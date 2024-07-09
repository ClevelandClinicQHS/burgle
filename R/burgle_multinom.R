#' @name burgle_
#'
#' @export
burgle.multinom <- function(object, ...){

  coef <- stats::coef(object)

  ft <- as.character(attr(object$terms, "predvars"))[-c(1:2)]
  if (length(ft) < 1) {
    formula <- "1"
  }
  else {
    formula <- ft
  }

  tlo <- attr(object$terms, "order")
  if(any(tlo >1)){
    tl <- attr(object$terms, "term.labels")
    tl0 <- tl[which(tlo <=1)]
    tli <- tl[which(tlo >1)]
    tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
    formula <- c(formula, sapply(tli2, function(x) make_ints(x, o_form = formula, tl0 = tl0)))
  }
  if(attr(object$terms, "intercept") == 0L){
    formula <- c(formula, "-1")
  }

  if (length(coef) == 0L) {
    cov <- matrix(0)
  }
  else {
    cov <- stats::vcov(object)
  }

  xlevels <- object$covdata$xlev
  rlev <- object$lev

  vcoef <- as.vector(apply(coef, 1, I))
  names(vcoef) <- colnames(cov)

  l <- list(coef = vcoef, cov = cov, xlevels = xlevels, formula = formula, rlev = rlev)

  class(l) <- "burgle_multinom"
  l

}


#' @name predict_burgle
#'
#' @param floor will set the minimum odds to 0, if negative odds exists
#' @param seed a seed to specificy for simulating responses (multinomial only)
#' @export
predict.burgle_multinom <- function(object, newdata = NA, original = TRUE, draws = 1, sims = 1, type = "lp", floor = FALSE, seed = NULL, ...){
  if (!is.data.frame(newdata))
    stop("newdata must be an object of class data.frame")
  nc <- names(object$coef)
  type <- match.arg(tolower(type), c("lp", "response", "odds"))
  nl <- names(object$xlevels)
  ck0 <- nl %in% colnames(newdata)
  if (!all(ck0))
    stop(paste(nl[!ck0], "is not present in newdata"))
  ulv <- lapply(nl, function(x) unique(newdata[, x])[[1]])
  ck1s <- mapply(function(x, y) (y %in% x), object$xlevels,
                 ulv, SIMPLIFY = FALSE)
  ck1 <- sapply(ck1s, all)
  if (length(ck1) > 0L) {
    if (!all(ck1)) {
      obs <- min(which(!ck1))
      stop(paste0("varaible ", names(object$xlevels)[obs],
                  " has new level(s) of ", paste(ulv[[obs]][!ck1s[[obs]]],
                                                 collapse = ",")))
    }
  }

  if(original & draws >1){
    stop("Can only have one draw from the original model")
  }

  rlev <- object$rlev

  if(!is.null(seed)) set.seed(seed)

  if(original){
    models <- object$coef
  }else{
    models <- MASS::mvrnorm(n = draws, mu = object$coef, Sigma = object$cov)
  }

  mm <- stats::model.matrix(stats::reformulate(object$formula), data = newdata, xlev = object$xlevels)

  rnl <- length(rlev) - 1

  if(is.null(dim(models))){
    pr1 <- apply(t(matrix(models, ncol = rnl)), 1, function(x) mm %*% x)
  }else{
    pr1 <- apply(models, 1, function(x) apply(t(matrix(x, ncol = rnl)), 1, function(y) mm %*% y), simplify = FALSE)
  }
  if(!is.list(pr1) & !is.matrix(pr1)) pr1 <- matrix(pr1, ncol = rnl)

  if(type == "lp"){
    return(pr1)
  }

  if(is.list(pr1)) lp1 <- lapply(pr1, function(x) calc_multi_odds(x, floor = floor)) else lp1 <- calc_multi_odds(pr1, floor = floor)


  if(type == "odds"){
    return(lp1)
  }

  if(is.list(lp1)){
    neg_ck <- any(sapply(lp1, function(x) any(x <0)))
    if(neg_ck){stop('At least one probability is less than 0, set type = "odds" to see where or\nSet floor = TRUE to set the probabilty to 0')}
    pn <- lapply(lp1,
                 function(y) replicate(sims,
                                       apply(y, 1, function(x) sample(rlev, prob = x, size = 1)),
                                       simplify = FALSE
                 ))

    if(sims < 2) pn <- lapply(pn, function(x) if(length(x) == 1) x[[1]] else x)

  }else{
    neg_ck <- any(lp1 <0)
    if(neg_ck){stop('At least one probability is less than 0, set type = "odds" to see where or\nSet floor = TRUE to set the probabilty to 0')}

    pn <- replicate(sims,
                    apply(lp1, 1, function(x) sample(rlev, prob = x, size = 1)),
                    simplify = FALSE
    )

    if(length(pn) == 1L){pn <- pn[[1]]}
  }

  pn

}

calc_multi_odds <- function(mat, floor = FALSE){
  mat2 <- t(apply(mat, 1, function(x) exp(x)/(1 + sum(exp(x)))))
  if(floor){
    mat2 <- cbind(1-pmin(rowSums(mat2),1L), mat2)
  }else{
    mat2 <- cbind(1-rowSums(mat2), mat2)
  }
  return(mat2)
}

#' @name burgle_
#'
#' @export
burgle.coxph <- function(object, ...){
  bh <- survival::basehaz(object, centered = FALSE)
  ft <- as.character(attr(object$terms, "predvars"))[-c(1:2)]

  ## intercept only model
  if(length(ft)<1){
    formula <- "1"
  }else{
    formula <- ft
  }

  ## interactions
  tlo <- attr(object$terms, "order")
  if(any(tlo >1)){
    tl <- attr(object$terms, "term.labels")
    tl0 <- tl[which(tlo <=1)]
    tli <- tl[which(tlo >1)]
    tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
    formula <- c(formula, sapply(tli2, function(x) make_ints(x, o_form = formula, tl0 = tl0)))
  }
  ## or no other coefficients

  if(!is.null(object$xlevels) && (!is.null(object$strata)| any(grepl("strata", names(object$xlevels))))){

    bh0 <- bh[, c("hazard", "strata")]
    bh <- bh[!duplicated(bh0),]
    ft <- as.character(attr(object$terms, "predvars"))[-c(1:2)]
    ft2 <- ft[!grepl("strata", ft)]
    if(length(ft2)<1){
      formula <- "1"
    }else{
      formula <- ft2
    }
    ## interactions
    tlo <- attr(object$terms, "order")
    if(any(tlo >1)){
      tl <- attr(object$terms, "term.labels")
      tl0 <- tl[which(tlo <=1)]
      tli <- tl[which(tlo >1)]
      tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
      formula <- c(formula, sapply(tli2, function(x) make_ints(x, o_form = formula, tl0 = tl0)))
    }

  }else{
    bh <- bh[!duplicated(bh$hazard),]
  }
  coef <- stats::coef(object)
  if(length(coef) == 0L){
    cov <- matrix(0)
  }else{
    cov <- stats::vcov(object)
  }
  rss <- sum(object$residuals ^2)/(object$n - length(coef))
  xlevels <- object$xlevels
  l <- list("coef" = coef,
            "cov" = cov,
            "rss" = rss,
            "xlevels" = xlevels,
            "formula" = formula,
            "basehaz" = bh)
  class(l) <- "burgle_coxph"
  l
}


#' @name predict_burgle
#'
#' @param times if type = "risk" time for which to predict risk, if times and sims is multiple the return will be lists within lists
#' @export
predict.burgle_coxph <- function(object, newdata = NA, original = TRUE, draws = 1, sims = 1, type = "lp", times = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "response", "risk"))

  ### check strata
  str_ck <- grepl("strata", names(object$xlevels))
  o_xlvs <- xlvs <- object$xlevels

  if(any(str_ck)){
    str1 <- object$xlevels[str_ck]
    o_xlvs <- object$xlevels[!str_ck]
    str1n <- names(str1)
    str1_v <- strsplit(gsub("strata|\\)|\\(", "", str1n), ", ")[[1]]
    vn <- length(str1_v)
    str1_ls <- strsplit(str1[[1]], ", ")
    new_xlvs <- lapply(seq_len(vn), function(y) unique(sapply(str1_ls, function(x) x[y])))
    ## if strata is numeric, tends to have an '=' sign
    new_xlvs <- lapply(new_xlvs, function(x) gsub(".*=", "", x))
    names(new_xlvs) <- str1_v
    xlvs <- append(o_xlvs, new_xlvs)
  }

  nl <- names(xlvs)
  ck0 <- nl %in% colnames(newdata)
  if(!all(ck0)) stop(paste(nl[!ck0], "is not present in newdata"))
  ulv <- lapply(nl, function(x) unique(newdata[,x]))
  ck1s <- mapply(function(x, y) (y %in% x), xlvs, ulv, SIMPLIFY = FALSE)
  ck1 <- sapply(ck1s, all)

  if(length(ck1) > 0L){
    if(!all(ck1)){
      obs <- min(which(!ck1))
      stop(
        paste0("variable ", names(object$xlevels)[obs], " has new level(s) of ", paste(ulv[[obs]][!ck1s[[obs]]], collapse = ","))

      )
    }
  }

  if(original & draws >1){
    stop("Can only have one draw from the original model")
  }
  o_coef <- object$coef
  if(is.null(o_coef)){
    o_coef <- 0L
  }
  if(original){
    models <- o_coef
  }else{
    models <- MASS::mvrnorm(n = draws, mu = o_coef, Sigma = object$cov)
  }

  mm <- stats::model.matrix(stats::reformulate(object$formula), data = newdata, xlev = o_xlvs)
  if(!is.integer(o_coef)){
    mm <- matrix(mm[,-1], nrow = nrow(newdata))
  }

  if(!is.null(dim(models))){
    preds <- apply(models, 1, function(x) mm %*% x)
  }else{
    preds <- mm %*% models
  }

  if(type == "lp"){
    return(preds)
  }

  bh <- object$basehaz


  if(is.null(times)) stop("times is missing")
  if(max(times) > max(bh$time)) warning(paste("times has a value of", max(times), "which is larger than the maximum time value of", max(bh$time)))

  if(any(str_ck)){
    str_s <- survival::strata(newdata[,names(new_xlvs)], shortlabel = TRUE)
    if(any(grepl("=", bh$strata))){
      if(length(new_xlvs) == 1){bh$strata <- factor(gsub(".*=", "", bh$strata))}
      else{bh$strata <- factor(paste0(sapply(regmatches(bh$strata, gregexpr("(?<==)(.*?)(?=,)", bh$strata, perl = TRUE)), paste, collapse = ", ")
                                      , ", ", gsub(".*=", "", bh$strata)))}
    }

    nd_i <- lapply(levels(bh$strata), function(x) which(str_s == x))
    nd_e <- sapply(nd_i, function(x) length(x)>0L)

    bh_tr <- lapply(times, function(x) bh[bh$time <= x,])

    bh_str <- lapply(levels(bh$strata), function(x) Reduce(rbind, lapply(bh_tr, function(y) utils::tail(y[y$strata == x,], 1))))

    ## check for minimum time needs to be put here, just set hazard to 0
    bh_str <- mapply(function(x, y) if(nrow(x) < length(times)){return(rbind(data.frame(hazard = rep(0, length(times)-nrow(x)), time = -Inf, strata = y), x))}else{x}, bh_str, levels(bh$strata), SIMPLIFY = FALSE)

    nd_i <- nd_i[nd_e]
    bh_str <- bh_str[nd_e]

    pr0 <- mapply(function(x, y) cbind(matrix(sapply(y$hazard, function(z) (1-exp(-z)^exp(preds[x,]))), ncol = length(times)
                                              , dimnames = list(rep(x, ncol(preds)), times)
    ), model = 1:(ncol(preds))), nd_i, bh_str)

    pr0 <- Reduce(rbind, pr0)


  }else{

    bh_tr <- Reduce(rbind, lapply(times, function(x) utils::tail(bh[bh$time <= x,], 1)))
    if(nrow(bh_tr) < length(times)){
      bh_tr <- rbind(data.frame(hazard = rep(0, length(times)-nrow(bh_tr)), time = -Inf), bh_tr)
    }

    pr0 <- cbind(matrix(sapply(bh_tr$hazard, function(z) (1-exp(-z)^exp(preds))),
                        ncol = length(times),
                        dimnames = list(rep(1:nrow(preds), ncol(preds)), times)),
                 model = sort(rep(1:(ncol(preds)), nrow(newdata))))

  }

  pr0 <- pr0[order(as.numeric(row.names(pr0)), as.numeric(pr0[,"model"])), ]

  pr0 <- lapply(1:draws, function(x) matrix(pr0[pr0[,"model"] == x, 1:length(times)], ncol = length(times)))

  if(is.list(pr0) & length(pr0) == 1){pr0 <- pr0[[1]]}

  if(type == "risk"){

    return(pr0)
  }

  if(sims >= 1 & type == "response"){
    if(!is.null(dim(pr0))){

      pn <- replicate(sims,
                      apply(pr0, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)),
                      simplify = FALSE)

      if(sims < 2) pn <- pn[[1]]

    }else{
      pn <- lapply(pr0,
                   function(y) replicate(sims, apply(y, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)),
                                         simplify = FALSE))

      if(sims < 2) pn <- lapply(pn, function(x) if(length(x) == 1) x[[1]] else x)


    }

  }

  pn

}


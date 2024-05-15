#' Predict for burgle methods
#'
#' @rdname predict_burgle
#'
#' @param object the results of burgle_lm object
#' @param newdata new data
#' @param original whether or not to predict using the original model
#' @param draws how many different models to simulate
#' @param sims how many simulated response to draw
#' @param type either 'lp', 'response', 'link' for glm or 'risk' if time dependent
#' @param se whether or not to include the standard error in the simulations
#' @param ... for future methods
#'
#' @return either a matrix of array of new model predictions
#' @export
#'
predict.burgle_lm <- function(object, newdata, original = FALSE, draws = 1, sims = 1, type = "lp", se = FALSE, ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "response", "link"))
  nl <- names(object$xlevels)
  ck0 <- nl %in% colnames(newdata)
  if(!all(ck0)) stop(paste(nl[!ck0], "is not present in newdata"))
  ulv <- lapply(nl, function(x) unique(newdata[,x]))
  ck1s <- mapply(function(x, y) (y %in% x), object$xlevels, ulv, SIMPLIFY = FALSE)
  ck1 <- sapply(ck1s, all)

  if(length(ck1) > 0L){
    if(!all(ck1)){
      obs <- min(which(!ck1))
      stop(
        paste0("varaible ", names(object$xlevels)[obs], " has new level(s) of ", paste(ulv[[obs]][!ck1s[[obs]]], collapse = ","))
      )

    }

  }
  if(original & draws >1){
    stop("Can only have one draw from the original model")
  }
  if(original){
    models <- object$coef
  }else{
    models <- MASS::mvrnorm(n = draws, mu = object$coef, Sigma = object$cov)
  }

  mm <- stats::model.matrix(stats::reformulate(object$formula), data = newdata, xlev = object$xlevels)

  if(!is.null(dim(models))){
    preds <- apply(models, 1, function(x) mm %*% x)
  }else{
    preds <- mm %*% models
  }

  if(type == "lp"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  ## rows are observation
  ## columns are models
  ## lists are the simulations
  pn <- replicate(sims,
                  apply(preds, 2, function(x) stats::rnorm(n = length(x), mean = x, sd = ifelse(se, sqrt(object$rss), 0))),
                  simplify = FALSE)

  if(length(pn) == 1L){pn <- pn[[1]]}

  pn

}


#' @name predict_burgle
#'
#' @export
predict.burgle_glm <- function(object, newdata, original = FALSE, draws = 1, sims = 1, type = "lp", se = FALSE, ...){
  preds <- predict.burgle_lm(object, newdata = newdata, original = original, draws = draws, sims = 1, type = "lp", se = FALSE,  ... = ...)
  preds <- replicate(sims,
                     apply(preds, 2, function(x) stats::rnorm(n = length(x), mean = x, sd = ifelse(se, sqrt(object$rss), 0))),
                     simplify = FALSE)
  if(length(preds) == 1L){
    preds <- preds[[1]]
  }
  if(type == "lp"){
    return(preds)
  }
  if(is.null(dim(preds))){
    preds <-  lapply(preds, object$inv_link)
  }else{
    preds <- object$inv_link(preds)
  }
  if(type == "link"){
    return(preds)
  }
  if(type == "response"){
    if(!grepl("binomial", object$family)) stop("please use type = 'link' for model families other than binomial and quasibinomial")
    if(!is.null(dim(preds))){
      pn <- apply(preds, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x))
    }else{
      pn <- lapply(preds,
                   function(y) apply(y, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)))
    }
  }

  pn

}

#' @name predict_burgle
#'
#' @param times if type = "risk" time for which to predict risk, if times and sims is multiple the return will be lists within lists
#' @export
predict.burgle_coxph <- function(object, newdata = NA, original = FALSE, draws = 1, sims = 1, type = "lp", times = NULL, ...){

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
  # mm <- matrix(stats::model.matrix(object$formula, data = newdata, xlev = o_xlvs)[,-1], nrow = nrow(newdata))

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
    # return(bh_str)

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
    # return(pr0)
    if(is.list(pr0) & length(pr0) == 1){pr0 <- pr0[[1]]}

    if(type == "risk"){
      # if(is.list(pr0) & length(pr0) == 1){pr0 <- pr0[[1]]}
      return(pr0)
    }

  if(sims >= 1 & type == "response"){
    if(!is.null(dim(pr0))){

      pn <- replicate(sims,
                      apply(pr0, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)),
                      simplify = FALSE)
      # return(pn)

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

predictCIF_cpp <- get("predictCIF_cpp", envir = asNamespace("riskRegression"), inherits = FALSE)

#' @name predict_burgle
#'
#' @param cause which cause do you want to predict
#'
#' @export
predict.burgle_CauseSpecificCox <- function(object, newdata = NULL, type = "lp", cause = 1, original = FALSE, draws = 1, sims = 1, times = NULL, ...) {
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  nMods <- length(object$cumhazards)
  if(!is.numeric(cause)| cause < 0| cause > nMods) stop("Invalid cause")
  type <- match.arg(tolower(type), c("lp", "response", "risk"))
  nOs <- nrow(newdata)
  ### check strata
  str_cks <- lapply(object$xlevels, function(x) grepl("strata", names(x)))
  o_xlvs <- xlvs <- object$xlevels

  new_xlvs <- vector(mode = "list", length = nMods)
  M.strata <- matrix(NA, nrow = nOs, ncol = nMods)
  M.eTime <- matrix(NA, nrow = nOs, ncol = nMods)

  for(i in 1:nMods){
    str_ck <- str_cks[[i]]
    o1 <- object$xlevels[[i]]
    n1 <- new_xlvs[[i]]

    if(any(str_ck)){
      str1 <- o1[str_ck]
      str_lv <- factor(str1[[1]])
      o_xlvs[[i]] <- o1 <- o1[!str_ck]
      str1n <- names(str1)
      str1_v <- strsplit(gsub("strata|\\)|\\(", "", str1n), ", ")[[1]]
      vn <- length(str1_v)
      str1_ls <- strsplit(str1[[1]], ", ")
      n1 <- lapply(seq_len(vn), function(y) unique(sapply(str1_ls, function(x) x[y])))
      names(n1) <- str1_v
      xlvs[[i]] <- append(o1, n1)
      ## New strata
      attr(M.strata, paste0("levels", i)) <- as.character(str_lv)
      str_s <- as.numeric(factor(survival::strata(newdata[,names(n1)], shortlabel = TRUE), levels = str_lv))-1
      M.strata[,i] <- str_s

    }else{
      M.strata[,i] <- 0
      attr(M.strata, paste0("levels", i)) <- factor(1)
    }

    M.eTime[, i] <- object$Lastevent[[i]][M.strata[, i] + 1]

  }

  vec.Etime <- apply(M.eTime, 1, max)

  if(original & draws > 1){
    stop("Can only have one draw from the original model")
  }
  o_coef <- object$coef
  no_coef <- sapply(o_coef, is.null)
  if(any(no_coef)){
    o_coef[no_coef] <- 0L
  }

  if(original){
    models <- o_coef
  }else{
    models <- lapply(1:nMods, function(x) MASS::mvrnorm(n = draws, mu = o_coef[[x]], Sigma = object$cov[[x]]))
  }
  ## add to something if only one covariate
  mms <- lapply(1:nMods, function(x) stats::model.matrix(stats::reformulate(object$formula[[x]]), data = newdata, xlev = o_xlvs[[x]]))
  if(any(!no_coef)){
    mms[!no_coef] <- lapply(mms[!no_coef], function(x) matrix(x[,-1] , nrow = nrow(newdata)))
  }

  # mms <- lapply(1:nMods, function(x) matrix(stats::model.matrix(object$formula[[x]], data = newdata, xlev = o_xlvs[[x]])[,-1], nrow = nrow(newdata)))


  preds <- mapply(function(x, y){
    if(!is.null(dim(x))){
      p1 <- apply(x, 1, function(x) y %*% x)
    }else{
      p1 <- y %*% x
    }
    p1
  }, models, mms, SIMPLIFY = TRUE)

  if(nrow(newdata) == 1L & draws == 1L){preds <- matrix(preds, nrow = 1)}

  if(nrow(preds) > nrow(newdata)){
    ov <- (nrow(newdata) * 1:draws)-draws
    if(length(ov == 1L)){ov <- 1}
    preds <- mapply(function(x, y) preds[c(x:y), ], ov, (nrow(newdata) *1:draws), SIMPLIFY = FALSE)
  }
  if(type == "lp"){
    return(preds)
  }
  # if(type == "risk"){

    if(is.null(times)) stop("times is missing")
    if(is.unsorted(times)) warning("times is unsorted")
    if(max(times) > max(sapply(object$Lastevent, max))) warning(paste("times has a value of", max(times), "which is larger than the maximum time value of", max(sapply(object$Lastevent, max)), "risk esimates may be NA or unreliable"))


    if(is.list(preds)){
      preds <- lapply(preds, function(x) predictCIF_cpp(hazard = object$hazard, cumhazard = object$cumhazards,
                                                                         eXb = exp(x), strata = M.strata, newtimes = times, etimes = object$eventTimes, etimeMax = vec.Etime,
                                                                         t0 = 0, nEventTimes = length(object$eventTimes), nNewTimes = length(times),
                                                                         nData = nrow(newdata), cause = cause - 1, nCause = nMods,
                                                                         survtype = FALSE, productLimit = TRUE >
                                                                           0, diag = FALSE, exportSurv = FALSE)[["cif"]])
    }else{
      preds <- predictCIF_cpp(hazard = object$hazard, cumhazard = object$cumhazards,
                                               eXb = exp(preds), strata = M.strata, newtimes = times, etimes = object$eventTimes, etimeMax = vec.Etime,
                                               t0 = 0, nEventTimes = length(object$eventTimes), nNewTimes = length(times),
                                               nData = nrow(newdata), cause = cause - 1, nCause = nMods,
                                               survtype = FALSE, productLimit = TRUE >
                                                 0, diag = FALSE, exportSurv = FALSE)[["cif"]]
    }

  # }
  if(type == "risk"){
    return(preds)
  }

  if(sims >= 1L & type == "response"){
    if(!is.null(dim(preds))){
      preds <- replicate(sims, apply(preds, MARGIN = 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)), simplify = FALSE)
      if(sims == 1L) preds <- preds[[1]]
    }else{
      preds <- lapply(preds, function(y) replicate(sims, apply(y, MARGIN = 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)), simplify = FALSE))
    }
  }
  preds

}

# predictCIF_cpp <- utils::getFromNamespace("predictCIF_cpp", "riskRegression")
# predictCIF_cpp <- get("predictCIF_cpp", envir = asNamespace("riskRegression"), inherits = FALSE)

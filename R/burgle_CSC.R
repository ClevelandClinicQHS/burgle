#' @name burgle_
#'
#' @export
burgle.CauseSpecificCox <- function(object, ...){

  models <- object$models
  evtimes <- object$eventTimes
  Net <- length(evtimes)

  basehzs <- lapply(models, function(x) riskRegression::predictCox(x, centered = FALSE,
                                                                   times = evtimes, newdata = NULL, type = c("hazard", "cumhazard"), keep.strata = TRUE, keep.times = TRUE,
                                                                   se = FALSE, keep.infoVar = TRUE))

  cumhazards <- lapply(basehzs, function(x) matrix(x[["cumhazard"]], byrow = FALSE, nrow = Net))
  hazards <- lapply(basehzs, function(x) matrix(x[["hazard"]], byrow = FALSE, nrow = Net))
  levT <- lapply(basehzs, `[[`, "lastEventTime")
  coefs <- lapply(models, stats::coef)

  vcovs <- mapply(function(x, y) if(length(x) == 0L){return(matrix(0))}else{stats::vcov(y)}, coefs, models, SIMPLIFY = FALSE)

  formulas <- lapply(models, function(x){
    ft <- as.character(attr(x$terms, "predvars"))[-c(1:2)]
    if(length(ft) <1){
      return("1")
    }
    f1 <- ft
    ## interactions
    tlo <- attr(x$terms, "order")
    if(any(tlo >1)){
      tl <- attr(x$terms, "term.labels")
      tl0 <- tl[which(tlo <=1)]
      tli <- tl[which(tlo >1)]
      tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
      f1 <- c(f1, sapply(tli2, function(x) make_ints(x, o_form = f1, tl0 = tl0)))
    }
    ##
    if(!is.null(x$strata)){
      ft <- as.character(attr(x$terms, "predvars"))[-c(1:2)]
      ft2 <- ft[!grepl("strata", ft)]
      if(length(ft2) <1){
        return("1")
      }
      ## interactions
      f1 <- ft2
      tlo <- attr(x$terms, "order")
      if(any(tlo >1)){
        tl <- attr(x$terms, "term.labels")
        tl0 <- tl[which(tlo <=1)]
        tli <- tl[which(tlo >1)]
        tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
        f1 <- c(f1, sapply(tli2, function(x) make_ints(x, o_form = f1, tl0 = tl0)))
      }
    }
    f1
  })

  evtimes <- object$eventTimes

  xlevels <- lapply(object$models, `[[`, "xlevels")

  l <- list("cumhazards" = cumhazards,
            "hazards" = hazards,
            "eventTimes"  = evtimes,
            "Lastevent" = levT,
            "coef" = coefs,
            "cov" = vcovs,
            "xlevels" = xlevels,
            "formulas" = formulas
  )

  class(l) <- "burgle_CauseSpecificCox"
  l

}

predictCIF_cpp <- get("predictCIF_cpp", envir = asNamespace("riskRegression"), inherits = FALSE)

#' @name predict_burgle
#'
#' @param cause which cause do you want to predict
#'
#' @export
predict.burgle_CauseSpecificCox <- function(object, newdata = NULL, type = "lp", cause = 1, original = TRUE, draws = 1, sims = 1, times = NULL, ...) {
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

  if(type == "risk"){
    return(preds)
  }

  if(sims >= 1L & type == "response"){
    if(!is.null(dim(preds))){
      preds <- replicate(sims, apply(preds, MARGIN = 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)), simplify = FALSE)
      if(sims == 1L) preds <- preds[[1]]
    }else{
      preds <- lapply(preds, function(y) replicate(sims, apply(y, MARGIN = 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)), simplify = FALSE))
      if(sims < 2) preds <- lapply(preds, function(x) if(length(x) == 1) x[[1]] else x)
      }
  }
  preds

}

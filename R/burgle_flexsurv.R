#' @name burgle_
#'
#' @export
burgle.flexsurvreg <- function(object, ...){

  coef <- stats::coef(object)
  terms <- object$covdata$terms
  terms <- delete.response(terms)
  # ft <- as.character(attr(object$covdata$terms, "predvars"))[-c(1:2)]
  # if (length(ft) < 1) {
  #   formula <- "1"
  # }
  # else {
  #   formula <- ft
  # }
  # tlo <- attr(object$covdata$terms, "order")
  # if(any(tlo >1)){
  #   tl <- attr(object$covdata$terms, "term.labels")
  #   tl0 <- tl[which(tlo <=1)]
  #   tli <- tl[which(tlo >1)]
  #   tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
  #   formula <- c(formula, sapply(tli2, function(x) make_ints(x, o_form = formula, tl0 = tl0)))
  # }
  if (length(coef) == 0L) {
    cov <- matrix(0)
  }
  else {
    cov <- stats::vcov(object)
    if(any(is.na(cov))){
      warning("No covariance estimates found, predicting will only be done from the estimated model")
      cov <- matrix(0, nrow = length(coef), ncol = length(coef))
    }
  }

  pf <- object$dfns$p
  hz <- object$dfns$H
  qn <- object$dfns$q
  unq <- sort(unique(object$data$Y[,"time"]))
  xlevels <- object$covdata$xlev
  contrasts <- attr(object$data$mml$mu, "contrasts")
  inv_t <- object$dlist$inv.transforms
  pars_i <- object$basepars
  loc <- which(names(coef) == object$dlist$location)
  opars_i <- setdiff(pars_i, loc)

  l <- list(coef = coef, cov = cov,xlevels = xlevels, contrasts = contrasts, #formula = formula,
            terms = terms,
            p_f = pf, p_h = hz, p_q = qn,
            e_times = unq,
            inv.transforms = inv_t, pars_indeces = pars_i, location = loc, opars_indeces = opars_i)
  class(l) <- "burgle_flexsurvreg"
  l

}

#' @name predict_burgle
#' @importFrom stats setNames
#'
#' @export
predict.burgle_flexsurvreg <- function(object, newdata = NA, original = TRUE, draws = 1, sims = 1,
                                       type = "lp", times = NULL, ...){

  if (!is.data.frame(newdata))
    stop("newdata must be an object of class data.frame")
  nc <- names(object$coef)
  type <- match.arg(tolower(type), c("lp", "response", "risk", "time"))
  # nl <- names(object$xlevels)
  # ck0 <- nl %in% colnames(newdata)
  # if (!all(ck0))
  #   stop(paste(nl[!ck0], "is not present in newdata"))
  # ulv <- lapply(nl, function(x) unique(newdata[, x])[[1]])
  # ck1s <- mapply(function(x, y) (y %in% x), object$xlevels,
  #                ulv, SIMPLIFY = FALSE)
  # ck1 <- sapply(ck1s, all)
  # if (length(ck1) > 0L) {
  #   if (!all(ck1)) {
  #     obs <- min(which(!ck1))
  #     stop(paste0("variable ", names(object$xlevels)[obs],
  #                 " has new level(s) of ", paste(ulv[[obs]][!ck1s[[obs]]],
  #                                                collapse = ",")))
  #   }
  # }
  if (original & draws > 1) {
    stop("Can only have one draw from the original model")
  }
  if (original) {
    models <- object$coef
  }else {
    models <- MASS::mvrnorm(n = draws, mu = object$coef,
                            Sigma = object$cov)
  }

  if(draws == 1L){
    params <- models[object$pars_indeces]
    locs <- models[object$location]
    o_params <- models[object$opars_indeces]
    models <- models[-object$pars_indeces]
    if(length(o_params) > 0L) o_params <- mapply(function(x, y) y(x), o_params, object$inv.transforms[object$opars_indeces])
    if(length(models) == 0L){
      models <- 0L
    }
  }else{
    params <- models[,object$pars_indeces]
    locs <- models[,object$location]
    o_params <- models[,object$opars_indeces]
    models <- models[,-object$pars_indeces]
    if(is.null(dim(o_params))){
      o_params <- mapply(function(x, y) y(o_params[x]), 1:length(o_params), object$inv.transforms[object$opars_indeces])
    }else{
      o_params <- mapply(function(x, y) y(o_params[, x]), 1:ncol(o_params), object$inv.transforms[object$opars_indeces])
    }
    if(length(models) == 0L){
      models <- matrix(0, nrow = draws)
    }
  }

  # if(length(models) == 0L){
  #   matrix(0, nrow = nrow(params))
  # }
  # if()

  # mm <- stats::model.matrix(stats::reformulate(object$formula), data = newdata,
  #                           xlev = object$xlevels, contrasts.arg = object$contrasts)[,-1]
  mm <- stats::model.matrix(object$terms, data = newdata,
                            xlev = object$xlevels, contrasts.arg = object$contrasts)[,-1]

  if(length(mm) == 0L){
    mm <- matrix(0, nrow = nrow(newdata))
  }


  if(is.vector(mm)) {mm <- matrix(mm)}

  if (!is.null(dim(models))) {
    # preds <- apply(models, 1, function(x) mm %*% x)
    preds <- fastmm(mm, t(models))
  }else {
    # preds <- mm %*% models
    preds <- as.vector(fastmm(mm, matrix(models)))
  }
  if (type == "lp") {
    if (sims > 1L)
      warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  if(is.null(times) & type %in% c("response", "risk")){
    stop("times is missing")
  }

  ##location
  if(!is.null(dim(models))){
    preds <- mapply(function(x, y) preds[, x] + y, 1:length(locs), locs)
  }else{
    preds <- preds + locs
  }
  preds <- object$inv.transforms[[object$location]](preds)


  ## now adapt risk
  ## shape, scale need to be 1st and 2nd arguments... regardless of distribution
  if(draws == 1){
    # if(length(o_params) > 0L){
    #
    #
    #   ## I think I can simplyify this to below, need to double check with hazards
    #   # list_pr <- lapply(preds, function(x) list(unlist(o_params), unlist(x)))
    #   # list_pr <- lapply(list_pr, unlist)
    #   # list_pr <- lapply(list_pr, setNames, c(nc[object$opars_indeces], nc[object$location]))
    #
    #   # list_pr <- list(preds, unlist(o_params))
    #   list_pr <- append(as.list(o_params), list(p = preds))
    #   names(list_pr) <- c(nc[object$opars_indeces], nc[object$location])
    #
    # }else{
    #
    #   list_pr <- lapply(preds, function(x) list(x))
    #   list_pr <- lapply(list_pr, unlist)
    #   list_pr <- lapply(list_pr, setNames, c(nc[object$location]))
    # }
    list_pr <- append(as.list(o_params), list(p = preds))
    names(list_pr) <- c(nc[object$opars_indeces], nc[object$location])
    if(type == "time"){
      ps <- runif(n = nrow(newdata))
      list_pr <- append(list_pr, list(p = ps))
      # qp <-
      ste <- do.call(object$p_q, list_pr)
      return(ste)

    }

    pr0 <- sapply(times, function(y){
      list_pr_x <- append(list_pr, list(x = y))
      pr00 <- do.call(object$p_h, list_pr_x)
      pr00
    })
    # pr0 <- do.call(object$p_h, list_pr)
    pr0 <- 1-exp(-pr0)

    if(nrow(pr0) == 1L) pr0 <- t(pr0)
  }else{
    if(length(o_params) > 0L){
      if(is.null(dim(o_params))){

        list_pr <- lapply(1:draws, function(x) append(as.list(o_params[x]), list(p = preds[, x])))

      }else{

        list_pr <- lapply(1:draws, function(x) append(as.list(o_params[x,]), list(p = preds[, x])))

         }

      list_pr <- lapply(list_pr, setNames, c(nc[object$opars_indeces], nc[object$location]))

    }else{

      list_pr <- lapply(1:draws, function(x) list(p = preds[, x]))
      list_pr <- lapply(list_pr, setNames, c(nc[object$location]))

    }
    # object$p_h
    # pr0 <- lapply(list_pr, function(y) t(sapply(y, function(x) do.call(flexsurv_risk, x))))
    if(type == "time"){

      ## I think this is permissable, you either do that or do it 3 times, depending on what Jarrod wants, if this the case you just need to move
      ## ps into the lapply
      ps <- runif(n = nrow(newdata))
      ste <- lapply(list_pr, function(x){
        # ps <- runif(n = nrow(newdata))
        list_pr_x <- append(x, list(p = ps))
        ste1 <- do.call(qp, list_pr_x)
        ste1
      })

      return(ste)

    }

    pr0 <- lapply(list_pr, function(z){
      sapply(times, function(y){
      list_pr_x <- append(z, list(x = y))
      pr00 <- do.call(object$p_h, list_pr_x)
      pr00
      }
      )
    })

    # pr0 <- sapply(times, function(y){
    #   list_pr_x <- append(list_pr, list(x = y))
    #   pr00 <- do.call(object$p_h, list_pr_x)
    #   pr00
    # })
    # pr0 <- do.call(object$p_h, list_pr)
    # pr0 <- 1-exp(-pr0)

    # list_pr <- lapply(list_pr, lapply, append, list(x = times), 1)
    # pr0 <- lapply(list_pr, function(y) t(sapply(y, function(x) do.call(object$p_h, x))))
    pr0 <- lapply(pr0, `row.names<-`, NULL)
    pr0 <- lapply(pr0, function(z) 1-exp(-z))
    if(nrow(pr0[[1]] == 1L))  pr0 <- lapply(pr0, t)
  }
  # pr0 <- exp(-pr0)


  if(type == "risk"){
    return(pr0)
  }

  if (sims >= 1 & type == "response") {
    if (!is.null(dim(pr0))) {

      pn <- simulate_responses_binom(pr0, sims)

      if(sims < 2) pn <- pn[[1]]
    } else {

      pn <- lapply(pr0, simulate_responses_binom, sims = sims)

      if(sims < 2) pn <- lapply(pn, function(x) if(length(x) == 1) x[[1]] else x)
    }
  }

  pn

}

flexsurv_risk <- function(f, t, start = 0, ...){
  dots <- list(...)
  r <- 1-((1 - f(t, ...))/(1 - f(start, ...)))
  r
}

## that's easy to implement
flexsurv_ptime <- function(fp, fq, t, start = 0, ...){

  dots <- list(...)
  sp <- fp(start, ...)
  qu_f_start <- sp + (1- sp)*t
  q1 <- fq(qu_f_start, ...)

  q1
}
### test this tomroorow

#unq <- sort(unique(object$data$Y[,"time"]))

# haxzs <-

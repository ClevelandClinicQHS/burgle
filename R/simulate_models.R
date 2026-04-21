#' Simulate Models
#'
#' Simulating from burgle_ objects and draw_models()
#'
#' @name simulate_models
#'
#' @param object the results of burgle_* object
#' @param models a matrix or vector of model coefficeints, one can specifically it or get from `draw_models()`
#' @param newdata new data of class data.frame
#' @param sims how many simulated response to draw
#' @param type either 'lp', 'response', 'link' for glm or 'risk' if time dependent
#' @param seed seed to set for reproducibility
#' @param ... arguemnts for specific methods
#'
simulate_models <- function(object, models, newdata, type = "lp", sims = 1, seed = NULL, ...){
  UseMethod("simulate_models")
}

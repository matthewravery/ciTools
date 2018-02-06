# Copyright (C) 2017 Institute for Defense Analyses
#
# This file is part of ciTools.
#
# ciTools is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ciTools is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ciTools. If not, see <http://www.gnu.org/licenses/>.

#' Confidence Intervals for Quantile Regression Models
#'
#' This function is one of the methods in \code{add_ci} and
#' automatically is called when an object of class \code{rq} is passed
#' to \code{add_ci}.
#'
#' Confidence intervals for \code{rq} objects are calculated
#' parametrically. This function is essentially a wrapper for
#' \code{predict(fit, tb, interval = "confidence")} if \code{fit} is a
#' quantile regresion model. Note that while we still refer to these values 
#' as confidence intervals, they may be better thought of as 
#' tolerance intervals. See "Confidence Intervals vs. Tolerance Intervals"
#' (Gocka, 1973) for a discussion of the distinction.
#' 
#' Unlike other objects (e.g., \code{lm}), methods for \code{add_pi},
#' \code{add_quantile} and \code{add_probs}, are not available for `
#' \code{rq} objects. It isn't clear what these quantities would be in the 
#' case of quantile regression, so we leave them undefined. 
#' 
#'
#' @param tb A tibble or data frame.
#' @param fit An object of class \code{rq}. Predictions are made with this
#'     object.
#' @param alpha A real number between 0 and 1. Controls the confidence
#'     level of the interval estimates.
#' @param names \code{NULL} or character vector of length two. If
#'     \code{NULL}, confidence bounds automatically will be named by
#'     \code{add_ci}, otherwise, the lower confidence bound will be
#'     named \code{names[1]} and the upper confidence bound will be
#'     named \code{names[2]}.
#' @param yhatName A string. Name of the vector of the predictions
#'     made for each observation in tb
#' @param ... Additional arguments.
#' @return A tibble, \code{tb}, with predicted values, upper and lower
#'     confidence bounds attached.
#'
#' @seealso \code{\link{quantreg::rq}} for a description of these types
#'     of objects, their use, and their arguments.
#'
#' @examples
#' # Fit a linear model
#' fit <- rq(dist ~ speed, data = cars, tau = .7)
#' # Get fitted values for each observation in cars, and append
#' # confidence intervals
#' add_ci(cars, fit)
#' # Try a different confidence level
#' add_ci(cars, fit, alpha = 0.5)
#' # Try custom names for the confidence bounds 
#' add_ci(cars, fit, alpha = 0.5, names = c("lwr", "upr"))
#' 
#' @export

add_ci.rq <- function(tb, fit, alpha = 0.05, names = NULL, yhatName = "pred", ...){
    
  if (is.null(names)){
    names[1] <- paste("LCB", alpha/2, sep = "")
    names[2] <- paste("UCB", 1 - alpha/2, sep = "")
  }
  if ((names[1] %in% colnames(tb))) {
    warning ("These CIs may have already been appended to your dataframe. Overwriting.")
  }
  out <- predict(fit, tb, interval = "confidence", level = 1 - alpha)
  
  if(is.null(tb[[yhatName]]))
    tb[[yhatName]] <- out[, 1]
  tb[[names[1]]] <- out[, 2]
  tb[[names[2]]] <- out[, 3]
  tibble::as_data_frame(tb)
  
     
}

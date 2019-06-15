# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class SignalDerivation (R6) ####

#' @export
#' 
#' @title 
#'   Derives data from a multivariate signal
#'
#' @description 
#'   This defines an abstract interface to a signal derivation. A signal derivation
#'   is an analysis of a signal at a single location. Parameters listed are for the
#'   constructor method ($new()).
#'   
#' @param signal
#'   An optional argument allowing the derivation to be tied to a specific signal,
#'   if desired.
#' 
#' @section Abstract Methods:
#'   \itemize{
#'     \item \code{$derive} - See \code{\link{SignalDerivation_derive}}
#'   }
#'     
SignalDerivation <- R6Class(
   classname = "SignalDerivation",
   public = list(
      signal = NULL,
      initialize = function(signal = NULL)
         {
            self$signal = signal;   
         }
      )
);

# Abstract method SignalDerivation$derive ####

#' @name SignalDerivation_derive
#' 
#' @title 
#'   Performs a signal derivation
#'   
#' @param signal
#'   The signal on which the derivation is performed.
#'   This parameter is only required if the SignalDerivation object is
#'   not already associated with a signal (see \code{\link{SignalDerivation}})
#' @param prevResults
#'   Object representing the results from a previous derivation (if applicable)
#' @param path
#'   The path where the results file is written
#' 
#' @section Abstract method of interface:
#'   \code{\link{SignalDerivation}}
#'
NULL

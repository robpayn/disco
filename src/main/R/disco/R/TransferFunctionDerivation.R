# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class TransferFunctionDerivation (R6) ####

#' @export
#' 
#' @title 
#'   Derives a transfer function from input and output signals
#'
#' @description 
#'   This defines an abstract interface to a transfer function derivation. 
#'   A transfer function derivation is an analysis of the changes made between
#'   an input and outpu signal. Parameters listed are for the
#'   constructor method ($new()).
#'   
#' @param signalIn
#'   An optional argument allowing the derivation to be tied to a specific input signal,
#'   if desired.
#' @param signalOut
#'   An optional argument allowing the derivation to be tied to a specific output signal,
#'   if desired.
#' 
#' @section Abstract Methods:
#'   \itemize{
#'     \item \code{$derive} - See \code{\link{TransferFunctionDerivation_derive}}
#'   }
#'     
TransferFunctionDerivation <- R6Class(
   classname = "TransferFunctionDerivation",
   public = list(
      signalIn = NULL,
      signalOut = NULL,
      initialize = function
         (
            signalIn = NULL,
            signalOut = NULL
         )
         {
            self$signalIn = signalIn;   
            self$signalOut = signalOut;
         }
      )
);
 
# Abstract method TransferFunctionDerivation$derive ####

#' @name TransferFunctionDerivation_derive
#' 
#' @title 
#'   Performs a transfer function derivation
#'   
#' @param signalIn
#'   The input signal on which the derivation is performed.
#'   This parameter is only required if the TransferFunctionDerivation object is
#'   not already associated with an input signal 
#'   (see \code{\link{TransferFunctionDerivation}})
#' @param signalOut
#'   The output signal on which the derivation is performed.
#'   This parameter is only required if the TransferFunctionDerivation object is
#'   not already associated with an output signal 
#'   (see \code{\link{TransferFunctionDerivation}})
#' @param prevResults
#'   Object representing the results from a previous derivation (if applicable)
#' @param path
#'   The path where the results file is written
#' 
#' @section Abstract method of interface:
#'   \code{\link{TransferFunctionDerivation}}
#'
NULL

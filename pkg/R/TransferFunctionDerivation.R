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
#'   R6 Class defining a transfer function derivation
#'
#' @description 
#'   This defines an abstract interface to a transfer function derivation. 
#'   A transfer function derivation is an analysis of the changes made between
#'   an input and outpu signal.
#'   
TransferFunctionDerivation <- R6Class(
   classname = "TransferFunctionDerivation",
   public = list(
      
      #' @field signalIn
      #'   The input signal providing the basis for the derivation
      signalIn = NULL,
      
      #' @field signalOut
      #'   The output signal providing the basis for the derivation
      signalOut = NULL,
      
      # Method TransferFunctionDerivation$new ####
      #
      #' @description 
      #'   Constructs a new instance of class TransferFunctionDerivation
      #'   
      #' @param signalIn
      #'   An optional argument allowing the derivation to be tied to a specific input signal,
      #'   if desired.
      #' @param signalOut
      #'   An optional argument allowing the derivation to be tied to a specific output signal,
      #'   if desired.
      #' 
      initialize = function
      (
         signalIn = NULL,
         signalOut = NULL
      )
      {
         self$signalIn = signalIn;   
         self$signalOut = signalOut;
      },

      # Abstract method TransferFunctionDerivation$derive ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
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
      #' @param index
      #'   Index of the derivation being performed
      #' @param ...
      #' 
      #' @return 
      #'   An object representing the results of the derivation
      #'   
      derive = function(...)
      {
         stop("TransferFunctionDerivation$derive has not been implemented.")
      }
      
   )
)

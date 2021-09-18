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
#'   R6 class defining a signal derivation
#'
#' @description 
#'   This defines an abstract interface to a signal derivation. A signal derivation
#'   is an analysis of a signal at a single location.
#'   
SignalDerivation <- R6Class(
   classname = "SignalDerivation",
   public = list(
      
      #' @field signal
      #'   The signal providing the basis for the derivation
      signal = NULL,
      
      # Method SignalDerivation$new ####
      #
      #' @description 
      #'   Creates a new instance of the class SignalDerivation
      #'   
      #' @param signal
      #'   An optional argument allowing the derivation to be tied to a specific signal,
      #'   if desired.
      #' 
      initialize = function(signal = NULL)
      {
         self$signal = signal;   
      },
      
      # Abstract method SignalDerivation$derive ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Performs a signal derivation
      #'   
      #' @param signal
      #'   The signal on which the derivation is performed.
      #'   This parameter should only be required if the SignalDerivation object is
      #'   not already associated with a signal.
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
         stop("SignalDerivation$derive has not been implemented.")
      }
      
   )
)

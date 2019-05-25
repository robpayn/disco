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
 
SignalDerivation$set(
   which = "public",
   name = "derive",
   value = function(signal, prevResults, path)
      {
         stop(paste(
            "Abstract method SignalDerivation$derive has not been", 
            "implemented by inheriting class."
         ));
      }
);

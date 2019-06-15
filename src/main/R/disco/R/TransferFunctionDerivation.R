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
#'   Derives data from a multivariate signal
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
 
TransferFunctionDerivation$set(
   which = "public",
   name = "derive",
   value = function
      (
         signalIn,
         signalOut,
         prevResults, 
         path
      )
      {
         stop(paste(
            "Abstract method SignalDerivation$derive has not been", 
            "implemented by inheriting class."
         ));
      }
);

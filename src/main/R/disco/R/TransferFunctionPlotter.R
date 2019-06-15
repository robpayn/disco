# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class TransferFunctionPlotter (R6) ####

#' @export
#' 
#' @title 
#'   Plots a multivariate signal
#'
#' @description 
#'   Abstract class for providing visualizations of a multivariate
#'   signal.
#'   This class provides the definition of an interface to a
#'   TransferFunctionPlotter object and is not intended to be instantiated
#'   directly.
#' 
#' @usage 
#'   TransferFunctionPlotter$new()
#' @param signalIn
#'   Optional input signal associated with the plotter a priori.
#'   Defaults to NULL.
#' @param signalOut
#'   Optional output signal associated with the plotter a priori.
#'   Defaults to NULL.
#' @param outputPath
#'   Optional path to results from a dignal analysis.
#'   Defaults to NULL.
#' 
#' @return 
#'   An instantiation of a TransferFunctionPlotter object
#' 
#' @section Methods:
#'   $new
#'   $plot - see \code{\link{TransferFunctionPlotter_plot}}  
TransferFunctionPlotter <- R6Class(
   classname = "TransferFunctionPlotter",
   public = list(
      signalIn = NULL,
      signalOut = NULL,
      outputPath = NULL,
      initialize = function
         (
            signalIn = NULL,
            signalOut = NULL,
            outputPath = NULL
         )
         {
            self$signalIn <- signalIn;
            self$signalOut <- signalOut;
            self$outputPath <- outputPath;
         }
      )
);

# Method TransferFunctionPlotter$plot ####

#' @name TransferFunctionPlotter_plot
#' 
#' @title 
#'   Plots a multivariate signal
#' 
#' @description 
#'   Provides a default visualization of a multivariate signal
#' 
#' @param signalIn
#'   A multivariate input signal of type \code{\link{Signal}}
#' @param signalOut
#'   A multivariate output signal of type \code{\link{Signal}}
#' @param outputPath
#'   A path to results from a signal analysis
#' @param label
#'   A label for the plot
#' @param timeBounds  
#'   The bounds to use for the time axis on the plot
#'   
#' @return 
#'   No designed return value
#' 
#' @section Abstract method of class:
#'   \code{\link{TransferFunctionPlotter}}  
TransferFunctionPlotter$set(
   which = "public",
   name = "plot",
   value = function
      (
         signalIn = NULL,
         signalOut = NULL,
         outputPath = NULL, 
         label, 
         timeBounds
      )
      {
         stop(paste(
            "Abstract method TransferFunctionPlotter$plot has not been", 
            "implemented by inheriting class."
         ));
      }
);

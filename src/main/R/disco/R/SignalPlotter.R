# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class SignalPlotter (R6) ####

#' @export
#' 
#' @title 
#'   Plots a multivariate signal
#'
#' @description 
#'   Abstract class for providing visualizations of a multivariate
#'   signal.
#'   This class provides the definition of an interface to a
#'   SignalPlotter object and is not intended to be instantiated
#'   directly.
#' 
#' @usage 
#'   SignalPlotter$new()
#' @param signal
#'   Optional signal associated with the plotter a priori.
#'   Defaults to NULL.
#' @param outputPath
#'   Optional path to results from a dignal analysis.
#'   Defaults to NULL.
#' 
#' @return 
#'   An instantiation of a SignalPlotter object
#' 
#' @section Methods:
#'   $new
#'   $plot - see \code{\link{SignalPlotter_plot}}  
SignalPlotter <- R6Class(
   classname = "SignalPlotter",
   public = list(
      signal = NULL,
      outputPath = NULL,
      initialize = function
         (
            signal = NULL, 
            outputPath = NULL
         )
         {
            self$signal <- signal;
            self$outputPath <- outputPath;
         }
      )
);

# Method SignalPlotter$plot ####

#' @name SignalPlotter_plot
#' 
#' @title 
#'   Plots a multivariate signal
#' 
#' @description 
#'   Provides a default visualization of a multivariate signal
#' 
#' @param signal
#'   A multivariate signal of type \code{\link{Signal}}
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
#'   \code{\link{SignalPlotter}}  
SignalPlotter$set(
   which = "public",
   name = "plot",
   value = function
      (
         signal = NULL, 
         outputPath = NULL, 
         label, 
         timeBounds
      )
      {
         stop(paste(
            "Abstract method SignalPlotter$plot has not been", 
            "implemented by inheriting class."
         ));
      }
);

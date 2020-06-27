# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# SignalInterface R6 Interface ####

#' @export
#'
#' @title
#'   R6 abstract class defining a signal
#' 
#' @description 
#'   Defines an abstract interface for an R6 class that manages
#'   a multivariate time series data set. Full functional implementations
#'   of this interface must implement the functions described below.
#'
SignalInterface <- R6Class(
   classname = "SignalInterface",
   public = list(
      
      # Abstract method SignalInterface$getWindow ####
      #
      #' @description
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Method for getting a subset of a signal. The time
      #'   stamps included in the window should start on or after
      #'   the minimum time and before or on the maximum time.
      #' 
      #' @param minTime
      #'   Time defining the beginning of the window
      #' @param maxTime
      #'   Time defining the end of the window
      #' @param ...
      #'
      #' @return
      #'   A subset of the signal as another SignalInterface object
      #'   
      getWindow = function(...)
      {
         stop("SignalInterface$getWindow has not been implemented.")
      },

      # Abstract method SignalInterface$getVariable ####
      #
      #' @description
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Gets the vector of values for a given variable
      #'   in the multivariate signal
      #' 
      #' @param variableName
      #'   The name of the variable
      #' @param ...
      #' 
      getVariable = function(...)
      {
         stop("SignalInterface$getVariable has not been implemented.")
      },
      
      # Abstract method SignalInterface$plotSummary ####
      #
      #' @description
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Plot a summary of the variables in the signal
      #' 
      #' @param x
      #'   X-axis variable
      #' @param mfrow
      #'   Plot layout (see \code{\link{par}})
      #' @param mar
      #'   Plot margins (see \code{\link{par}})
      #' @param ...
      #'   
      plotSummary = function(...)
      {
         stop("SignalInterface$plotSummary has not been implemented.")
      },
      
      # Abstract method SignalInterface$plot ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Plot a single variable in the signal
      #'   
      #' @param variableName
      #'   The variable header to plot
      #' @param ...
      #'   
      plot = function(...)
      {
         stop("SignalInterface$plot has not been implemented.")
      },

      # Abstract method SignalInterface$addVariable ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Add a variable to a signal
      #'   
      #' @param variableHeader
      #'   The header for the property name for the new variable
      #' @param metadata
      #'   The metadata for the column
      #' @param values
      #'   The vector of values for the new column
      #' @param ...
      #' 
      addVariable = function(...)
      {
         stop("SignalInterface$addVariable has not been implemented.")
      },
      
      # Abstract method SignalInterface$writeCSV ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Write the signal in csv format
      #'   
      #' @param path
      #'   Path to the CSV files that will be created
      #' @param name
      #'   Name base for the files
      #' @param timeVariableName
      #'   Header to use for the time column
      #' @param variables
      #'   Vector of variable names to include in output
      #' @param ...
      #'   
      writeCSV = function(...)
      {
         stop("SignalInterface$writeCSV has not been implemented.")
      },

      # Abstract method SignalInterface$interpolate ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Creates a new signal by interpolating between points of
      #'   this signal based on a provided vector of times
      #'   
      #' @param time
      #'   Vector of times at which the signal should be interpolated
      #' @param ...
      #'   
      interpolate = function(...)
      {
         stop("SignalInterface$interpolate has not been implemented.")
      }
      
   )
)

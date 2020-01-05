# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Signal R6 Interface ####

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
Signal <- R6Class(
   classname = "Signal",
   public = list(
      
      #' @field time
      #'   A vector representing the times for the elements of the signal
      time = NULL,
      
      # Abstract method Signal$getWindow ####
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
      #'   A subset of the signal as a SignalDataFrame object
      #'   
      getWindow = function(...)
      {
         stop("Signal$getWindow has not been implemented.")
      },

      # Abstract method Signal$getVariable ####
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
         stop("Signal$getVariable has not been implemented.")
      },
      
      # Abstract method Signal$plotSummary ####
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
         stop("Signal$plotSummary has not been implemented.")
      },
      
      # Abstract method Signal$plot ####
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
         stop("Signal$plot has not been implemented.")
      },

      # Abstract method Signal$addVariable ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Add a variable to a signal
      #'   
      #' @param variableName
      #'   The name of the variable (will be used as the header)
      #' @param value
      #'   The vector of values for the vector.
      #'   Must match the length of the signal
      #' @param units
      #'   The units of the values.
      #' @param dimensions
      #'   The dimensions of the property
      #' @param ...
      #' 
      addVariable = function(...)
      {
         stop("Signal$addVariable has not been implemented.")
      },
      
      # Abstract method Signal$writeCSV ####
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
         stop("Signal$writeCSV has not been implemented.")
      },

      # Abstract method Signal$interpolate ####
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
         stop("Signal$interpolate has not been implemented.")
      }
      
   )
)

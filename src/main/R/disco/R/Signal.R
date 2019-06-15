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
#'   Interface for a signal
#' 
#' @description 
#'   Defines an abstract interface for an R6 class that manages
#'   a multivariate time series data set. Full functional implementations
#'   of this interface must implement the functions described below.
#'
#' @section Abstract Methods:
#'   \itemize{
#'     \item \code{$getWindow} - See \code{\link{Signal_getWindow}}
#'     \item \code{$getVariable} - See \code{\link{Signal_getVariable}}
#'     \item \code{$addVariable} - See \code{\link{Signal_addVariable}}
#'     \item \code{$writeCSV} - See \code{\link{Signal_writeCSV}}
#'     \item \code{$interpolate} - See \code{\link{Signal_interpolate}}
#'     \item \code{$plotSummary} - See \code{\link{Signal_plotSummary}}
#'     \item \code{$plot} - See \code{\link{Signal_plot}}
#'   }
#'
Signal <- R6Class(
   classname = "Signal",
   public = list(
      time = NULL
      )
);


# Abstract method Signal$getWindow ####

#' @name Signal_getWindow
#'
#' @title
#'   Gets a subset of a signal based on a time window
#'
#' @description
#'   Method for getting a subset of a signal. The time
#'   stamps included in the window should start on or after
#'   the minimum time and before or on the maximum time.
#' 
#' @param minTime
#'   Time defining the beginning of the window
#' @param maxTime
#'   Time defining the end of the window
#'
#' @return
#'   A subset of the signal as a SignalDataFrame object
#'
#' @section Abstract method of interface:
#'   \code{\link{Signal}}
#'
NULL

# Abstract method Signal$getVariable ####

#' @name Signal_getVariable
#'
#' @title
#'   Gets a variable vector
#' 
#' @description
#'   Gets the vector of values for a given variable
#'   in the multivariate signal
#' 
#' @param variableName
#'   The name of the variable
#'
#' @section Method of class:
#'   \code{\link{Signal}}
#'
NULL

# Abstract method Signal$plotSummary ####

#' @name Signal_plotSummary
#'
#' @title
#'   Plots a signal summary
#' 
#' @description
#'   Plot a summary of the variables in the signal
#' 
#' @param x
#'   X-axis variable
#' @param mfrow
#'   Plot layout (see \code{\link{par}})
#' @param mar
#'   Plot margins (see \code{\link{par}})
#'
#' @section Method of class:
#'   \code{\link{Signal}}
#'
NULL

# Abstract method Signal$plot ####

#' @name Signal_plot
#' 
#' @title 
#'   Plot a single variable in the signal
#'   
#' @param variableName
#'   The variable header to plot
#' @param ...
#'   Parameters to be passed on to the plot function
#'   See \code{\link{plot.default}}
#'     
#' @section Method of class:
#'   \code{\link{Signal}}
#'
NULL

# Abstract method Signal$addVariable ####

#' @name Signal_addVariable
#' 
#' @title 
#'   Add a variable to a signal
#'   
#' @param property
#'   The property of the variable (will be used as the header)
#' @param value
#'   The vector of values for the vector.
#'   Must match the length of the signal
#' @param units
#'   The units of the values.
#' @param dimensions
#'   The dimensions of the property
#' 
#' @section Method of class:
#'   \code{\link{Signal}}
#'
NULL

# Abstract method Signal$writeCSV ####

#' @name Signal_writeCSV
#' 
#' @title 
#'   Write the signal in csv format
#'   
#' @param path
#'   Path to the CSV files that will be created
#' @param name
#'   Name base for the files
#' @param timeVariableName
#'   Header to use for the time column
#'   
#' @section Method of class:
#'   \code{\link{Signal}}
#'
NULL

# Abstract method Signal$interpolate ####

#' @name Signal_interpolate
#' 
#' @title 
#'   Interpolates a signal
#'   
#' @description 
#'   Creates a new signal by interpolating between points of
#'   this signal based on a provided vector of times
#'   
#' @param time
#'   Vector of times at which the signal should be interpolated
#'   
#' @section Method of class:
#'   \code{\link{Signal}}
#'
NULL
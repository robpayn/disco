# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class SignalSummarizer (R6) ####

#' @export
#' 
#' @title 
#'   A summarizer for a signal analysis
#'   
#' @section Abstract Methods:
#'   \itemize{
#'     \item \code{$open} - 
#'       See \code{\link{SignalSummarizer_open}}
#'     \item \code{$summarize} - 
#'       See \code{\link{SignalSummarizer_summarize}}
#'     \item \code{$close} - 
#'       See \code{\link{SignalSummarizer_close}}
#'   }
#'   
SignalSummarizer <- R6Class(
   classname = "SignalSummarizer"
);

# Abstract method SignalSummarizer$open ####

#' @name SignalSummarizer_open
#' 
#' @title 
#'   Open the summarizer
#' 
#' @description 
#'   Performs an initial operations necessary to get the summarizer
#'   ready for multiple calls to method summarize.
#'   
#' @param path
#'   The path to the summarizer output
#'   
#' @section Method of class:
#'   \code{\link{SignalSummarizer}}
#'   
NULL


# Abstract method SignalSummarizer$summarize ####

#' @name SignalSummarizer_summarize
#' 
#' @title 
#'   Summarizes a signal analysis
#' 
#' @description 
#'   Perform the summary on a provided signal
#' 
#' @param signal
#'   The signal object to be analyzed
#' @param outputPath
#'   A path to results from a signal analysis.
#'   Not necessary if summarizer does not need output.
#' @param label
#'   A label for the summary
#' @param timeBounds  
#'   The temporal bounds on the summary
#'   
#' @section Method of class:
#'   \code{\link{SignalSummarizer}}
#'   
NULL

# Abstract method SignalSummarizer$close ####

#' @name SignalSummarizer_close
#' 
#' @title 
#'   Closes the summarizer
#'   
#' @description 
#'   Perform any operations necesary to close the summarizer
#'   and write any final output.
#' 
#' @section Method of class:
#'   \code{\link{SignalSummarizer}}
#'   
NULL
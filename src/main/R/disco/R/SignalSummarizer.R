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
#'   R6 class defining a signal summarizer
#' 
#' @description 
#'   Summarizes a signal and possibly its analysis
#'   
SignalSummarizer <- R6Class(
   classname = "SignalSummarizer",
   public = list(
      # Abstract method SignalSummarizer$open ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Performs an initial operations necessary to get the summarizer
      #'   ready for multiple calls to method summarize.
      #'   
      #' @param path
      #'   The path to the summarizer output
      #' @param ...
      #'   
      #' @return 
      #'   Return value not required.
      #'   
      open = function(...)
      {
         stop("SignalSummarizer$open has not been implemented.")
      },
      
      # Abstract method SignalSummarizer$summarize ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
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
      #' @param ...
      #'   
      #' @return 
      #'   Return value not required.
      #'   
      summarize = function(...)
      {
         stop("SignalSummarizer$summarize has not been implemented.")
      },
      
      # Abstract method SignalSummarizer$close ####
      #
      #' @description 
      #'   This is an abstract declaration and the method
      #'   must be implemented by extending classes.
      #'   
      #'   Perform any operations necesary to close the summarizer
      #'   and write any final output.
      #' 
      #' @param ...
      #' 
      #' @return 
      #'   Return value not required.
      #'   
      close = function(...)
      {
         stop("SignalSummarizer$close has not been implemented.")
      }
      
   )
)

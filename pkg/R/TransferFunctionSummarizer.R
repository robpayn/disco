# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class TransferFunctionSummarizer (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining a transfer function summarizer
#'   
#' @description 
#'   Summarizes a transfer function and possibly its analysis
#'   
TransferFunctionSummarizer <- R6Class(
   classname = "TransferFunctionSummarizer",
   public = list(
      
      # Abstract method TransferFunctionSummarizer$open ####
      #
      #' @description 
      #'   Performs an initial operations necessary to get the summarizer
      #'   ready for multiple calls to method summarize.
      #'   
      #' @param path
      #'   The path to the summarizer output
      #' @param ...
      #'   
      open = function(...)
      {
         stop("TransferFunctionSummarizer$open has not been implemented.")
      },

      # Abstract method TransferFunctionSummarizer$summarize ####
      #
      #' @description 
      #'   Perform the summary on an analysis of a provided 
      #'   input and output signal
      #' 
      #' @param signalIn
      #'   A multivariate input signal of type \code{\link{Signal}}
      #' @param signalOut
      #'   A multivariate output signal of type \code{\link{Signal}}
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
         stop("TransferFunctionSummarizer$summarize has not been implemented.")
      },
      
      # Abstract method TransferFunctionSummarizer$close ####
      #
      #' @description 
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
         stop("TransferFunctionSummarizer$close has not been implemented.")
      }
      
   )
)


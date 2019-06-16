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
#'   A summarizer for a transfer function analysis
#'   
#' @section Abstract Methods:
#'   \itemize{
#'     \item \code{$open} - 
#'       See \code{\link{TransferFunctionSummarizer_open}}
#'     \item \code{$summarize} - 
#'       See \code{\link{TransferFunctionSummarizer_summarize}}
#'     \item \code{$close} - 
#'       See \code{\link{TransferFunctionSummarizer_close}}
#'   }
#'   
TransferFunctionSummarizer <- R6Class(
   classname = "TransferFunctionSummarizer"
);

# Abstract method TransferFunctionSummarizer$open ####

#' @name TransferFunctionSummarizer_open
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
#'   \code{\link{TransferFunctionSummarizer}}
#'   
NULL

# Abstract method TransferFunctionSummarizer$summarize ####

#' @name TransferFunctionSummarizer_summarize
#' 
#' @title 
#'   Summarizes a transfer function analysis
#' 
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
#'   
#' @section Method of class:
#'   \code{\link{TransferFunctionSummarizer}}
#'   
NULL

# Abstract method TransferFunctionSummarizer$close ####

#' @name TransferFunctionSummarizer_close
#' 
#' @title 
#'   Closes the summarizer
#'   
#' @description 
#'   Perform any operations necesary to close the summarizer
#'   and write any final output.
#' 
#' @section Method of class:
#'   \code{\link{TransferFunctionSummarizer}}
#'   
NULL
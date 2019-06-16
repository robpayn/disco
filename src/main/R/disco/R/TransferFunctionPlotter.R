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
#'   Plots a transfer function analysis summary
#'
#' @description 
#'   Abstract class for providing visualizations of the 
#'   results of a tranfer function analysis.
#'   This class provides the definition of an interface to a
#'   TransferFunctionPlotter object and is not intended to be instantiated
#'   directly.
#' 
#' @param signalIn
#'   Optional input signal associated with the plotter a priori.
#'   Defaults to NULL.
#' @param signalOut
#'   Optional input signal associated with the plotter a priori.
#'   Defaults to NULL.
#' @param outputPath
#'   Optional path to results from a dignal analysis.
#'   Defaults to NULL.
#' @param fileName
#'   The file name for the pdf file generated
#'   Defaults to "windowSummary.pdf"
#' @param mfrow
#'   The dimension of panels for the figure (see \code{\link{par}})
#'   Defaults to c(3,2), or 3 rows and 2 columns of panels per figure.
#' @param mar
#'   The margins of each panel in the figure (see \code{\link{par}})
#'   Defaults to c(4, 4, 2, 4) + 0.1
#' 
#' @section Implements interface \code{\link{TransferFunctionSummarizer}}:
#'   \code{$open}
#'   \itemize{
#'     \item see \code{\link{TransferFunctionSummarizer_open}}
#'     \item see \code{\link{TransferFunctionPlotter_open}}
#'   }
#'   \code{$summarize}
#'   \itemize{
#'     \item see \code{\link{TransferFunctionSummarizer_summarize}}
#'     \item see \code{\link{TransferFunctionPlotter_summarize}}
#'   }
#'   \code{$close}
#'   \itemize{
#'     \item see \code{\link{TransferFunctionSummarizer_close}}
#'     \item see \code{\link{TransferFunctionPlotter_close}}
#'   }
#' 
TransferFunctionPlotter <- R6Class(
   classname = "TransferFunctionPlotter",
   inherit = TransferFunctionSummarizer,
   public = list(
      signalIn = NULL,
      signalOut = NULL,
      outputPath = NULL,
      fileName = NULL,
      mfrow = NULL,
      mar = NULL,
      initialize = function
         (
            signalIn = NULL,
            signalOut = NULL,
            outputPath = NULL,
            fileName = "windowSummary.pdf",
            mfrow = c(3,2),
            mar = c(4, 4, 2, 4) + 0.1
         )
         {
            self$signalIn <- signalIn;
            self$signalOut <- signalOut;
            self$outputPath <- outputPath;
            self$fileName <- fileName;
            self$mfrow <- mfrow;
            self$mar <- mar;
         }
      )
);

# Method TransferFunctionPlotter$open ####

#' @name TransferFunctionPlotter_open
#' 
#' @title 
#'   Opens the tranfer function plotter
#'   
#' @description 
#'   Opens the PDF graphics device on the provided path
#' 
#' @section Method of class:
#'   \code{\link{TransferFunctionPlotter}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{TransferFunctionSummarizer_open}} -
#'     See interface for further usage documentation
#'   
TransferFunctionPlotter$set(
   which = "public",
   name = "open",
   value = function
   (
      path
   )
   {
      pdf(file = sprintf(
         fmt = "%s/%s",
         path,
         self$fileName
      ));
      par(
         mfrow = self$mfrow,
         mar = self$mar
      );      
   }
);

# Method TransferFunctionPlotter$close ####

#' @name TransferFunctionPlotter_close
#' 
#' @title 
#'   Closes the transfer function plotter
#'   
#' @description 
#'   Closes the graphics device to write the PDF file
#' 
#' @section Method of class:
#'   \code{\link{TransferFunctionPlotter}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{TransferFunctionSummarizer_close}} -
#'     See interface for further usage documentation
#'   
TransferFunctionPlotter$set(
   which = "public",
   name = "close",
   value = function()
   {
      dev.off();      
   }
);

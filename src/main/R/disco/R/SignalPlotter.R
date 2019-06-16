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
#'   Plots a signal analysis summary
#'
#' @description 
#'   Abstract class for providing visualizations of a signal analysis.
#'   The parameters listed below are for the constructor method ($new).
#' 
#' @param signal
#'   Optional signal associated with the plotter a priori.
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
#' @section Implements interface \code{\link{SignalSummarizer}}:
#'   \code{$open}
#'   \itemize{
#'     \item see \code{\link{SignalSummarizer_open}}
#'     \item see \code{\link{SignalPlotter_open}}
#'   }
#'   \code{$summarize}
#'   \itemize{
#'     \item see \code{\link{SignalSummarizer_summarize}}
#'     \item see \code{\link{SignalPlotter_summarize}}
#'   }
#'   \code{$close}
#'   \itemize{
#'     \item see \code{\link{SignalSummarizer_close}}
#'     \item see \code{\link{SignalPlotter_close}}
#'   }
#'   
SignalPlotter <- R6Class(
   classname = "SignalPlotter",
   inherit = SignalSummarizer,
   public = list(
      signal = NULL,
      outputPath = NULL,
      fileName = NULL,
      mfrow = NULL,
      mar = NULL,
      initialize = function
         (
            signal = NULL, 
            outputPath = NULL,
            fileName = "windowSummary.pdf",
            mfrow = c(3,2),
            mar = c(4, 4, 2, 4) + 0.1
         )
         {
            self$signal <- signal;
            self$outputPath <- outputPath;
            self$fileName <- fileName;
            self$mfrow <- mfrow;
            self$mar <- mar;
         }
      )
);

# Method SignalPlotter$open ####

#' @name SignalPlotter_open
#' 
#' @title 
#'   Opens the signal plotter
#'   
#' @description 
#'   Opens the PDF graphics device on the provided path
#' 
#' @section Method of class:
#'   \code{\link{SignalPlotter}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{SignalSummarizer_open}} -
#'     See interface for further usage documentation
#'   
SignalPlotter$set(
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

# Method SignalPlotter$close ####

#' @name SignalPlotter_close
#' 
#' @title 
#'   Closes the signal plotter
#'   
#' @description 
#'   Closes the graphics device to write the PDF file
#' 
#' @section Method of class:
#'   \code{\link{SignalPlotter}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{SignalSummarizer_close}} -
#'     See interface for further usage documentation
#'   
SignalPlotter$set(
   which = "public",
   name = "close",
   value = function()
   {
      dev.off();      
   }
);

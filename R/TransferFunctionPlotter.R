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
#'   R6 class defining a transfer function plotter
#'
#' @description 
#'   Abstract class for providing visualizations of the 
#'   results of a tranfer function analysis.
#'   This class provides the definition of an interface to a
#'   TransferFunctionPlotter object and is not intended to be instantiated
#'   directly.
#' 
TransferFunctionPlotter <- R6Class(
   classname = "TransferFunctionPlotter",
   inherit = TransferFunctionSummarizer,
   public = list(
      
      #' @field signalIn
      #'   The input signal to be plotted
      signalIn = NULL,
      
      #' @field signalOut
      #'   The output signal to be plotted
      signalOut = NULL,
      
      #' @field outputPath
      #'   The path containing the output
      outputPath = NULL,
      
      #' @field fileName
      #'   The name of the pdf file to be generated
      fileName = NULL,
      
      #' @field mfrow
      #'   The structure of the multipanel plot
      mfrow = NULL,
      
      #' @field mar
      #'   The margins for the plot
      mar = NULL,
      
      #' @field device
      #'   The device to use for coding. If NULL, a device
      #'   will not be opened. A PDF device is currently the
      #'   only other option.
      device = NULL,
      
      # Method TransferFunctionPlotter$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
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
      #' @param device
      #'   The device to use for coding. If NULL, a device
      #'   will not be opened. A PDF device is currently the
      #'   only other option.
      #'   Defaults to "pdf".
      #'   
      initialize = function
      (
         signalIn = NULL,
         signalOut = NULL,
         outputPath = NULL,
         fileName = "windowSummary.pdf",
         mfrow = c(3,2),
         mar = c(4, 4, 2, 4) + 0.1,
         device = "pdf"
      )
      {
         self$signalIn <- signalIn;
         self$signalOut <- signalOut;
         self$outputPath <- outputPath;
         self$fileName <- fileName;
         self$mfrow <- mfrow;
         self$mar <- mar;
         self$device <- device;
      },
      
      # Method TransferFunctionPlotter$open ####
      #
      #' @description 
      #'   Opens the PDF graphics device on the provided path
      #' 
      #' @param path
      #'   the path to which pdf files should be written
      #' 
      #' @return 
      #'   No defined return value.
      #' 
      open = function
      (
         path
      )
      {
         if(!is.null(self$device)) {
            pdf(file = sprintf(
               fmt = "%s/%s",
               path,
               self$fileName
            ));
         }
         par(
            mfrow = self$mfrow,
            mar = self$mar
         );      
      },

      # Method TransferFunctionPlotter$close ####
      #
      #' @description  
      #'   Closes the graphics device to write the PDF file
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      close = function()
      {
         if(!is.null(self$device)) {
            dev.off();
         }
      }
      
   )
)

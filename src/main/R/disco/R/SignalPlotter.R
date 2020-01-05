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
#'   R6 class defining a signal plotter
#'
#' @description 
#'   Abstract class for providing visualizations of a signal analysis.
#' 
SignalPlotter <- R6Class(
   classname = "SignalPlotter",
   inherit = SignalSummarizer,
   public = list(
      
      #' @field signal
      #'   The signal to be plotted
      signal = NULL,
      
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
      
      # Method SignalPlotter$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class SignalPlotter
      #'   
      #' @param signal
      #'   Optional signal associated with the plotter a priori.
      #'   Defaults to NULL.
      #' @param outputPath
      #'   Optional path to results from a signal analysis.
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
      },
      
      # Method SignalPlotter$open ####
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
         pdf(file = sprintf(
            fmt = "%s/%s",
            path,
            self$fileName
         ));
         par(
            mfrow = self$mfrow,
            mar = self$mar
         );      
      },

      # Method SignalPlotter$close ####
      #
      #' @description 
      #'   Closes the graphics device to write the PDF file
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      close = function()
      {
         dev.off();      
      }
      
   )
)

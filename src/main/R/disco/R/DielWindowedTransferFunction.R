# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class DielWindowedTransferFunction (R6) ####

#' @export
#'
#' @title 
#'   Windowed transfer function analysis delimited by times of day
#'   
#' @description 
#'   Manages a windowed transfer function analysis based on an input
#'   and output signal, where the windows are 
#'   described by a shifting period of time starting at a given
#'   time of day, ending at a given time of day, and with a duration
#'   of a defined number of days apart. Parameters listed here are
#'   for the R6 class constructor method ($new).
#' 
#' @param signalIn
#'   The input signal object from which the windows are extracted
#' @param signalOut
#'   The output signal object from which the windows are extracted
#' @param path
#'   The path to the analysis input/output on the file system
#' @param dateStart
#'   The date to start the first window
#' @param dateEnd
#'   The date on which to end the last window
#' @param windowStart
#'   The time of day to start a given window
#' @param windowEnd
#'   The time of day to end a given window
#' @param windowDays
#'   The number of days for a given window
#' @param inputDir
#'   Optional name of the directory for the input for each window
#'   Defaults to "input".
#' @param slideDays
#'   The number of days to slide the window for generating each
#'   window for analysis.
#'   Defaults to 1.
#' 
#' @section Methods:
#'   \itemize{
#'     \item $generateWindowInput - 
#'       see \code{\link{DielWindowedTransferFunction_generateWindowInputOffset}}
#'     \item $analyze -
#'       see \code{\link{DielWindowedTransferFunction_analyze}}
#'     \item $summarizeWindows - 
#'       see \code{\link{DielWindowedTransferFunction_summarizeWindows}}
#'   }
#'     
DielWindowedTransferFunction <- R6Class(
   classname = "DielWindowedTransferFunction",
   public = list(
      signalIn = NULL,
      signalOut = NULL,
      path = NULL,
      dateStart = NULL,
      dateEnd = NULL,
      windows = NULL,
      windowStart = NULL,
      windowEnd = NULL,
      windowDays = NULL,
      slideDays = NULL,
      inputPaths = NULL,
      inputFilePaths = NULL,
      initialize = function
         (
            signalIn,
            signalOut,
            path,
            dateStart,
            dateEnd,
            windowStart,
            windowEnd,
            windowDays,
            inputDir = "input",
            slideDays = 1
         )
         {
            self$signalIn <- signalIn;
            self$signalOut <- signalOut;
            self$path <- path;
            self$dateStart <- as.Date(dateStart);
            self$dateEnd <- as.Date(dateEnd);
            self$windowDays <- windowDays;
            self$slideDays <- slideDays;
            self$windows <- seq(
               from = self$dateStart, 
               to = self$dateEnd - self$windowDays, 
               by = self$slideDays
            );
            self$windowStart <- windowStart;
            self$windowEnd <- windowEnd;
            
            self$inputPaths <- sprintf(
               fmt = "%s/dates/%s/%s", 
               self$path, 
               self$windows,
               inputDir
            );
            self$inputFilePaths <- sprintf(
               fmt = "%s/signal.RData",
               self$inputPaths
            );
         }
      )
);

# Method DielWindowedTransferFunction$generateWindowInputOffset ####

#' @name DielWindowedTransferFunction_generateWindowInputOffset
#' 
#' @title 
#'   Generate the input for windows
#' 
#' @description 
#'   Parses the signal into windows based on the window definitions
#'   and a temporal offset between the output signal and the input signal.
#'   The input signal is interpolated to be a constant offset time from
#'   the output signal.
#'   
#' @param offsetTime
#'   The offset time between the input signal and output signal in seconds
#' 
#' @section Method of class:
#'   \code{\link{DielWindowedTransferFunction}}
#'   
DielWindowedTransferFunction$set(
   which = "public",
   name = "generateWindowInputOffset",
   value = function
      (
         offsetTime
      )
      {
         # Create the input directories
         
         dir.create(
            path = self$path, 
            recursive = TRUE, 
            showWarnings = FALSE
         );
         sapply(
            X = self$inputPaths,
            FUN = dir.create,
            recursive = TRUE,
            showWarnings = FALSE
         );
         
         # Populate the input for each window
         
         for (index in 1:length(self$windows)) {
            
            # Define the time window
            
            minTime = sprintf(
               fmt = "%s %s",
               self$windows[index],
               self$windowStart
            );
            maxTime = sprintf(
               fmt = "%s %s",
               self$windows[index] + self$windowDays,
               self$windowEnd
            );
            
            # Write the output signal and interpolate
            # and write the input signal
            
            signalOut <- self$signalOut$getWindow(
               minTime = minTime,
               maxTime = maxTime
            );
            signalIn <- self$signalIn$interpolate(
               time = signalOut$time - offsetTime
            );
            save(signalIn, signalOut, file = self$inputFilePaths[index]);
         }
      }
);

# Method DielWindowedTransferFunction$analyze ####

#' @name DielWindowedTransferFunction_analyze
#' 
#' @title 
#'   Analyze each window
#' 
#' @description 
#'   Runs the provided analyzer object for all windows
#'   
#' @param transferFunctionDerivation
#'   A TransferFunctionDerivation object that will perform the analysis
#'   for each window
#' @param outputDir
#'   Name of subdirectory where output is located
#'   
#' @section Method of class:
#'   \code{\link{DielWindowedTransferFunction}}
#'   
DielWindowedTransferFunction$set(
   which = "public",
   name = "analyze",
   value = function
   (
      transferFunctionDerivation,
      outputDir = "output"
   )
   {
      outputPaths <- sprintf(
         fmt = "%s/dates/%s/%s", 
         self$path, 
         self$windows,
         outputDir
      );
      sapply(
         X = outputPaths,
         FUN = dir.create,
         recursive = TRUE,
         showWarnings = FALSE
      );
      load(file = self$inputFilePaths[1]);
      results <- transferFunctionDerivation$derive(
         signalIn = signalIn,
         signalOut = signalOut,
         prevResults = NULL,
         path = outputPaths[1]
      );
      for(index in 2:length(self$windows)) {
         load(file = self$inputFilePaths[index]);
         results <- transferFunctionDerivation$derive(
            signalIn = signalIn,
            signalOut = signalOut,
            prevResults = results,
            path = outputPaths[index]
         );
      }
   }
);

# Method DielWindowedTransferFunction$summarizeWindows ####

#' @name DielWindowedTransferFunction_summarizeWindows
#' 
#' @title 
#'   Summarize the output of the analyses of windows
#' 
#' @description 
#'   Uses the provided TransferFunctionSummarizer to generate a summary
#'   of the analysis of each window.
#'   
#' @param signalSummarizer
#'   A signal summarizer that will generate the summary output for each
#'   window
#' @param outputDir
#'   Subdirectory where output is located.
#'   Not necessary if the summarizer does not need output.
#' @param summaryPath
#'   The path to where the summary should be written
#' 
#' @section Method of class:
#'   \code{\link{DielWindowedTransferFunction}}
#'   
DielWindowedTransferFunction$set(
   which = "public",
   name = "summarizeWindows",
   value = function
   (
      transferFunctionSummarizer,
      outputDir = NULL,
      summaryPath = "summary"
   )
   {
      if(is.null(outputDir))
      {
         outputPaths <- NULL
      } else {
         outputPaths <- sprintf(
            fmt = "%s/dates/%s/%s", 
            self$path, 
            self$windows,
            outputDir
         );
      }
      
      path <- sprintf(
         fmt = "%s/%s",
         self$path,
         summaryPath
      );
      dir.create(
         path = path, 
         recursive = TRUE, 
         showWarnings = FALSE
      );
      transferFunctionSummarizer$open(path = path);
      
      for(index in 1:length(self$windows)) {
         load(file = self$inputFilePaths[index]);
         minTime = sprintf(
            fmt = "%s %s",
            self$windows[index],
            self$windowStart
         );
         maxTime = sprintf(
            fmt = "%s %s",
            self$windows[index] + self$windowDays,
            self$windowEnd
         );
         if (is.null(outputPaths)) {
            outputPath <- NULL;
         } else {
            outputPath <- outputPaths[index];
         }
         transferFunctionSummarizer$summarize(
            signalIn = signalIn,
            signalOut = signalOut,
            outputPath = outputPath,
            label = self$windows[index], 
            timeBounds = c(as.POSIXct(minTime), as.POSIXct(maxTime))
         );
      }
      
      transferFunctionSummarizer$close();
   }
);

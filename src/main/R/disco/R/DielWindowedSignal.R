# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Class DielWindowedSignal (R6) ####

#' @export
#' 
#' @title 
#'   Windowed signal analysis delimited by times of day
#'   
#' @description 
#'   Manages a windowed signal analysis, where the windows are 
#'   described by a shifting period of time starting at a given
#'   time of day, ending at a given time of day, and with a duration
#'   of a defined number of days apart.
#' 
#' @usage 
#'   DielWindowedSignal$new()
#' @param signal
#'   The signal object from which the windows are extracted
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
#' @param slideDays
#'   The number of days to slide the window for generating each
#'   window for analysis.
#'   Defaults to 1.
#' 
#' @return 
#'   No defined return value.
#' 
#' @section Methods:
#'   \itemize{
#'     \item $generateWindowInput - 
#'       see \code{\link{DielWindowedSignal_generateWindowInput}}
#'     \item $analyze -
#'       see \code{\link{DielWindowedSignal_analyze}}
#'     \item $summarizeWindows - 
#'       see \code{\link{DielWindowedSignal_summarizeWindows}}
#'   }
#'     
DielWindowedSignal <- R6Class(
   classname = "DielWindowedSignal",
   public = list(
      signal = NULL,
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
      outputPaths = NULL,
      initialize = function
         (
            signal,
            path,
            dateStart,
            dateEnd,
            windowStart,
            windowEnd,
            windowDays,
            slideDays = 1
         )
         {
            self$signal <- signal;
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
         }
   )
);

# Method DielWindowedSignal$generateWindowInput ####

#' @name DielWindowedSignal_generateWindowInput
#' 
#' @title 
#'   Generate the input for windows
#' 
#' @description 
#'   Parses the signal into windows based on the window definitions
#'   
#' @param pipelineDir
#'   Optional directory structure from the base path where the folders corresponding
#'   to analysis windows should be placed.
#'   Default value is "dates".
#' @param inputDir
#'   Optional name of the directory within each window folder where the 
#'   input data for that window should be placed.
#'   Default value is "input".
#'   
#' @section Method of class:
#'   \code{\link{DielWindowedSignal}}
#'   
DielWindowedSignal$set(
   which = "public",
   name = "generateWindowInput",
   value = function
      (
         pipelineDir = "dates",
         inputDir = "input"
      )
      {
         self$inputPaths <- sprintf(
            fmt = "%s/%s/%s/%s", 
            self$path, 
            pipelineDir,
            self$windows,
            inputDir
         );
         self$inputFilePaths <- sprintf(
            fmt = "%s/signal.RData",
            self$inputPaths
         );
         
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
         for (index in 1:length(self$windows)) {
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
            signal <- self$signal$getWindow(
               minTime = minTime,
               maxTime = maxTime
            );
            save(signal, file = self$inputFilePaths[index]);
         }
      }
);

# Method DielWindowedSignal$analyze ####

#' @name DielWindowedSignal_analyze
#' 
#' @title 
#'   Analyze each window
#' 
#' @description 
#'   Runs the provided analyzer object for all windows
#'   
#' @param signalDerivation
#'   A SignalDerivation object that will perform the analysis
#'   for each window
#' @param pipelineDir
#'   Optional directory structure from the base path where the folders corresponding
#'   to analysis windows should be placed.
#'   Default value is "dates".
#' @param outputDir
#'   Optional name of the directory within each window folder where the 
#'   output data for that window should be placed.
#'   Default value is "output".
#'   
#' @section Method of class:
#'   \code{\link{DielWindowedSignal}}
#'   
DielWindowedSignal$set(
   which = "public",
   name = "analyze",
   value = function
      (
         signalDerivation,
         pipelineDir = "dates",
         outputDir = "output"
      )
      {
         self$outputPaths <- sprintf(
            fmt = "%s/%s/%s/%s", 
            self$path,
            pipelineDir,
            self$windows,
            outputDir
         );
         sapply(
            X = self$outputPaths,
            FUN = dir.create,
            recursive = TRUE,
            showWarnings = FALSE
         );
         
         load(file = self$inputFilePaths[1]);
         results <- signalDerivation$derive(
            signal = signal, 
            prevResults = NULL,
            path = self$outputPaths[1]
         );
         for(index in 2:length(self$windows)) {
            load(file = self$inputFilePaths[index]);
            results <- signalDerivation$derive(
               signal = signal, 
               prevResults = results,
               path = self$outputPaths[index]
            );
         }
      }
);

# Method DielWindowedSignal$summarizeWindows ####

#' @name DielWindowedSignal_summarizeWindows
#' 
#' @title 
#'   Summarize the output of the analyses of windows
#' 
#' @description 
#'   Uses the provided SignalSummarizer to generate a summary
#'   of the analysis of each window.
#'   
#' @param signalSummarizer
#'   A signal summarizer that will generate the summary output for each
#'   window
#' @param summaryDir
#'   Optional path from the base path where the summary output will be written.
#'   Default value is "summary".
#' @param useResults
#'   Optional switch to turn of summarization of results.
#'   Default value is TRUE (results will be sumarized).
#' 
#' @section Method of class:
#'   \code{\link{DielWindowedSignal}}
#'   
DielWindowedSignal$set(
   which = "public",
   name = "summarizeWindows",
   value = function
      (
         signalSummarizer,
         summaryDir = "summary",
         useResults = TRUE
      )
      {
         # Set up the summary output location and open the summarizer
         path <- sprintf(
            fmt = "%s/%s",
            self$path,
            summaryDir
         );
         dir.create(
            path = path, 
            recursive = TRUE, 
            showWarnings = FALSE
         );
         signalSummarizer$open(path = path);
         
         # Iterate the summary through the windows
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
            if (!useResults | is.null(self$outputPaths)) {
               outputPath <- NULL;
            } else {
               outputPath <- self$outputPaths[index];
            }
            signalSummarizer$summarize(
               signal = signal,
               outputPath = outputPath,
               label = self$windows[index], 
               timeBounds = c(as.POSIXct(minTime), as.POSIXct(maxTime))
            );
         }
         
         # Close the summarizer
         signalSummarizer$close();
      }
);

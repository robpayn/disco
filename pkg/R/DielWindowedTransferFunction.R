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
#'   of a defined number of days apart.
#' 
DielWindowedTransferFunction <- R6Class(
   classname = "DielWindowedTransferFunction",
   public = list(

      #' @field signalIn
      #'   The input signal containing the data to be windowed
      signalIn = NULL,
      
      #' @field signalOut
      #'   The output signal containing the data to be windowed
      signalOut = NULL,
      
      #' @field path
      #'   The directory path to the windows
      path = NULL,
      
      #' @field dateStart
      #'   The start date for the windows
      dateStart = NULL,
      
      #' @field dateEnd
      #'   The end date for the windows
      dateEnd = NULL,
      
      #' @field windows
      #'   Vector of dates representing the start of each window
      windows = NULL,
      
      #' @field windowStart
      #'   The time of day each window starts
      windowStart = NULL,
      
      #' @field windowEnd
      #'   The time of day each window ends
      windowEnd = NULL,
      
      #' @field windowDays
      #'   The number of days composing each window
      windowDays = NULL,
      
      #' @field slideDays
      #'   The number of days to slide the window for generating each
      #'   window for analysis.
      slideDays = NULL,
      
      #' @field inputPaths
      #'   The vector of paths to the input directories for each window
      inputPaths = NULL,
      
      #' @field inputFilePaths
      #'   The vector of paths to the input signal files for each window
      inputFilePaths = NULL,
      
      #' @field outputPaths
      #'   The vector of paths to the output directories for each window
      outputPaths = NULL,
      
      # Method DielWindowedTransferFunction$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class DielWindowedTransferFunction
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
      #' @param slideDays
      #'   The number of days to slide the window for generating each
      #'   window for analysis.
      #'   Defaults to 1.
      #'   
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
      },
      
      # Method DielWindowedTransferFunction$generateWindowInputOffset ####
      #
      #' @description 
      #'   Parses the signal into windows based on the window definitions
      #'   and a temporal offset between the output signal and the input signal.
      #'   The input signal is interpolated to be a constant offset time from
      #'   the output signal.
      #'   
      #' @param offsetTime
      #'   The offset time between the input signal and output signal in seconds
      #' @param pipelineDir
      #'   Optional directory structure from the base path where the folders corresponding
      #'   to analysis windows should be placed.
      #'   Default value is "dates".
      #' @param inputDir
      #'   Optional name of the directory within each window folder where the 
      #'   input data for that window should be placed.
      #'   Default value is "input".
      #' 
      #' @return 
      #'   No defined return value.
      #' 
      generateWindowInputOffset = function
      (
         offsetTime,
         pipelineDir = "dates",
         inputDir = "input"
      )
      {
         # Create the input directories
         
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
            saveRDS(
               list(signalIn = signalIn, signalOut = signalOut), 
               file = self$inputFilePaths[index]
            );
         }
      },
      
      
      # Method DielWindowedTransferFunction$analyze ####
      #
      #' @description 
      #'   Runs the provided analyzer object for all windows
      #'   
      #' @param transferFunctionDerivation
      #'   A TransferFunctionDerivation object that will perform the analysis
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
      #' @return 
      #'   No defined return value.
      #'   
      analyze = function
      (
         transferFunctionDerivation,
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
         signals <- readRDS(file = self$inputFilePaths[1]);
         results <- transferFunctionDerivation$derive(
            signalIn = signals$signalIn,
            signalOut = signals$signalOut,
            prevResults = NULL,
            path = self$outputPaths[1]
         );
         for(index in 2:length(self$windows)) {
            signals <- readRDS(file = self$inputFilePaths[index]);
            results <- transferFunctionDerivation$derive(
               signalIn = signals$signalIn,
               signalOut = signals$signalOut,
               prevResults = results,
               path = self$outputPaths[index]
            );
         }
      },
      
      # Method DielWindowedTransferFunction$summarizeWindows ####
      #
      #' @description 
      #'   Uses the provided TransferFunctionSummarizer to generate a summary
      #'   of the analysis of each window.
      #'   
      #' @param transferFunctionSummarizer
      #'   A transfer function summarizer that will generate the summary output for each
      #'   window
      #' @param summaryDir
      #'   Optional path from the base path where the summary output will be written.
      #'   Default value is "summary".
      #' @param useResults
      #'   Optional switch to turn of summarization of results.
      #'   Default value is TRUE (results will be sumarized).
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      summarizeWindows = function
      (
         transferFunctionSummarizer,
         summaryDir = "summary",
         useResults = TRUE
      )
      {
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
         transferFunctionSummarizer$open(path = path);
         
         for(index in 1:length(self$windows)) {
            signals <- readRDS(file = self$inputFilePaths[index]);
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
            transferFunctionSummarizer$summarize(
               signalIn = signals$signalIn,
               signalOut = signals$signalOut,
               outputPath = outputPath,
               label = self$windows[index], 
               timeBounds = c(as.POSIXct(minTime), as.POSIXct(maxTime))
            );
         }
         
         transferFunctionSummarizer$close();
      }
   )
)

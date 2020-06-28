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
#'   R6 class defining a diel windowed signal
#'   
#' @description 
#'   Manages a windowed signal analysis, where the windows are 
#'   described by a shifting period of time starting at a given
#'   time of day, ending at a given time of day, and with a duration
#'   of a defined number of days apart.
#' 
DielWindowedSignal <- R6Class(
   classname = "DielWindowedSignal",
   public = list(
      
      #' @field signal
      #'   The signal containing the data to be windowed
      signal = NULL,
      
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
      
      #' @field timeZone
      #'   Character string representing the time zone
      timeZone = NULL,
      
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
      
      # Method DielWindowedSignal$new ####
      #
      #' @description 
      #'   Create an instance of the class DielWindowedSignal
      #'   
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
      #' @param timeZone
      #'   Character string representing the time zone for the times
      #' @param slideDays
      #'   The number of days to slide the window for generating each
      #'   window for analysis.
      #'   Defaults to 1.
      #'   
      initialize = function
      (
         signal,
         path,
         dateStart,
         dateEnd,
         windowStart,
         windowEnd,
         windowDays,
         timeZone,
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
         self$timeZone <- timeZone;
      },
      
      # Method DielWindowedSignal$generateWindowInput ####
      #
      #' @description 
      #'   Parses the signal into windows based on the window definitions
      #'   
      #' @param outerDir
      #'   Optional directory structure from the base path where the folders corresponding
      #'   to analysis windows should be placed.
      #'   Default value is "dates".
      #' @param innerDir
      #'   Optional name of the directory within each window folder where the 
      #'   input data for that window should be placed.
      #'   Default value is "input".
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      generateWindowInput = function
      (
         outerDir = "dates",
         innerDir = "input"
      )
      {
         self$inputPaths <- sprintf(
            fmt = "%s/%s/%s/%s", 
            self$path, 
            outerDir,
            self$windows,
            innerDir
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
            minTime = as.POSIXct(
               sprintf(
                  fmt = "%s %s",
                  self$windows[index],
                  self$windowStart
               ),
               tz = self$timeZone
            );
            maxTime = as.POSIXct(
               sprintf(
                  fmt = "%s %s",
                  self$windows[index] + self$windowDays,
                  self$windowEnd
               ),
               tz = self$timeZone
            );
            signal <- self$signal$getWindow(
               minTime = minTime,
               maxTime = maxTime
            );
            saveRDS(
               signal, 
               file = self$inputFilePaths[index]
            );
         }
      },
      
      # Method DielWindowedSignal$analyze ####
      #
      #' @description 
      #'   Runs the provided analyzer object for all windows
      #'   
      #' @param signalDerivation
      #'   A SignalDerivation object that will perform the analysis
      #'   for each window
      #' @param outerDir
      #'   Optional directory structure from the base path where the folders corresponding
      #'   to analysis windows should be placed.
      #'   Default value is "dates".
      #' @param innerDir
      #'   Optional name of the directory within each window folder where the 
      #'   output data for that window should be placed.
      #'   Default value is "output".
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      analyze = function
      (
         signalDerivation,
         outerDir = "dates",
         innerDir = "output"
      )
      {
         self$outputPaths <- sprintf(
            fmt = "%s/%s/%s/%s", 
            self$path,
            outerDir,
            self$windows,
            innerDir
         );
         sapply(
            X = self$outputPaths,
            FUN = dir.create,
            recursive = TRUE,
            showWarnings = FALSE
         );
         
         signal <- readRDS(file = self$inputFilePaths[1]);
         results <- signalDerivation$derive(
            signal = signal, 
            prevResults = NULL,
            path = self$outputPaths[1]
         );
         for(index in 2:length(self$windows)) {
            signal <- readRDS(file = self$inputFilePaths[index]);
            results <- signalDerivation$derive(
               signal = signal, 
               prevResults = results,
               path = self$outputPaths[index]
            );
         }
      },
      
      # Method DielWindowedSignal$summarizeWindows ####
      #
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
      #' @return 
      #'   No defined return value.
      #' 
      summarizeWindows = function
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
            signal <- readRDS(file = self$inputFilePaths[index]);
            minTime = as.POSIXct(
               sprintf(
                  fmt = "%s %s",
                  self$windows[index],
                  self$windowStart
               ),
               tz = self$timeZone
            );
            maxTime = as.POSIXct(
               sprintf(
                  fmt = "%s %s",
                  self$windows[index] + self$windowDays,
                  self$windowEnd
               ),
               tz = self$timeZone
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
   )
)

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
#' @param inputDir
#'   Optional name of the directory for the input for each window
#'   Defaults to "input".
#' @param outputDir
#'   Optional name of the directory for the output for each window.
#'   Defaults to "output".
#' @param slideDays
#'   The number of days to slide the window for generating each
#'   window for analysis.
#'   Defaults to 1.
#' 
#' @return 
#'   No defined return value.
#' 
#' @section Methods:
#'   $new - see above \cr
#'   $generateWindowInput - see
#'     \code{\link{DielWindowedSignal_generateWindowInput}} \cr
#'   $analyze - see
#'     \code{\link{DielWindowedSignal_analyze}} \cr
#'   $plotWindowSummary - see
#'     \code{\link{DielWindowedSignal_plotWindowSummary}} \cr
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
      initialize = function
         (
            signal,
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

# Method DielWindowedSignal$generateWindowInput ####

#' @name DielWindowedSignal_generateWindowInput
#' 
#' @title 
#'   Generate the input for windows
#' 
#' @description 
#'   Parses the signal into windows based on the window definitions
#' 
#' @section Method of class:
#'   \code{\link{DielWindowedSignal}}
#'   
DielWindowedSignal$set(
   which = "public",
   name = "generateWindowInput",
   value = function()
      {
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
         results <- signalDerivation$derive(
            signal = signal, 
            prevResults = NULL,
            path = outputPaths[1]
         );
         for(index in 2:length(self$windows)) {
            load(file = self$inputFilePaths[index]);
            results <- signalDerivation$derive(
               signal = signal, 
               prevResults = results,
               path = outputPaths[index]
            );
         }
      }
);

# Method DielWindowedSignal$plotWindowSummary ####

#' @name DielWindowedSignal_plotWindowSummary
#' 
#' @title 
#'   Summarize the output of the analyses of windows
#' 
#' @description 
#'   Uses the provided SignalPlotter to generate a visual summary
#'   of the analysis of each window.
#'   
#' @param signalPlotter
#'   A signal plotter that will generate the summary plot for each
#'   window
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
#' @section Method of class:
#'   \code{\link{DielWindowedSignal}}
#'   
DielWindowedSignal$set(
   which = "public",
   name = "plotWindowSummary",
   value = function
   (
      signalPlotter,
      fileName = "windowSummary.pdf",
      outputDir = NULL,
      mfrow = c(3,2),
      mar = c(4, 4, 2, 4) + 0.1
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
         fmt = "%s/summary",
         self$path
      );
      dir.create(
         path = path, 
         recursive = TRUE, 
         showWarnings = FALSE
      );
      pdf(file = sprintf(
         fmt = "%s/%s",
         path,
         fileName
      ));
      par(
         mfrow = mfrow,
         mar = mar
      );
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
         signalPlotter$plot(
            signal = signal,
            outputPath = outputPath,
            label = self$windows[index], 
            timeBounds = c(as.POSIXct(minTime), as.POSIXct(maxTime))
         );
      }
      dev.off();
   }
);

DielWindowedSignal$set(
   which = "public",
   name = "tabulateWindowSummary",
   value = function
      (
         resultExtractor,
         outputDir = "output"
      )
      {
         outputPaths <- sprintf(
            fmt = "%s/dates/%s/%s", 
            self$path, 
            self$windows,
            outputDir
         );
         for(index in 1:length(self$windows)) {
            load(file = self$inputFilePaths[index]);
            resultExtractor$extract(
               signal = signal,
               outputPath = outputPaths[index]
            );
         }
         path <- sprintf(
            fmt = "%s/summary",
            self$path
         );
         dir.create(
            path = path, 
            recursive = TRUE, 
            showWarnings = FALSE
         );
         resultExtractor$write(path);
      }     
);

# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# R6 Class DielWindowedSignal ####

#' @export
#' 
#' @title 
#'   R6 class defining a diel windowed signal analysis
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
      
      ## Attributes ####
      
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
      
      ## Method: constructor ####
      #
      #' @description 
      #'   Create an instance of the class.
      #'   
      #' @param signal
      #'   The signal object from which the windows are extracted.
      #'   Expected data type is a disco::Signal object.
      #' @param path
      #'   The path to the analysis input/output on the file system.
      #'   Expected data type is a character vector with a single element.
      #' @param dateStart
      #'   The date to start the first window.
      #'   Expected data type is either a Date object or a character vector
      #'   with a single element that is coercible to a Date with as.Date 
      #'   using a default format.
      #' @param dateEnd
      #'   The date on which to end the last window.
      #'   Expected data type is either a Date object or a character vector
      #'   with a single element that is coercible to a Date with as.Date 
      #'   using a default format.
      #' @param windowStart
      #'   The time of day to start a given window.
      #'   Expected data type is a character vector with a single element.
      #'   Format should be interpretable by as.POSIXct by default.
      #' @param windowEnd
      #'   The time of day to end a given window.
      #'   Expected data type is a character vector with a single element.
      #'   Format should be interpretable by as.POSIXct by default.
      #' @param windowDays
      #'   The number of days for a given window.
      #'   Expected data type is an numeric or integer vector with a single
      #'   element.
      #' @param timeZone
      #'   The time zone for the other time related arguments.
      #'   Expected data type is a character vector with a single element.
      #' @param slideDays
      #'   Optional: number of days to slide the window for generating each
      #'   window for analysis.
      #'   Defaults value is 1.
      #'   Expected data type is a numeric or integer vector with a 
      #'   single element.
      #'   
      #' @returns
      #'   This function is called by the constructor (new()) method of the 
      #'   R6 class generator. The constructor returns a new instance of this
      #'   class.
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
         # Populate attributes
         
         self$signal <- signal;
         self$path <- path;
         self$dateStart <- as.Date(dateStart);
         self$dateEnd <- as.Date(dateEnd);
         self$windowDays <- windowDays;
         self$slideDays <- slideDays;
         self$windowStart <- windowStart;
         self$windowEnd <- windowEnd;
         self$timeZone <- timeZone;
         
         # Determine the potential vector of windows based
         # on arguments provided
         
         self$windows <- seq(
            from = self$dateStart,
            to = self$dateEnd - self$windowDays,
            by = self$slideDays
         )
      },
      
      ## Method: createWindows ####
      #
      #' @description 
      #'   Parses the signal into windows based on the window definitions
      #'   
      #' @param outerDir
      #'   Optional: directory structure from the base path where the folders 
      #'   corresponding to analysis windows should be placed.
      #'   Default value is "dates".
      #'   Expected data type is a character vector with a single element.
      #' @param innerDir
      #'   Optional: name of the directory within each window folder where the 
      #'   signal file for that window should be placed.
      #'   Default value is "input".
      #'   Expected data type is a character vector with a single element.
      #' @param fileName
      #'   Optional: file name to use for the signal within a given window.
      #'   Default value is "signal".
      #'   Expected data type is a character vector with a single element.
      #' @param minimumRecords
      #'   Optional: a signal for a window needs at least this number of 
      #'   records for the window to be created. A null value indicates that 
      #'   the number of signal records should not be used to determine
      #'   valid windows.
      #'   Default value is 1 (windows with 0 records will not be created).
      #'   Expected data type is an atomic vector with a single numeric 
      #'   or integer element.
      #' @param isValid
      #'   Optional: logical flags to determine if a window should (TRUE) or 
      #'   should not (FALSE) be created. A null value indicates all windows
      #'   should initially be considered valid, pending further evaluation
      #'   based on criteria defined by other arguments.
      #'   Default value is NULL.
      #'   Expected data type is an atomic vector of logical values with the 
      #'   same length as the number of potential windows.
      #' @param validFunction
      #'   Optional: function to call to determine if a signal for a given
      #'   day should be considered valid (function returns TRUE) or invalid
      #'   (function returns FALSE). Providing a null value disables 
      #'   this feature.
      #'   Default value is NULL.
      #'   Expected data type is a function with a single required argument
      #'   called "signal", to which the signal for a given window will be
      #'   passed. The function should return a logical vector with
      #'   a single element.
      #' 
      #' @return 
      #'   Invisibly returns the character vector representing the valid
      #'   windows.
      #'   
      createWindows = function
      (
         outerDir = "dates",
         innerDir = "input",
         fileName = "signal",
         minimumRecords = 1,
         isValid = NULL,
         validFunction = NULL
      )
      {
         dir.create(
            path = self$path, 
            recursive = TRUE, 
            showWarnings = FALSE
         )
         
         if (outerDir != "") {
            outerDir <- paste0("/", outerDir)
         }
         if (innerDir != "") {
            innerDir <- paste0("/", innerDir)
         }
         inputPaths <- sprintf(
            fmt = "%s%s/%s%s",
            self$path,
            outerDir,
            self$windows,
            innerDir
         )
         inputFilePaths <- sprintf(
            fmt = "%s/%s.RData",
            inputPaths,
            fileName
         )
         
         if (is.null(isValid)) {
            isValid <- rep(TRUE, times = length(self$windows))
         }
         
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
            
            if (!is.null(minimumRecords)) {
               isValid[index] <- 
                  isValid[index] &&
                  signal$getLength() >= minimumRecords
            }
            
            if (!is.null(validFunction)) {
               isValid[index] <- 
                  isValid[index] && 
                  validFunction(signal = signal)
            }
         
            if (isValid[index]) {
               dir.create(
                  path = inputPaths[index],
                  recursive = TRUE,
                  showWarnings = FALSE
               )
               saveRDS(
                  signal, 
                  file = inputFilePaths[index]
               );
            }
         }
         
         self$windows <- self$windows[isValid]
         self$inputPaths <- inputPaths[isValid]
         self$inputFilePaths <- inputFilePaths[isValid]
         
         invisible(self$windows)
      },
      
      ## Method: analyze ####
      #
      #' @description 
      #'   Runs the provided analyzer object for all windows
      #'   
      #' @param signalDerivation
      #'   A SignalDerivation object that will perform the analysis
      #'   for each window
      #' @param outerDir
      #'   Optional: directory structure from the base path where the folders 
      #'   corresponding to analysis windows should be placed.
      #'   Default value is "dates".
      #' @param innerDir
      #'   Optional: name of the directory within each window folder where the 
      #'   output data for that window should be placed.
      #'   Default value is "output".
      #' 
      #' @return 
      #'   Invisibly returns a character vector with the paths to the 
      #'   derivation outputs.
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
            path = self$outputPaths[1],
            index = 1
         );
         for(index in 2:length(self$windows)) {
            signal <- readRDS(file = self$inputFilePaths[index]);
            results <- signalDerivation$derive(
               signal = signal, 
               prevResults = results,
               path = self$outputPaths[index],
               index = index
            );
         }
         
         invisible(self$outputPaths)
      },
      
      ## Method: summarizeWindows ####
      #
      #' @description 
      #'   Uses the provided SignalSummarizer to generate a summary
      #'   of the analysis of each window.
      #'   
      #' @param signalSummarizer
      #'   A signal summarizer that will generate the summary output for each
      #'   window
      #' @param summaryDir
      #'   Optional: path from the base path where the summary output 
      #'   will be written.
      #'   Default value is "summary".
      #' @param useResults
      #'   Optional: switch to turn of summarization of results.
      #'   Default value is TRUE (results will be sumarized).
      #' 
      #' @return 
      #'   No return value.
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
         signalSummarizer$close()
         
         invisible(NULL)
      }
   )
)

# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# R6 class DielWindowedTransferFunction ####

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
      
      ## Attributes ####

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
      #'   Constructs a new instance of the class
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
      #' @param timeZone
      #'   Character string representing the time zone for the times
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
         timeZone,
         slideDays = 1
      )
      {
         # Populate the attributes
         
         self$signalIn <- signalIn;
         self$signalOut <- signalOut;
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
         );
      },
      
      ## Method: createWindowsOffset ####
      #
      #' @description 
      #'   Parses the signal into windows based on the window definitions
      #'   and a temporal offset between the output signal and the input signal.
      #'   The input signal is interpolated to be a constant offset time from
      #'   the output signal.
      #'   
      #' @param offsetTime
      #'   If a single element character vector, offsetTime should represent
      #'   the variable name providing values for the offset time 
      #'   (in seconds) between corresponding elements of the input signal 
      #'   and output signals.
      #'   If a numerical vector, offsetTime should represent the offset time 
      #'   (in seconds) between corresponding elements of the input signal 
      #'   and output signals. A vector with a single element implies a 
      #'   offset time.
      #'   Expected data type is a character vector with a single element
      #'   or a numeric vector.
      #' @param headersIn
      #'   Vector of character strings indicating the headers of variables 
      #'   from signalIn that need to be considered the input signal.
      #'   Expected data type is a character vector.
      #' @param outerDir
      #'   Optional directory structure from the base path where the folders 
      #'   corresponding to analysis windows should be placed.
      #'   Default value is "dates".
      #'   Expected data type is a character vector with a single element.
      #' @param innerDir
      #'   Optional name of the directory within each window folder where the 
      #'   input data for that window should be placed.
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
      #'   Expected data type is a function with two required arguments
      #'   called "signalOut" and "signalIn", to which the output and input
      #'   signals for a given window will be passed. 
      #'   The function should return a logical vector with
      #'   a single element.
      #' 
      #' @return 
      #'   Invisibly returns the character vector representing the valid
      #'   windows.
      #' 
      createWindowsOffset = function
      (
         offsetTime,
         headersIn,
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
         );
         
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
         );
         inputFilePaths <- sprintf(
            fmt = "%s/%s.RData",
            inputPaths,
            fileName
         );

         if (is.null(isValid)) {
            isValid <- rep(TRUE, times = length(self$windows))
         }
         
         for (index in 1:length(self$windows)) {
            
            # Define the time window
            
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
            
            # Write the output signal and interpolate
            # and write the input signal
            
            signalOut <- self$signalOut$getWindow(
               minTime = minTime,
               maxTime = maxTime
            );

            if (!is.numeric(offsetTime)) {
               if ( is.character(offsetTime) && (length(offsetTime) == 1) ) {
                  offset <- signalOut$getVariable(offsetTime)
               } else {
                  stop(paste(
                     "The argument offsetTime must be a header name for a",
                     "variable in the output signal or a numeric vector",
                     "of offset values (can be a single value)."
                  ))
               }
            } else {
               offset <- offsetTime
            }

            signalIn <- self$signalIn$getWindow(
               minTime = minTime - offset[1],
               maxTime = maxTime - offset[length(offsetTime)]
            );

            if (!is.null(minimumRecords)) {
               isValid[index] <- 
                  isValid[index] &&
                  signalOut$getLength() >= minimumRecords &&
                  signalIn$getLength() >= minimumRecords 
            }
            
            if (!is.null(validFunction)) {
               isValid[index] <- 
                  isValid[index] && 
                  validFunction(signalOut = signalOut, signalIn = signalIn)
            }
            
            if (isValid[index]) {
               dir.create(
                  path = inputPaths[index],
                  recursive = TRUE,
                  showWarnings = FALSE
               )

               signalIn <- self$signalIn$interpolate(
                  headers = headersIn,
                  time = signalOut$getTime() - offset
               );
               
               saveRDS(
                  list(signalIn = signalIn, signalOut = signalOut), 
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
      #' @param transferFunctionDerivation
      #'   A TransferFunctionDerivation object that will perform the analysis
      #'   for each window
      #' @param outerDir
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
         outerDir = "dates",
         outputDir = "output"
      )
      {
         self$outputPaths <- sprintf(
            fmt = "%s/%s/%s/%s", 
            self$path, 
            outerDir,
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
            path = self$outputPaths[1],
            index = 1
         );
         for(index in 2:length(self$windows)) {
            signals <- readRDS(file = self$inputFilePaths[index]);
            results <- transferFunctionDerivation$derive(
               signalIn = signals$signalIn,
               signalOut = signals$signalOut,
               prevResults = results,
               path = self$outputPaths[index],
               index = index
            );
         }
      },
      
      ## Method: summarizeWindows ####
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
      #'   Default value is TRUE (results will be summarized).
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
            transferFunctionSummarizer$summarize(
               signalIn = signals$signalIn,
               signalOut = signals$signalOut,
               outputPath = outputPath,
               label = self$windows[index], 
               timeBounds = c(minTime, maxTime)
            );
         }
         
         transferFunctionSummarizer$close();
      }
   )
)

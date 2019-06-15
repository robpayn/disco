# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

#' @export
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

# Method DielWindowedTransferFunction$plotWindowSummary ####

DielWindowedTransferFunction$set(
   which = "public",
   name = "plotWindowSummary",
   value = function
   (
      transferFunctionPlotter,
      fileName = "windowSummary.pdf",
      outputDir = NULL,
      summaryPath = "summary",
      mfrow = c(3,2),
      mar = c(2, 4, 2, 4) + 0.1
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
         transferFunctionPlotter$plot(
            signalIn = signalIn,
            signalOut = signalOut,
            outputPath = outputPath,
            label = self$windows[index], 
            timeBounds = c(as.POSIXct(minTime), as.POSIXct(maxTime))
         );
      }
      dev.off();
   }
);

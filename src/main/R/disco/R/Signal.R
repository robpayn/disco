# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Signal R6 Interface ####

#' @export
#'
#' @title
#'   Manages time series data
#'
#' @section Methods:
#'   \code{$new}\cr
#'   \code{$getWindow} (abstract) - See \code{\link{Signal_getWindow}}
#'
Signal <- R6Class(
   classname = "Signal"
);

#' @name Signal_getWindow
#'
#' @title
#'   Gets a subset of a signal based on a time window
#'
#' @description
#'   Abstract method defining the interface for getting a subset
#'   of a signal.
#'
#' @section Abstract method of class:
#'   \code{\link{Signal}}
#'
Signal$set(
   which = "public",
   name = "getWindow",
   value = function(...)
      {
         stop("Abstract method 'getWindow' has not been implemented.");
      }
);

# SignalDataFrame R6 Class ####

#' @export
#'
#' @title
#'   Signal based on a time series DataFrame object
#'
#' @section Methods:
#'   Static:\cr
#'   \code{$constructFromCSV} -
#'     See \code{\link{SignalDataFrame_constructFromCSV}}
#'
#'   \code{$new}\cr
#'   \code{$getWindow} -
#'     See \code{\link{SignalDataFrame_getWindow}}
#'
SignalDataFrame <- R6Class(
   classname = "SignalDataFrame",
   inherit = Signal,
   public = list(
      dataFrame = NULL,
      timeHeader = NULL,
      time = NULL,
      initialize = function(
            dataFrame = DataFrame$new(),
            timeHeader = "time",
            ...
         )
         {
            self$dataFrame <- dataFrame;
            self$timeHeader <- timeHeader;
            if (!is.null(self$dataFrame$data[[self$timeHeader]])) {
               self$time <-
                  as.POSIXct(self$dataFrame$data[[self$timeHeader]], ...);
            }
         },
      plotSummary = function(
         x = NULL,
         mfrow = c(length(self$dataFrame$data) - 1, 1),
         mar = c(4, 4, 1, 1) + 0.1
         )
         {
            par(
               mfrow = mfrow,
               mar = mar
            );
            for(header in names(self$dataFrame$data)) {
               if (header != self$timeHeader) {
                  if (is.null(x)) {
                     self$plot(
                        header = header
                     );
                  } else {
                     self$plot(
                        header = header,
                        x = x
                     )
                  }
               }
            }
         },
      plot = function(
         header,
         x = self$time,
         y = self$dataFrame$data[[header]],
         xlab = self$timeHeader,
         ylab = sprintf(
            "%s (%s)",
            header,
            self$dataFrame$metaColumns[header,]$units
            ),
         ...
         )
         {
            plot(
               x = x,
               y = y,
               xlab = xlab,
               ylab = ylab,
               ...
            );
         }
   )
);

# Static method SignalDataFrame$constructFromCSV ####

#' @name SignalDataFrame_constructFromCSV
#'
#' @title
#'   Static method to construct a DataFrame signal from csv files
#'
#' @section Static method of class:
#'   \code{\link{SignalDataFrame}}
#'
SignalDataFrame$constructFromCSV <- function(
   fileName,
   timeHeader = "time",
   ...
)
{

   return(
      SignalDataFrame$new(
         DataFrame$constructFromCSV(fileName),
         timeHeader,
         ...
      )
   );

};

# Method SignalDataFrame$getWindow ####

#' @name SignalDataFrame_getWindow
#'
#' @title
#'   Gets a subset of a signal based on a time window
#'
#' @description
#'   Method for getting a subset of a signal.
#'
#' @return
#'   A subset of the signal as a SignalDataFrame object
#'
#' @section Abstract method of class:
#'   \code{\link{SignalDataFrame}}
#'
SignalDataFrame$set(
   which = "public",
   name = "getWindow",
   value = function(minTime, maxTime)
      {
         indices <-
            self$time > as.POSIXct(minTime) &
            self$time < as.POSIXct(maxTime);
         subSignal <- SignalDataFrame$new();
         subSignal$time <- self$time[indices];
         subSignal$dataFrame$data <- self$dataFrame$data[indices,];
         subSignal$dataFrame$copyMetaData(self$dataFrame);
         return(subSignal);
      }
);

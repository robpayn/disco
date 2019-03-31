# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# SignalDataFrame R6 Class ####

#' @export
#'
#' @title
#'   Signal based on a time series DataFrame object
#'
SignalDataFrame <- R6Class(
   classname = "SignalDataFrame",
   inherit = DataFrame,
   public = list(
      timeHeader = NULL,
      time = NULL,
      initialize = function(timeHeader = "time")
         {
            self$timeHeader <- timeHeader;
         },
      setTimeFromData = function(...)
         {
            self$time <-
               as.POSIXct(self$data[[self$timeHeader]], ...);
         },
      getWindow = function(minTime, maxTime)
         {
            indices <-
               self$time > as.POSIXct(minTime) &
               self$time < as.POSIXct(maxTime);
            signal <- SignalDataFrame$new();
            signal$time <- self$time[indices];
            signal$data <- self$data[indices,];
            signal$copyMetaData(self);
            return(signal);
         },
      copyMetaData = function(signal)
         {
            self$meta <- signal$meta;
            self$metaColumns <- signal$metaColumns;
         },
      plotSummary = function(
         x = NULL,
         mfrow = c(length(self$data) - 1, 1),
         mar = c(4, 4, 1, 1) + 0.1
         )
         {
            par(
               mfrow = mfrow,
               mar = mar
            );
            for(header in names(self$data)) {
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
         y = self$data[[header]],
         xlab = self$timeHeader,
         ylab = sprintf(
            "%s (%s)",
            header,
            self$metaColumns[header,]$units
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

#' @name SignalDataFrame_constructFromCSV
#'
#' @title
#'   Static method to construct a DataFrame signal from csv files
#'
SignalDataFrame$constructFromCSV <- function(
   signal,
   fileName,
   ...
)
{

   signal <- DataFrame$constructFromCSV(signal, fileName);
   signal$setTimeFromData(...);
   return(signal);

};

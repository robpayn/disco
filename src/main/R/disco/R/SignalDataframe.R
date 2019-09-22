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
#' @usage 
#'   SignalDataFrame$new()
#' @param dataFrame
#'   The dataframe providing the basis of the signal.
#'   Defaults to a new DataFrame object.
#' @param timeVariableName
#'   The header in the DataFrame data for the column with
#'   time data.
#'   Defaults to "time".
#' @param ...
#'   Arguments passed to as.POSIXct in coverting the time
#'   data to a POSIXct vector.
#'
#' @section Implements interface \code{\link{Signal}}:
#'   \code{$getWindow}
#'   \itemize{
#'     \item see \code{\link{Signal_getWindow}}
#'     \item see \code{\link{SignalDataFrame_getWindow}}
#'   }
#'   \code{$getVariable}
#'   \itemize{
#'     \item see \code{\link{Signal_getVariable}}
#'     \item see \code{\link{SignalDataFrame_getVariable}}
#'   }
#'   \code{$getVariable}
#'   \itemize{
#'     \item see \code{\link{Signal_addVariable}}
#'     \item see \code{\link{SignalDataFrame_addVariable}}
#'   }
#'   \code{$writeCSV}
#'   \itemize{
#'     \item see \code{\link{Signal_writeCSV}}
#'     \item see \code{\link{SignalDataFrame_writeCSV}}
#'   }
#'   \code{$interpolate}
#'   \itemize{
#'     \item see \code{\link{Signal_interpolate}}
#'     \item see \code{\link{SignalDataFrame_interpolate}}
#'   }
#'   \code{$plotSummary}
#'   \itemize{
#'     \item see \code{\link{Signal_plotSummary}}
#'     \item see \code{\link{SignalDataFrame_plotSummary}}
#'   }
#'   \code{$plot}
#'   \itemize{
#'     \item see \code{\link{Signal_plot}}
#'     \item see \code{\link{SignalDataFrame_plot}}
#'   }
#'     
#' @section Static Methods:
#'   \code{$constructFromCSV} -
#'     see \code{\link{SignalDataFrame_constructFromCSV}}
#'
SignalDataFrame <- R6Class(
   classname = "SignalDataFrame",
   inherit = Signal,
   public = list(
      dataFrame = NULL,
      timeVariableName = NULL,
      initialize = function(
            dataFrame = DataFrame$new(),
            timeVariableName = "time",
            ...
         )
         {
            self$dataFrame <- dataFrame;
            self$timeVariableName <- timeVariableName;
            if (!is.null(self$dataFrame$data[[self$timeVariableName]])) {
               self$time <-
                  as.POSIXct(self$dataFrame$data[[self$timeVariableName]], ...);
            }
         }
   )
);

# Static method SignalDataFrame$constructFromCSV ####

#' @name SignalDataFrame_constructFromCSV
#'
#' @title
#'   Static method to construct a DataFrame signal from csv files
#' 
#' @description
#'   Constructs a signal based on a series of csv files providing
#'   data and metadata bout the signal
#'   
#' @param fileName
#'   The filename basis for the import. See \code{\link{DataFrame_constructFromCSV}}
#' @param timeVariableName
#'   The header in the csv data for the column with
#'   time data.
#'   Defaults to "time".
#'
#' @section Static method of class:
#'   \code{\link{SignalDataFrame}}
#'
SignalDataFrame$constructFromCSV <- function(
   fileName,
   timeVariableName = "time",
   ...
)
{

   return(
      SignalDataFrame$new(
         dataFrame = DataFrame$constructFromCSV(fileName),
         timeVariableName = timeVariableName,
         ...
      )
   );

};


# Method SignalDataFrame$getVariable ####

#' @name SignalDataFrame_getVariable
#'
#' @title
#'   Gets a variable vector
#'
#' @section Method of class:
#'   \code{\link{SignalDataFrame}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Signal_getVariable}} -
#'     See interface for documentation
#'   
SignalDataFrame$set(
   which = "public",
   name = "getVariable",
   value = function(variableName) 
   {
      return(self$dataFrame$data[[variableName]]);
   }
);

# Method SignalDataFrame$getWindow ####

#' @name SignalDataFrame_getWindow
#'
#' @title
#'   Gets a subset of a signal based on a time window
#'
#' @section Method of class:
#'   \code{\link{SignalDataFrame}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Signal_getWindow}} -
#'     See interface for documentation
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

# Method SignalDataFrame$plotSummary ####

#' @name SignalDataFrame_plotSummary
#' 
#' @title 
#'   Plot a summary of the multivariate signal
#'   
#' @param x
#'   Defaults to signal time variable
#' @param mfrow
#'   Defaults to \code{c(length(self$dataFrame$data) - 1, 1)}
#' @param mar
#'   Defaults to \code{c(4, 4, 1, 1) + 0.1}
#'   
#' @section Method of class:
#'   \code{\link{SignalDataFrame}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Signal_plotSummary}} -
#'     See interface for documentation
#'   
SignalDataFrame$set(
   which = "public",
   name = "plotSummary",
   value = function
      (
         x = NULL,
         mfrow = c(length(self$dataFrame$data) - 1, 1),
         mar = c(4, 4, 1, 1) + 0.1
      )
      {
         par(
            mfrow = mfrow,
            mar = mar
         );
         for(variableName in names(self$dataFrame$data)) {
            if (variableName != self$timeVariableName) {
               if (is.null(x)) {
                  self$plot(
                     variableName = variableName
                  );
               } else {
                  self$plot(
                     variableName = variableName,
                     x = x
                  )
               }
            }
         }
      }
);

# Method SignalDataFrame$plot ####

#' @name SignalDataFrame_plot
#' 
#' @title 
#'   Plot a single variable in the signal
#'   
#' @description 
#'   Plots a single variable in the signal based on the provided header
#'   for the variable
#' 
#' @param variableName
#'   Name of the variable to plot on the y axis.  
#' @param x
#'   Variable plotted on x axis. 
#'   Default value is the time.
#' @param timeZone
#'   If specified, the time zone for the datetime scale.
#'   Defult value is NA (no change in time zone).
#' @param y
#'   Variable plotted on y axis. 
#'   Default value is the values in the provided variableName argument.
#' @param xlim
#'   Vector of 2 values for the min and max of x axis scale.
#'   Default is min and max of x variable
#' @param xlab
#'   Label for x axis.
#'   Default value is the header for time data.
#' @param ylim
#'   Vector of 2 values for the min and max of y axis scale.
#'   Default value is min and max of y variable.
#' @param ylab
#'   Label for y axis.
#'   Default value is the header selected and the units
#'   for that variable
#' @param compareWith
#'   Vector of signals to plot on the same axes.
#'   Specifying signals for comparison will adjust the xlim
#'   and ylim values to be sure all values will be visible on
#'   plot.
#'   Default value is an empty list (no additional plots).
#' @param colors
#'   A vector of character strings specifying colors for additional
#'   plots.
#'   Defaults to a color blind friendly palette of purple, orange,
#'   and green.
#' @param pchs
#'   A vector of integers specifying the point symbol to use for 
#'   additional plots.
#'   Defaults to square, triangle, and diamond.
#' @param ...
#'   Any remaining arguments provided will be passed to the call to
#'   the plot.default method, allowing for further customization of
#'   the plot.
#'   
#' @section Method of class:
#'   \code{\link{SignalDataFrame}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Signal_plot}} -
#'     See interface for documentation
#'   
SignalDataFrame$set(
   which = "public",
   name = "plot",
   value = function
      (
         variableName,
         x = self$time,
         timeZone = NA,
         y = self$dataFrame$data[[variableName]],
         xlim = c(min(x), max(x)),
         xlab = self$timeVariableName,
         ylim = c(min(y), max(y)),
         ylab = sprintf(
            "%s (%s)",
            variableName,
            self$dataFrame$metaColumns[variableName,]$units
            ),
         compareWith = list(),
         colors = c(
            "purple",
            "orange",
            "green"
            ),
         pchs = c(
           0,
           2,
           5
         ),
         ...
      )
      {
         if(!is.na(timeZone)) {
            attributes(x)$tzone <- timeZone;
         }
         if(length(compareWith) > 0) {
            xlims <- sapply(
               compareWith, 
               FUN = function(x) 
               {
                  time <- x$time;
                  if(!is.na(timeZone)) {
                     attributes(time)$tzone <- timeZone;
                  } 
                  return(
                     c(
                        min(time),
                        max(time)
                     )
                  );
               }
            );
            xlim <- c(
               min(xlim[1], xlims[1,]),
               max(xlim[2], xlims[2,])
            );
            
            ylims <- sapply(
               compareWith, 
               FUN = function(x) 
                  {
                     return(
                        c(
                           min(x$getVariable(variableName)),
                           max(x$getVariable(variableName))
                        )
                     );
                  }
            );
            ylim <- c(
               min(ylim[1], ylims[1,]),
               max(ylim[2], ylims[2,])
            );
         }
         plot(
            x = x,
            y = y,
            xlim = xlim,
            xlab = xlab,
            ylim = ylim,
            ylab = ylab,
            ...
         );
         if(length(compareWith) > 0) {
            for(plotCount in 1:length(compareWith)) {
               time <- compareWith[[plotCount]]$time;
               if(!is.na(timeZone)) {
                  attributes(time)$tzone <- timeZone;
               }
               points(
                  x = time, 
                  y = compareWith[[plotCount]]$getVariable(variableName),
                  col = colors[plotCount],
                  pch = pchs[plotCount]
               );
            }
         }
      }
);

# Method SignalDataFrame$addVariable ####

#' @name SignalDataFrame_addVariable
#' 
#' @title 
#'   Add a variable to a signal
#'   
#' @section Method of class:
#'   \code{\link{SignalDataFrame}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Signal_addVariable}} -
#'     See interface for documentation
#'   
SignalDataFrame$set(
   which = "public",
   name = "addVariable",
   value = function
      (
         variableName,
         value,
         units,
         dimensions
      )
      {
         self$dataFrame$addColumn(
            property = variableName,
            value = value,
            units = units,
            dimensions = dimensions
         );
      }
);

# Method SignalDataFrame$writeCSV ####

#' @name SignalDataFrame_writeCSV
#' 
#' @title 
#'   Write the signal in csv format
#'   
#' @param timeVariableName
#'   Defaults to the time header attribute of the signal
#'   
#' @section Method of class:
#'   \code{\link{SignalDataFrame}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Signal_writeCSV}} -
#'     See interface for documentation
#'   
SignalDataFrame$set(
   which = "public",
   name = "writeCSV",
   value = function
      (
         path, 
         name,
         timeVariableName = self$timeVariableName
      )
      {
         self$dataFrame$data[, timeVariableName] <- 
            as.character(self$time);
         write.csv(
            x = self$dataFrame$data,
            file = sprintf(
               fmt = "%s/%s.csv",
               path,
               name
               ),
            row.names = FALSE
         );
         write.csv(
            x = self$dataFrame$metaColumns,
            file = sprintf(
               fmt = "%s/%s_meta_columns.csv",
               path,
               name
            ),
            row.names = FALSE
         );
         write.csv(
            x = self$dataFrame$meta,
            file = sprintf(
               fmt = "%s/%s_meta.csv",
               path,
               name
            ),
            row.names = FALSE
         );
      }
);

# Method SignalDataFrame$interpolate ####

#' @name SignalDataFrame_interpolate
#' 
#' @title 
#'   Interpolates a signal
#'   
#' @section Method of class:
#'   \code{\link{SignalDataFrame}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Signal_interpolate}} -
#'     See interface for documentation
#'   
SignalDataFrame$set(
   which = "public",
   name = "interpolate",
   value = function(time)
      {
         interpSignal <- SignalDataFrame$new();
         interpSignal$time <- time;
         interpSignal$dataFrame$copyMetaData(self$dataFrame);
         
         interpSignal$dataFrame$data <- data.frame(
            as.character(time),
            stringsAsFactors = FALSE
         );
         names(interpSignal$dataFrame$data) <- interpSignal$timeVariableName;
         
         for(column in 1:length(self$dataFrame$data)) {
            if (names(self$dataFrame$data)[column] != self$timeVariableName) {
               interpSignal$dataFrame$data[,names(self$dataFrame$data)[column]] <- 
                  approx(
                     x = self$time,
                     y = self$dataFrame$data[,column],
                     xout = interpSignal$time
                  )$y;
            }
         }
         
         return(interpSignal);
      }
);

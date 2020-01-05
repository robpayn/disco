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
#'   R6 Class representing a signal data frame
#'   
#' @description 
#'   This is an implementation of the Signal interface that is
#'   backed by a DataFrame object for the time series data
#'   
SignalDataFrame <- R6Class(
   classname = "SignalDataFrame",
   inherit = Signal,
   public = list(
      
      #' @field dataFrame
      #'   The data frame object containing the time series data
      dataFrame = NULL,
      
      #' @field timeVariableName
      #'   The name of the column in the data frame with the time data
      timeVariableName = NULL,
      
      # Method SignalDataFrame$new ####
      #
      #' @description 
      #'   Create an instance of class SignalDataframe
      #' 
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
               as.POSIXct(
                  self$dataFrame$data[[self$timeVariableName]], 
                  ...
               );
         }
      },
      
      # Static method SignalDataFrame$constructFromCSV ####
      #
      #' @description
      #'   Static method that constructs a signal based on a series of csv files providing
      #'   data and metadata bout the signal
      #'   
      #' @param fileName
      #'   The filename basis for the import.
      #' @param timeVariableName
      #'   The header in the csv data for the column with
      #'   time data.
      #'   Defaults to "time".
      #' @param ...
      #'   Abstract parameters that are passed to the class constructor
      #' 
      #' @return 
      #'   A new SignalDataframe object based on the provided csv files
      #' 
      constructFromCSV = function
      (
         fileName,
         timeVariableName = "time",
         ...
      )
      {
         
         return(
            SignalDataFrame$new(
               dataFrame = DataFrame$public_methods$constructFromCSV(fileName),
               timeVariableName = timeVariableName,
               ...
            )
         );
         
      },
      
      # Method SignalDataFrame$getVariable ####
      #
      #' @description 
      #'   Get the time series vector for the variable with the provided name
      #'   
      #' @param variableName
      #'   Name of the variable for which the data will be provided
      #'   
      #' @return 
      #'   The vector of values for the requested variable
      #'   
      getVariable = function(variableName) 
      {
         return(self$dataFrame$data[[variableName]]);
      },
      
      # Method SignalDataFrame$getWindow ####
      #
      #' @description 
      #'   Get a subset of the signal based on a provided window of time
      #' 
      #' @param minTime
      #'   The starting time of the window
      #' @param maxTime
      #'   The ending time of the window
      #'
      #' @return 
      #'   A new signal representing the subset of the current signal
      #'   
      getWindow = function(minTime, maxTime)
      {
         indices <-
            self$time > as.POSIXct(minTime) &
            self$time < as.POSIXct(maxTime);
         subSignal <- SignalDataFrame$new();
         subSignal$time <- self$time[indices];
         subSignal$dataFrame$data <- self$dataFrame$data[indices,];
         subSignal$dataFrame$copyMetaData(self$dataFrame);
         return(subSignal);
      },
      
      # Method SignalDataFrame$plotSummary ####
      #
      #' @description 
      #'   Plot a summary of the multivariate signal
      #'   
      #' @param x
      #'   Vector to plot on the x axis
      #'   Defaults to signal time variable
      #' @param mfrow
      #'   Value for graphing parameter controlling multipanel plots
      #'   Defaults to \code{c(length(self$dataFrame$data) - 1, 1)}
      #' @param mar
      #'   Value for the graphing parameter controling margins
      #'   Defaults to \code{c(4, 4, 1, 1) + 0.1}
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      plotSummary = function
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
      },
      
      
      # Method SignalDataFrame$plot ####
      #
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
      #' @param comp.colors
      #'   A vector of character strings specifying colors for additional
      #'   plots.
      #'   Defaults to a color blind friendly palette of purple, orange,
      #'   and green.
      #' @param comp.pchs
      #'   A vector of integers specifying the point symbol to use for 
      #'   additional plots.
      #'   Defaults to square, triangle, and diamond.
      #' @param ...
      #'   Any remaining arguments provided will be passed to the call to
      #'   the plot.default method, allowing for further customization of
      #'   the plot.
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      plot = function
      (
         variableName,
         x = self$time,
         timeZone = NA,
         y = self$dataFrame$data[[variableName]],
         xlim = c(min(x), max(x)),
         xlab = self$timeVariableName,
         ylim = c(
            min(y, na.rm = TRUE), 
            max(y, na.rm = TRUE)
         ),
         ylab = sprintf(
            "%s (%s)",
            variableName,
            self$dataFrame$metaColumns[variableName,]$units
         ),
         compareWith = list(),
         comp.colors = c(
            "purple",
            "orange",
            "green"
         ),
         comp.pchs = c(
            0,
            2,
            5
         ),
         ...
      )
      {
         if(!is.na(timeZone)) {
            attributes(x)$tzone <- timeZone;
            attributes(xlim)$tzone <- timeZone;
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
                  col = comp.colors[plotCount],
                  pch = comp.pchs[plotCount]
               );
            }
         }
      },
      
      # Method SignalDataFrame$addVariable ####
      #
      #' @description 
      #'   Add a variable to a signal
      #'   
      #' @param variableName
      #'   Name of the variable
      #' @param value
      #'   Vector of values for the variable
      #'   (must be the same length as the time series)
      #' @param units
      #'   Units of the variable
      #' @param dimensions
      #'   Dimensions of the variable
      #'   
      #' @return 
      #'   No defined return value
      #' 
      addVariable = function
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
      },
      
      # Method SignalDataFrame$writeCSV ####
      #
      #' @description 
      #'   Write the signal in csv format
      #'   
      #' @param path
      #'   The path to the written files
      #' @param name
      #'   The base name of the written files
      #' @param timeVariableName
      #'   Defaults to the time header attribute of the signal
      #' @param variables
      #'   A vector of variable names to specify the columns to be output.
      #'   Defaults to NULL which will output all columns.
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      writeCSV = function
      (
         path, 
         name,
         timeVariableName = self$timeVariableName,
         variables = NULL
      )
      {
         # Create the metacolumns dataframe for writing
         metaColumns <- self$dataFrame$metaColumns;
         metaColumns <- rbind(
            metaColumns,
            data.frame(
               property = timeVariableName,
               units = "text",
               dimensions = "Time",
               stringsAsFactors = FALSE
            )
         );
         row.names(metaColumns)[nrow(metaColumns)] <- timeVariableName;
         
         # Create the data dataframe for writing
         dataOut <- self$dataFrame$data;
         dataOut[, timeVariableName] <- as.character(self$time);
         
         # Adjust the output if variables argument is provided
         if(!is.null(variables)) {
            variables <- c(variables, timeVariableName);
            dataOut <- dataOut[,variables];
            metaColumns <- metaColumns[variables,];
         }
         
         write.csv(
            x = dataOut,
            file = sprintf(
               fmt = "%s/%s.csv",
               path,
               name
            ),
            row.names = FALSE
         );
         write.csv(
            x = metaColumns,
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
      },

      # Method SignalDataFrame$interpolate ####
      #      
      #' @description 
      #'   Interpolates a signal based on the provided time vector
      #' 
      #' @param time
      #'   Time vector providing the basis for the interpolation
      #'   
      #' @return 
      #'   A new signal with values interpolated from the current
      #'   signal based on the provided time vector
      #'   
      interpolate = function(time)
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

   )
)

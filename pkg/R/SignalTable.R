# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# SignalTable R6 Class ####

#' @export
#'
#' @title
#'   R6 Class representing a signal data frame
#'   
#' @description 
#'   This is an implementation of the Signal interface that is
#'   composed of a DataTable object for the time series data
#'   
SignalTable <- R6Class(
   classname = "SignalTable",
   inherit = Signal,
   public = list(
      
      #' @field table
      #'   The data frame object containing the time series data
      table = NULL,
      
      #' @field timeVariableName
      #'   The name of the column in the data frame with the time data
      timeVariableName = NULL,
      
      # Method SignalTable$initialize ####
      #
      #' @description 
      #'   Create an instance of class SignalTable
      #' 
      #' @param dataframe
      #'   Optional R S3 dataframe providing the data.
      #'   Defaults to NULL which allows a specific DataTable object to be supplied.
      #' @param meta
      #'   Optional R S3 dataframe of metadata.
      #'   Defaults to NULL.
      #' @param metaColumns
      #'   Optional R S3 dataframe of metadata for the columns in the signal.
      #'   Defaults to NULL
      #' @param table
      #'   The data table providing the basis of the signal.
      #'   Defaults to a new DataTable object based on the R S3 dataframe 
      #'   and metadata provided.
      #' @param timeVariableName
      #'   The property name for the table column with time data.
      #'   Defaults to "time". If the result of checking for a time column
      #'   returns NULL, then the time attribute will remain NULL after construction.
      #' @param ...
      #'   Arguments passed to as.POSIXct for coverting the time
      #'   data to a POSIXct vector. Note that the system time zone will be
      #'   assumed if a tz argument is not provided.
      #'   
      initialize = function(
         dataframe = NULL,
         meta = NULL,
         metaColumns = NULL,
         table = DataTable$new(
            dataframe = dataframe,
            meta = meta,
            metaColumns = metaColumns
         ),
         timeVariableName = "time",
         ...
      )
      {
         self$table <- table;
         self$timeVariableName <- timeVariableName;
         if (!is.null(self$table$data[[self$timeVariableName]])) {
            self$time <-
               as.POSIXct(
                  self$table$data[[self$timeVariableName]], 
                  ...
               );
         } 
      },
      
      # Method (static) SignalTable$constructFromCSV ####
      #
      #' @description
      #'   Static method that constructs a signal based on a series of csv files providing
      #'   data and metadata bout the signal
      #'   
      #' @param filePath
      #'   Path to the data file
      #' @param metaPath
      #'   Optional path to the metadata file.
      #'   Default is NULL.
      #' @param timeVariableName
      #'   The header in the csv data for the column with
      #'   time data.
      #'   Defaults to "time".
      #' @param ...
      #'   Abstract parameters that are passed to the class constructor
      #' 
      #' @return 
      #'   A new SignalTable object based on the provided csv files
      #' 
      constructFromCSV = function
      (
         filePath,
         metaPath = NULL,
         timeVariableName = "time",
         ...
      )
      {
         
         return(
            SignalTable$new(
               table = DataTable$public_methods$constructFromCSV(
                  filePath,
                  metaPath
               ),
               timeVariableName = timeVariableName,
               ...
            )
         );
         
      },
      
      # Method SignalTable$getVariable ####
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
         return(self$table$data[[variableName]]);
      },
      
      # Method SignalTable$getWindow ####
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
      #'   A new signal object representing the subset of the current signal
      #'   defined by the provided minimum and maximum times.
      #'   
      getWindow = function(minTime, maxTime)
      {
         indices <-
            self$time > as.POSIXct(minTime) &
            self$time < as.POSIXct(maxTime);
         subSignal <- SignalTable$new();
         subSignal$time <- self$time[indices];
         subSignal$table$data <- self$table$data[indices,];
         subSignal$table$copyMetaData(self$table);
         return(subSignal);
      },
      
      # Method SignalTable$plotSummary ####
      #
      #' @description 
      #'   Plot a summary of the multivariate signal
      #'   
      #' @param x
      #'   Vector to plot on the x axis
      #'   Defaults to signal time variable
      #' @param mfrow
      #'   Value for graphing parameter controlling multipanel plots
      #'   Defaults to \code{c(length(self$table$data) - 1, 1)}
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
         mfrow = c(length(self$table$data) - 1, 1),
         mar = c(4, 4, 1, 1) + 0.1
      )
      {
         par(
            mfrow = mfrow,
            mar = mar
         );
         for(variableName in names(self$table$data)) {
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
      
      
      # Method SignalTable$plot ####
      #
      #' @description 
      #'   Plots a single variable in the signal based on the provided header
      #'   for the variable
      #' 
      #' @param variableHeader
      #'   Name of the variable to plot on the y axis.  
      #' @param x
      #'   Variable plotted on x axis. 
      #'   Default value is the time.
      #' @param timeZone
      #'   If specified, the time zone for the datetime scale.
      #'   Defult value is NA (no change in time zone).
      #' @param y
      #'   Variable plotted on y axis. 
      #'   Default value is the values in the provided variableHeader argument.
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
         variableHeader,
         x = self$time,
         timeZone = NA,
         y = self$table$data[[variableHeader]],
         xlim = c(min(x), max(x)),
         xlab = self$timeVariableName,
         ylim = c(
            min(y, na.rm = TRUE), 
            max(y, na.rm = TRUE)
         ),
         ylab = sprintf(
            "%s (%s)",
            variableHeader,
            self$table$metaColumns["units", variableHeader]
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
                        min(x$getVariable(variableHeader)),
                        max(x$getVariable(variableHeader))
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
                  y = compareWith[[plotCount]]$getVariable(variableHeader),
                  col = comp.colors[plotCount],
                  pch = comp.pchs[plotCount]
               );
            }
         }
      },
      
      # Method SignalTable$addVariable ####
      #
      #' @description 
      #'   Add a variable to a signal
      #'   
      #' @param variableHeader
      #'   The header for the property name for the new variable
      #' @param metadata
      #'   The metadata for the column
      #' @param values
      #'   The vector of values for the new column
      #'   
      #' @return 
      #'   No defined return value
      #' 
      addVariable = function
      (
         variableHeader,
         metadata,
         values
      )
      {
         self$table$addColumn(
            header = variableHeader,
            metadata = metadata,
            values = values
         );
      },
      
      # Method SignalTable$writeCSV ####
      #
      #' @description 
      #'   Write the signal in csv format
      #'   
      #' @param path
      #'   The path to the written files
      #' @param name
      #'   The base name of the written files
      #' @param headers
      #'   Optional vector of variable names to specify the columns to be output.
      #'   Defaults to NULL which will output all columns.
      #' @param writeMetadata
      #'   Optional logical value to determine if metadata are written
      #'   Defaults to TRUE.
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      writeCSV = function
      (
         path, 
         name,
         headers = NULL,
         writeMetadata = TRUE
      )
      {
         # Initialize the metacolumns R S3 dataframe for writing
         metaColumns <- self$table$metaColumns;
         # Initialize the data R S3 dataframe for writing
         dataOut <- self$table$data;
         
         # Adjust the output if variables argument is provided
         if(!is.null(headers)) {
            dataOut <- dataOut[, headers];
            metaColumns <- metaColumns[, headers];
         }
         
         fileConn <- file(
            description = sprintf(
               fmt = "%s/%s.csv",
               path,
               name
            ), 
            open = "w"
         );
         writeLines(
            text = "# The following row contains the names of metadata rows that precede the data.",
            con = fileConn
         );
         writeLines(
            text = paste(
               shQuote(
                  string = c("header", rownames(metaColumns)), 
                  type = "cmd"
               ),
               collapse = ","
            ),
            con = fileConn
         );
         writeLines(
            text = c(
               "# The following rows contain the metadata for each column.",
               "# There will be one row of metadata corresponding to each name of a metadata row specified above."
            ),
            con = fileConn
         );
         write.table(
            x = metaColumns,
            file = fileConn,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE
         );
         writeLines(
            text = "# The following rows contain the variable headers and tabular data.",
            con = fileConn
         );
         write.table(
            x = dataOut,
            file = fileConn,
            sep = ",",
            row.names = FALSE
         );
         close(fileConn);
         
         if (writeMetadata && !is.null(self$table$meta)) {
            self$writeMetadata();
         }
      },
      
      # Method SignalTable$writeMetadata ####
      #      
      #' @description 
      #'   Writes the metadata object as an XML file
      #' 
      #' @return 
      #'   No defined return value
      #'   
      writeMetadata = function()
      {
         self$table$meta$writeXML();
      },

      # Method SignalTable$interpolate ####
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
         interpSignal <- SignalTable$new();
         interpSignal$time <- time;
         interpSignal$table$copyMetaData(self$table);
         
         interpSignal$table$data <- data.frame(
            as.character(time),
            stringsAsFactors = FALSE
         );
         names(interpSignal$table$data) <- interpSignal$timeVariableName;
         
         for(column in 1:length(self$table$data)) {
            if (names(self$table$data)[column] != self$timeVariableName) {
               interpSignal$table$data[,names(self$table$data)[column]] <- 
                  approx(
                     x = self$time,
                     y = self$table$data[,column],
                     xout = interpSignal$time
                  )$y;
            }
         }
         
         return(interpSignal);
      },
      
      # Method SignalTable$trim ####
      #      
      #' @description 
      #'   Trims the signal based on indices provided
      #' 
      #' @param firstIndex
      #'   Index that will become the first element of the signal
      #' @param lastIndex
      #'   Index that will become the last element of the signal
      #'   
      #' @return 
      #'   The new length of the signal
      #'   
      trim = function(firstIndex, lastIndex)
      {
         self$table$data <- self$table$data[firstIndex:lastIndex,];
         self$time <- self$time[firstIndex:lastIndex];
         return(self$getLength());
      },
      
      # Method SignalTable$trimEdges ####
      #      
      #' @description 
      #'   Trims the signal based on skipped rows
      #' 
      #' @param skipBeginning
      #'   Rows to skip at the beginning of the signal
      #' @param skipEnding
      #'   Rows to skip at the end of the signal
      #'   
      #' @return 
      #'   The new length of the signal
      #'   
      trimEdges = function
      (
         skipBeginning,
         skipEnding
      )
      {
         firstIndex <- 1 + skipBeginning;
         lastIndex <- self$getLength() - skipEnding;
         return(
            self$trim(firstIndex = firstIndex, lastIndex = lastIndex)
         );
      },
      
      # Method SignalTable$trimWindow ####
      #      
      #' @description 
      #'   Trims the signal based on removing a window
      #' 
      #' @param startIndex
      #'   First index of window to be removed
      #' @param endIndex
      #'   Last index of window to be removed
      #'   
      #' @return 
      #'   The new length of the signal
      #'   
      trimWindow = function
      (
         startIndex,
         endIndex
      )
      {
         
         if (startIndex < 2) {
            firstGroup = integer(length = 0);
         } else {
            firstGroup = 1:(startIndex - 1);
         }
         if (endIndex > self$getLength() - 1) {
            lastGroup = integer(length = 0);
         } else {
            lastGroup = (endIndex + 1):self$getLength();
         }
         self$table$data <- self$table$data[c(firstGroup, lastGroup),];
         self$time <- self$time[c(firstGroup, lastGroup)];
         return(self$getLength());
         
      }, # end method trimWindow()
      
      # Method SignalTable$filterOut ####
      #      
      #' @description 
      #'   Removes the rows designated by the provided indices
      #' 
      #' @param index
      #'   A vector of numerical or logical indices. The rows specified by
      #'   the numbers in the vector will be removed, or logical values of 'FALSE' 
      #'   in the vector will indicate that row should be removed removed.
      #'   
      #' @return 
      #'   The new length of the signal after rows are removed
      #'   
      filterOut = function
      (
         index
      )
      {
         if (is.integer(index) || is.numeric(index)) {
            logicalIndex <- !seq_len(self$getLength()) %in% as.integer(index);
         } else if (is.logical(index)) {
            logicalIndex <- index;
         } else {
            stop("SignalTable$filter can only use integer, numeric, or logical indices")
         }

         self$table$data <- self$table$data[logicalIndex, ];
         self$time <- self$time[logicalIndex];
         
         return(self$getLength());

      }, # end method filter()
      
      # Method SignalTable$getLength ####
      #      
      #' @description 
      #'   Gets the length of the signal
      #' 
      #' @return 
      #'   The length of the signal (number of rows in the
      #'   unerlying data table).
      #'   
      getLength = function()
      {
         return(nrow(self$table$data));
      },
      
      # Method SignalTable$writeRData ####
      #      
      #' @description 
      #'   Writes the signal as a list in an RData file
      #' 
      #' @param path
      #'   The path to the written file
      #' @param name
      #'   The base name of the written file
      #' @param timeVariableName
      #'   Optional name of the the time variable to write in the data
      #'   Defaults to NULL which will result in the time variable
      #'   not being written to the output.
      #' @param variables
      #'   Optional vector of variable names to specify the columns to be output.
      #'   Defaults to NULL which will output all columns.
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      writeRData = function
      (
         path, 
         name,
         timeVariableName = NULL,
         variables = NULL
      )
      {
         # Initiate the metacolumns dataframe for writing
         metaColumns <- self$table$metaColumns;
         # Initiate the data dataframe for writing
         dataOut <- self$table$data;
         
         # FIXME I am about to break this
         if (!is.null(timeVariableName)) {
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
            dataOut[, timeVariableName] <- as.character(self$time);
         }
         
         # Adjust the output if variables argument is provided
         if(!is.null(variables)) {
            variables <- c(variables, timeVariableName);
            dataOut <- dataOut[,variables];
            
            # FIXME I am about to break this.
            metaColumns <- metaColumns[variables,];
         }
         
         saveRDS(
            object = list(
               data = dataOut,
               data_meta_columns = metaColumns,
               data_meta = self$table$meta
            ),
            file = sprintf(
               fmt = "%s/%s.RData",
               path,
               name
            )
         );
      }
      
   )
)

# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# Signal R6 Class ####

#' @export
#'
#' @title
#'   R6 Class representing a signal data frame
#'   
#' @description 
#'   This is an implementation of the Signal interface that is
#'   composed of a DataTable object for the time series data
#'   
Signal <- R6Class(
   classname = "Signal",
   inherit = DataTable,
   public = list(
      
      #' @field timeHeader
      #'   The name of the column in the data frame with the time data
      timeHeader = NULL,
      timeFormat = NULL,

      # Method Signal$initialize ####
      #
      #' @description 
      #'   Create an instance of class Signal
      #' 
      #' @param x
      #'   R object indicating the source of information for constructing a signal.
      #'   If the class of the object is a character string, an signal will be attempted
      #'   to be constructed from a standard DataTable csv file.
      #' @param metaColumns
      #'   Optional data frame of metadata for the columns in the signal. Metadata from the
      #'   text file will be used if constructing from a file. Metadata must be provided if
      #'   constructing from a dataframe.
      #'   Defaults to NULL.
      #' @param timeHeader
      #'   Optional character string representing the header for the table column with time data.
      #'   Defaults to "time".
      #' @param timeFormat
      #'   Optional character string representing the format in "strptime" for the time column
      #'   that needs to be converted from a character string vector.
      #'   Defaults to <4 character year>:<2 character month of year>:<2 character day of month>
      #'   <24 hour time>:<2 character minutes of hour>:<2 character seconds of minute>
      #' @param timeZone
      #'   Optional system specific string for the time zone for the times column.
      #'   Defaults to "UTC"
      #' @param ...
      #'   Optional additional arguments passed to as.POSIXct for coverting the time
      #'   data to a POSIXct vector.
      #' 
      #' @return 
      #'   A reference to an object that is a new instance of the class
      #'   
      initialize = function
      (
         x,
         metaColumns = NULL,
         timeHeader = "time",
         timeFormat = "%Y-%m-%d %H:%M:%S",
         timeZone = "UTC",
         ...
      )
      {
         
         super$initialize(
            x = x,
            metaColumns = metaColumns
         );
         
         self$timeHeader <- timeHeader;
         self$timeFormat <- timeFormat;
         
         switch(
            class(x),
            character =
            {
               self$setVariable(
                  header = self$timeHeader,
                  values = as.POSIXct(
                     self$getVariable(self$timeHeader),
                     format = self$timeFormat,
                     tz = timeZone,
                     ...
                  )
               );
            },
            data.frame =
            {

            }
         );
                  
      },
      
      # Method Signal$getTime ####
      #
      #' @description 
      #'   Get the vector of values representing the times for the signal data
      #' 
      #' @return 
      #'   A vector of times.
      #'   
      getTime = function()
      {
         return(self$getVariable(self$timeHeader));
      },
      
      setTimeHeader = function(timeHeader)
      {
         names(self$data)[names(self$data) == self$timeHeader] <- timeHeader;
         names(self$metaColumns)[names(self$metaColumns) == self$timeHeader] <- timeHeader;
         self$timeHeader <- timeHeader;
      },
      
      # Method Signal$setTimeZone ####
      #
      #' @description 
      #'   Set the time zone
      #' 
      #' @param 
      #'   Character string representing the time zone
      #' 
      #' @return 
      #'   The previous setting for the time zone
      #'   
      setTimeZone = function(timeZone)
      {
         time <- self$getTime();
         prevTimeZone <- attributes(time)$tzone;
         attributes(time)$tzone <- timeZone;
         self$setVariable(
            header = self$timeHeader, 
            values = time
         );
         return(prevTimeZone);
      },
      
      filterVariables = function(headers)
      {
         if(!any(headers == self$timeHeader)) {
            headers <- c(headers, self$timeHeader)
         }
         return(super$filterVariables(headers = headers));
      },
      
      # Method Signal$getWindow ####
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
         time <- self$getTime();
         indices <-
            time > minTime &
            time < maxTime;
         signal <- Signal$new(
            x = self$data[indices, ],
            metaColumns = self$metaColumns,
            timeHeader = self$timeHeader,
            timeFormat = self$timeFormat
         );
         return(signal);
      },
      
      # Method Signal$plotSummary ####
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
         mar = c(4, 4, 1, 1) + 0.1,
         mfrow = NULL,
         headers = character()
      )
      {
         if(length(headers) == 0) {
            for(header in names(self$data)) {
               if (class(self$getVariable(header))[1] == "numeric") {
                  headers <- c(headers, header);
               }
            };
         } 
         
         par(
            mar = mar
         );
         if (is.null(mfrow)) {
            par(mfrow = c(length(headers), 1));
         }
         
         for(header in headers) {
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
      },
      
      
      # Method Signal$plot ####
      #
      #' @description 
      #'   Plots a single variable in the signal based on the provided header
      #'   for the variable
      #' 
      #' @param header
      #'   Name of the variable to plot on the y axis.  
      #' @param x
      #'   Variable plotted on x axis. 
      #'   Default value is the time.
      #' @param timeZone
      #'   If specified, the time zone for the datetime scale.
      #'   Defult value is NA (no change in time zone).
      #' @param y
      #'   Variable plotted on y axis. 
      #'   Default value is the values in the provided header argument.
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
         header,
         x = self$getTime(),
         timeZone = NA,
         y = self$getVariable(header),
         xlim = c(min(x), max(x)),
         xlab = self$timeHeader,
         ylim = c(
            min(y, na.rm = TRUE), 
            max(y, na.rm = TRUE)
         ),
         ylab = sprintf(
            "%s (%s)",
            header,
            self$metaColumns["units", header]
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
                        min(x$getVariable(header)),
                        max(x$getVariable(header))
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
               time <- compareWith[[plotCount]]$getTime();
               if(!is.na(timeZone)) {
                  attributes(time)$tzone <- timeZone;
               }
               points(
                  x = time, 
                  y = compareWith[[plotCount]]$getVariable(header),
                  col = comp.colors[plotCount],
                  pch = comp.pchs[plotCount]
               );
            }
         }
      },
      
      # Method Signal$writeCSV ####
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
      
      # Method Signal$writeMetadata ####
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

      # Method Signal$interpolate ####
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
         interpSignal <- Signal$new();
         interpSignal$time <- time;
         interpSignal$table$copyMetaData(self$table);
         
         interpSignal$table$data <- data.frame(
            as.character(time),
            stringsAsFactors = FALSE
         );
         names(interpSignal$table$data) <- interpSignal$timeHeader;
         
         for(column in 1:length(self$table$data)) {
            if (names(self$table$data)[column] != self$timeHeader) {
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
      
      # Method Signal$trim ####
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
      
      # Method Signal$trimEdges ####
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
      
      # Method Signal$trimWindow ####
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
      
      # Method Signal$filterOut ####
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
            stop("Signal$filter can only use integer, numeric, or logical indices")
         }

         self$table$data <- self$table$data[logicalIndex, ];
         self$time <- self$time[logicalIndex];
         
         return(self$getLength());

      }, # end method filter()
      
      # Method Signal$writeRData ####
      #      
      #' @description 
      #'   Writes the signal as a list in an RData file
      #' 
      #' @param path
      #'   The path to the written file
      #' @param name
      #'   The base name of the written file
      #' @param timeHeader
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
         timeHeader = NULL,
         variables = NULL
      )
      {
         # Initiate the metacolumns dataframe for writing
         metaColumns <- self$table$metaColumns;
         # Initiate the data dataframe for writing
         dataOut <- self$table$data;
         
         # FIXME I am about to break this
         if (!is.null(timeHeader)) {
            metaColumns <- rbind(
               metaColumns,
               data.frame(
                  property = timeHeader,
                  units = "text",
                  dimensions = "Time",
                  stringsAsFactors = FALSE
               )
            );
            row.names(metaColumns)[nrow(metaColumns)] <- timeHeader;
            dataOut[, timeHeader] <- as.character(self$time);
         }
         
         # Adjust the output if variables argument is provided
         if(!is.null(variables)) {
            variables <- c(variables, timeHeader);
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

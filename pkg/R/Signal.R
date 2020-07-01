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

      #' @field timeFormat
      #'   Charater string with the time format (strptime)
      timeFormat = NULL,

      # Method Signal$initialize ####
      #
      #' @description 
      #'   Create an instance of class Signal
      #' 
      #' @param x
      #'   R object indicating the source of information for constructing a signal.
      #'   If the class of the object is a character string, a signal will be constructed from 
      #'   a standard DataTable csv file. If the class of the object is a data frame, a signal
      #'   will be constructed from the data frame. Note that the constructor will try to turn
      #'   the time column into a POSIXct if it is not already that type (based on the provided
      #'   time format and time zone).
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
               if(class(self$data[[timeHeader]])[1] != "POSIXct") {
                  self$data[[timeHeader]] <- as.POSIXct(
                     self$data[[timeHeader]],
                     format = timeFormat,
                     tz = timeZone
                  );
               }
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
      
      # Method Signal$setTimeHeader ####
      #
      #' @description 
      #'   Set the time header
      #' 
      #' @param timeHeader
      #'   Character string representing the new time header to use
      #' 
      #' @return 
      #'   The old value of the time header
      #'   
      setTimeHeader = function(timeHeader)
      {
         oldHeader <- self$timeHeader;
         names(self$data)[names(self$data) == self$timeHeader] <- timeHeader;
         names(self$metaColumns)[names(self$metaColumns) == self$timeHeader] <- timeHeader;
         self$timeHeader <- timeHeader;
         return(oldHeader);
      },
      
      # Method Signal$setTimeZone ####
      #
      #' @description 
      #'   Set the time zone
      #' 
      #' @param timeZone
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
      
      # Method Signal$getDataframe ####
      #
      #' @description 
      #'   Get the signal in a data frame format
      #' 
      #' @return 
      #'   Copy of the signal data in data frame format
      #'   
      getDataframe = function()
      {
         return(self$data);
      },
      
      # Method Signal$getWindow ####
      #
      #' @description 
      #'   Get a subset of the signal based on a provided window of time
      #' 
      #' @param minTime
      #'   POSIXct object with the starting time of the window
      #' @param maxTime
      #'   POSIXct object with the ending time of the window
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
      interpolate = function(
         headers, 
         time,
         timeMeta = NULL,
         ...
      )
      {
         
         if(is.null(timeMeta)) {
            timeMeta <- self$metaColumns[[self$timeHeader]];
         }
         
         interpSignal <- Signal$new(
            x = data.frame(time = time),
            timeHeader = "time",
            metaColumns = data.frame(
               timeMeta,
               row.names = row.names(self$metaColumns)
            )
         );
         for (header in headers) {
            interpSignal$setVariable(
               header = header,
               values = approx(
                  x = self$getTime(),
                  y = self$getVariable(header),
                  xout = time,
                  ...
               )$y,
               metadata = self$metaColumns[[header]]
            );
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
         self$data <- self$data[firstIndex:lastIndex,];
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
      
      # Method Signal$filterVariables ####
      #
      #' @description 
      #'   Filter the signal to the desired variables
      #' 
      #' @param headers
      #'   Vector of character strings representing the headers for
      #'   variable to keep. All other variables will be excluded, 
      #'   escept the variable with time.
      #' 
      #' @return 
      #'   Value returned by the super$filterVariables method
      #'   
      filterVariables = function(headers)
      {
         if(!any(headers == self$timeHeader)) {
            headers <- c(headers, self$timeHeader)
         }
         return(super$filterVariables(headers = headers));
      },
      
      # Method Signal$filterRows ####
      #      
      #' @description 
      #'   Removes the rows designated by the provided indices
      #' 
      #' @param index
      #'   A vector of numerical or logical indices. The rows specified by
      #'   the numbers in the vector will be removed, or logical values of 'FALSE' 
      #'   in the vector will indicate that row should be removed removed.
      #' @param out
      #'   Optional logical value indicating if filter is in the "out" or "in" direction.
      #'   Defaluts to "TRUE", which will remove the rows with the provided indices. Setting
      #'   to "FALSE" will remove all rows except those with the provided indices.
      #'   
      #' @return 
      #'   The new length of the signal after rows are removed
      #'   
      filterRows = function
      (
         index,
         out = TRUE
      )
      {
         if (is.integer(index) || is.numeric(index)) {
            logicalIndex <- !seq_len(self$getLength()) %in% as.integer(index);
         } else if (is.logical(index)) {
            logicalIndex <- index;
         } else {
            stop("Signal$filter can only use integer, numeric, or logical indices")
         }
         
         if(!out) {
            logicalIndex <- !logicalIndex;
         }

         self$data <- self$data[logicalIndex, ];

         return(self$getLength());

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
         headers = NULL
      )
      {
         # Initialize the metacolumns R S3 dataframe for writing
         metaColumns <- self$metaColumns;
         # Initialize the data R S3 dataframe for writing
         dataOut <- self$data;
         
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
      #' @param headers
      #'   Vector of character strings specifying the headers for which
      #'   variables to summarize.
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
                  time <- x$getTime();
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
      }
      
   )
)

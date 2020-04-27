# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# R6 Class DataFileCampbell2019 ####

#
#' @export
#'
#' @title 
#'   A campbell data file class (R6 Class)
#'
#' @description 
#'   Provides utilities for managing data in a Campbell Scientific 
#'   datalogger file, with the file structure typical in 2019.
#'
DataFileCampbell2019 <- R6Class(
   classname = "DataFileCampbell2019",
   public = list(
      
      #' @field filePath
      #'   character string representing the path to the file
      filePath = NULL,
      
      #' @field timeZone
      #'   character string representing the time zone (system dependent)
      timeZone = NULL,

      #' @field signal
      #'   signal containing data from the file
      signal = NULL,
      
      #' @field timeColumnIndex
      #'   an integer representing the index of the time column in the data file
      timeColumnIndex = NULL,

      #' @field numMetaRows
      #'   integer representing the number of rows of metadata at the top
      #'   of the file
      numMetaRows = NULL,
      
      #' @field columnNames
      #'   vector of character strings representing the column names
      columnNames = NULL,
      
      # Method DataFileCampbell2019$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class DataFileCampbell2019
      #'   
      #' @param filePath
      #'   Character string representing the path to the file
      #' @param timeZone
      #'   Optional character string representing the time zone.
      #'   Defaults to "UTC".
      #' @param columnNames
      #'   Optional vector of character strings representing the column names.
      #'   Defaults to NULL.
      #' @param numMetaRows
      #'   Optional integer representing the number of rows of metadata at the top
      #'   of the file
      #'   Defaults to 4.
      #' @param timeColumnIndex
      #'   Optional integer representing the index of the time column in the data.
      #'   Defaults to 1.
      #'   
      initialize = function
      (
         filePath,
         timeZone = "UTC",
         columnNames = NULL,
         numMetaRows = 4,
         timeColumnIndex = 1
      ) 
      {
         self$filePath <- filePath;
         self$timeZone <- timeZone;
         self$columnNames <- columnNames;
         self$numMetaRows <- numMetaRows;
         self$timeColumnIndex <- timeColumnIndex;
      },
      
      # Method DataFileCampbell2019$read ####
      #
      #' @description 
      #'   Read the data file to populate the signal attribute
      #'   
      #' @param meta
      #'   R S3 dataframe of metadata.
      #'   Defaults to NULL.
      #' @param metaColumns
      #'   R S3 dataframe of metadata for the columns in the signal.
      #'   Defaults to NULL
      #' @param stringsAsFactors
      #'   Optional logical value to turn strings as factors on or off.
      #'   Default value is FALSE (do not interpret strings as categorical factors).
      #'   
      #' @return 
      #'   The number of data rows read from the file. 
      #'   
      read = function
      (
         meta,
         metaColumns,
         stringsAsFactors = FALSE
      ) 
      {
         data <- read.csv(
            file = self$filePath,
            skip = self$numMetaRows,
            header = FALSE,
            stringsAsFactors = stringsAsFactors
         );
         if (!is.null(self$columnNames)) {
            names(data) <- self$columnNames;
         }
         
         self$signal <- SignalTable$new(
            data = data,
            meta = meta,
            metaColumns = metaColumns,
            tz = self$timeZone
         );
         
         return(nrow(data));
      }
      
   )
)

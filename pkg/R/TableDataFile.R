# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# R6 Class TableDataFile ####

#
#' @export
#'
#' @title 
#'   A class for tabular data in a text data file (R6 Class)
#'
#' @description 
#'   Provides utilities for managing tabular data stored in
#'   a text file using column delimiters
#'
TableDataFile <- R6Class(
   classname = "TableDataFile",
   public = list(
      
      ## Attributes ####
      
      #' @field instrument
      #'   Character string identifying the instrument used.
      instrument = NULL,
      
      #' @field filePath
      #'   character string representing the path to the file
      filePath = NULL,
      
      #' @field delimiter
      #'   Character string representing the delimiter for columns
      delimiter = NULL,
      
      #' @field timeZone
      #'   character string representing the time zone (system dependent)
      timeZone = NULL,

      #' @field timeFormat
      #'   character string representing the time format (strptime)
      timeFormat = NULL,
      
      #' @field timeVariableName
      #'   character string representing the time variable header in the table
      timeVariableName = NULL,
      
      #' @field signal
      #'   signal containing data from the file
      signal = NULL,
      
      #' @field numMetaRows
      #'   integer representing the number of rows of metadata at the top
      #'   of the file
      numMetaRows = NULL,
      
      #' @field metaColumns
      #'   Data frame with context for each column
      metaColumns = NULL,

      #' @field metadata
      #'   Data frame with context for each column
      metadata = NULL,
      
      ## Method: constructor ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #' 
      #' @param instrument
      #'   Character string identifying the instrument used  
      #' @param filePath
      #'   Character string representing the path to the file
      #' @param timeVariableName
      #'   Character string representing the time variable header in the table.
      #' @param numMetaRows
      #'   Integer representing the number of rows of metadata at the top
      #'   of the file
      #' @param metaColumns
      #'   Data frame with context for each column.
      #' @param delimiter
      #'   Character string representing the delimiter for columns
      #' @param timeZone
      #'   Character string representing the time zone.
      #' @param timeFormat
      #'   character string representing the time format (strptime)
      #' @param metadata
      #'   Optional metadata object.
      #'   Defaults to NULL
      #'   
      initialize = function
      (
         instrument,
         filePath,
         timeVariableName,
         numMetaRows,
         metaColumns,
         delimiter,
         timeZone,
         timeFormat,
         metadata = NULL
      ) 
      {
         self$instrument <- instrument;
         self$filePath <- filePath;
         self$delimiter <- delimiter;
         self$timeZone <- timeZone;
         self$timeFormat <- timeFormat;
         self$timeVariableName <- timeVariableName;
         self$numMetaRows <- numMetaRows;
         self$metaColumns <- metaColumns;
         self$metadata <- metadata;
      },
      
      ## Method: read ####
      #
      #' @description 
      #'   Read the data file to populate the signal attribute
      #'   
      #' @param dataLayer
      #'   Optional: data layer
      #'   Default value is "root".
      #' @param stringsAsFactors
      #'   Optional: logical value to turn strings as factors on or off.
      #'   Default value is FALSE (do not interpret strings as categorical factors).
      #'   
      #' @return 
      #'   The signal object created by reading the file.
      #'   
      read = function
      (
         dataLayer = "root",
         stringsAsFactors = FALSE
      ) 
      {
         data <- read.table(
            file = self$filePath,
            sep = self$delimiter,
            skip = self$numMetaRows,
            header = FALSE,
            col.names = names(self$metaColumns),
            stringsAsFactors = stringsAsFactors
         );
         
         if (!is.null(self$metadata)) {
            self$addMetadata(
               metadata = self$metadata,
               instrument = self$instrument
            );
         }

         self$metaColumns["dataLayer",] <- dataLayer;
         
         self$signal <- SignalTable$new(
            data = data,
            meta = self$metadata,
            metaColumns = self$metaColumns,
            timeVariableName = self$timeVariableName,
            timeFormat = self$timeFormat,
            tz = self$timeZone
         );
         
         return(self$signal);
      },
      
      ## Method: addMetadata ####
      #
      #' @description 
      #'   Must be implemented by extending classes
      #'   
      #' @param metadata
      #'   Metadata
      #' @param instrument
      #'   Instrument
      #'   
      addMetadata = function(metadata, instrument)
      {
         stop("Function TableDataFile$addMetadata has not been implemented by the subclass.");
      }
      
   )
)

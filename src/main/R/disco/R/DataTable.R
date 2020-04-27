# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# DataTable R6 Class ####
#
#' @export
#'
#' @title 
#'   An tabular data set with metadata
#'
#' @description 
#'   An R6 implementation of a data table, including metadata
#'   for the data in the table
#'
DataTable <- R6Class(
   classname = "DataTable",
   public = list(
      
      #' @field data
      #'   the S3 dataframe underlying the data structure
      data = NULL,
      
      #' @field meta 
      #'   the S3 dataframe containing the general metadata
      meta = NULL,
      
      #' @field metaColumns
      #'   the S3 dataframe containing the column metadata
      metaColumns = NULL,
      
      # Method DataTable.initialize ####
      #
      #' @description 
      #'   Construct a new instance of a data table
      #' 
      #' @param dataframe
      #'   An R S3 dataframe object with the data.
      #' @param meta
      #'   Optional R S3 dataframe with general metadata.
      #'   Default value is NULL.
      #' @param metaColumns
      #'   Optional RS3 dataframe with column-specific metadata.
      #'   Default value is NULL.
      #'   
      initialize = function(
         dataframe,
         meta = NULL,
         metaColumns = NULL
      )
      {
         self$data <- dataframe;
         self$meta <- meta;
         self$metaColumns <- metaColumns;
      },
      
      # Method (static) DataTable.constructFromCSV ####
      #
      #' @description 
      #'   Static method for creating a DataTable object 
      #'   based on csv formatted information
      #' 
      #' @param fileName
      #'   Base name of the files containing the csv data and metadata
      #'   
      constructFromCSV = function(fileName)
      {
         
         dataTable <- DataTable$new();
         dataTable$data <- read.csv(
            file = paste(
               fileName,
               ".csv",
               sep = ""
            ),
            header = TRUE,
            stringsAsFactors = FALSE
         );
         dataTable$meta <- read.csv(
            file = paste(
               fileName,
               "_meta.csv",
               sep = ""
            ),
            header = TRUE,
            stringsAsFactors = FALSE
         );
         rownames(dataTable$meta) <- dataTable$meta$key;
         # FIXME I am about to break this
         dataTable$metaColumns <- read.csv(
            file = paste(
               fileName,
               "_meta_columns.csv",
               sep = ""
            ),
            header = TRUE,
            stringsAsFactors = FALSE
         );
         rownames(dataTable$metaColumns) <- dataTable$metaColumns$property;
         return(dataTable);
         
      },
      
      # Method DataTable$copyMetaData ####
      #
      #' @description 
      #'   Copies the metadata from the provided data table 
      #'   object to this data table object
      #'   
      #' @param dataTable
      #'   The data table object from which the metadata is copied
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      copyMetaData = function(dataTable)
      {
         self$meta <- dataTable$meta;
         self$metaColumns <- dataTable$metaColumns;
      },
      
      # Method DataTable$addColumn ####
      #
      #' @description 
      #'   Adds a column of data and associated metata 
      #'   as a variable to the data table
      #'   
      #' @param header
      #'   The header for the property name for the new column
      #' @param metadata
      #'   The metadata for the column
      #' @param values
      #'   The vector of values for the new column
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      addColumn = function
      (
         header,
         metadata,
         values
      )
      {
         if (length(metadata) != nrow(self$metaColumns)) {
            stop("Number of elements in metadata vector does not match length of metadata required.")
         }
         self$data[, header] <- values;
         self$metaColumns[, header] <- metadata;
      }
      
   )
)

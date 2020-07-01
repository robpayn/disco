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
      #' @param filePath
      #'   Path to the data file
      #' @param metaPath
      #'   Optional path to the metadata file.
      #'   Defaults to NULL.
      #'   
      constructFromCSV = function
      (
         filePath,
         metaPath = NULL
      )
      {
         con <- file(
            description = filePath,
            open = "r"
         );
         
         metaNames <- as.character(read.table(
            file = con,
            sep = ",",
            nrows = 1,
            comment.char = "#",
            stringsAsFactors = FALSE
         )[1,]);
         
         headers <- as.character(read.table(
            file = con,
            sep = ",",
            nrows = 1,
            comment.char = "#",
            stringsAsFactors = FALSE
         )[1,]);
         
         metaColumns <- read.table(
            file = con,
            sep = ",",
            nrows = length(metaNames) - 1,
            col.names = headers,
            row.names = metaNames[2:length(metaNames)],
            stringsAsFactors = FALSE
         );
         
         data <- read.table(
            file = con,
            sep = ",",
            header = TRUE,
            comment.char = "#",
            stringsAsFactors = FALSE
         )
         
         close(con);

         if (!is.null(metaPath)) {
            meta <- Metadata$new(
               path = metaPath
            );
            meta$readXML();
         } else {
            meta <- null;
         }
         
         return(
            DataTable$new(
               dataframe = data,
               meta = meta,
               metaColumns = metaColumns
            )
         );
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

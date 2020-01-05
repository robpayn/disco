# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# DataFrame R6 Class ####
#
#' @export
#'
#' @title 
#'   R6 Class representing a dataframe
#'
#' @description 
#'   An R6 implementation of a data frame, including metadata
#'   for the data in the data frame
#'
DataFrame <- R6Class(
   classname = "DataFrame",
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
      
      # DataFrame.constructFromCSV static method ####
      #
      #' @description 
      #'   Static method for creating a DataFrame object 
      #'   based on csv formatted information
      #' 
      #' @param fileName
      #'   Base name of the files containing the csv data and metadata
      #'   
      constructFromCSV = function(fileName)
      {
         
         dataFrame <- DataFrame$new();
         dataFrame$data <- read.csv(
            file = paste(
               fileName,
               ".csv",
               sep = ""
            ),
            header = TRUE,
            stringsAsFactors = FALSE
         );
         dataFrame$meta <- read.csv(
            file = paste(
               fileName,
               "_meta.csv",
               sep = ""
            ),
            header = TRUE,
            stringsAsFactors = FALSE
         );
         rownames(dataFrame$meta) <- dataFrame$meta$key;
         dataFrame$metaColumns <- read.csv(
            file = paste(
               fileName,
               "_meta_columns.csv",
               sep = ""
            ),
            header = TRUE,
            stringsAsFactors = FALSE
         );
         rownames(dataFrame$metaColumns) <- dataFrame$metaColumns$property;
         return(dataFrame);
         
      },
      
      # Method DataFrame$copyMetaData ####
      #
      #' @description 
      #'   Copies the metadata from the provided data frame 
      #'   object to this data frame object
      #'   
      #' @param dataFrame
      #'   The data frame object from which the metadata is copied
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      copyMetaData = function(dataFrame)
      {
         self$meta <- dataFrame$meta;
         self$metaColumns <- dataFrame$metaColumns;
      },
      
      # Method DataFrame$addColumn ####
      #
      #' @description 
      #'   Adds a column of data and associated metata 
      #'   as a variable to the data frame
      #'   
      #' @param property
      #'   The property name for the new column
      #' @param value
      #'   The vector of values for the new column
      #' @param units
      #'   The units for the values for the new column
      #' @param dimensions
      #'   The dimensions of the variable for the new column
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      addColumn = function
      (
         property,
         value,
         units,
         dimensions
      )
      {
         self$data[, property] <- value;
         self$metaColumns[property, "property"] <- property;
         self$metaColumns[property, "units"] <- units;
         self$metaColumns[property, "dimensions"] <- dimensions;
      }
      
   )
)

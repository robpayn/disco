# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#'
NULL

# DataFrame R6 Class ####

#' @export
#'
#' @title
#'   Data frame with metadata
#' 
#' @description 
#'   An R6 implementation of a data frame including metadata
#'   for the data in the data frame
#'
#' @section Static methods:
#'   \itemize{
#'     \item \code{$constructFromCSV} -
#'       See \code{\link{DataFrame_constructFromCSV}}
#'   }
#' @section Methods:
#'   \itemize{
#'     \item \code{$copyMetaData} -
#'       See \code{\link{DataFrame_copyMetaData}}
#'     \item \code{$addColumn} -
#'       See \code{\link{DataFrame_addColumn}}
#'   }
#'   
DataFrame <- R6Class(
   classname = "DataFrame",
   public = list(
      data = NULL,
      meta = NULL,
      metaColumns = NULL
   )
);

# DataFrame.constructFromCSV static method ####

#' @name DataFrame_constructFromCSV
#'
#' @title
#'   Static method for creating a DataFrame object from CSV files
#'   
#' @description 
#'   Creates a DataFrame object based on csv formatted information
#' 
#' @param fileName
#'   Base name of the files containing the csv data and metadata
#' 
#' @return 
#'   A reference to the new object
#'
#' @section Static method of class:
#'   \code{\link{DataFrame}}
#'   
DataFrame$constructFromCSV <- function(fileName)
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

};

# Method DataFrame$copyMetaData ####

#' @name DataFrame_copyMetaData
#'
#' @title
#'   Copy metadata from another DataFrame object
#'   
#' @description 
#'   Copies the metadata from the provided data frame 
#'   object to this data frame object
#'   
#' @param dataFrame
#'   The data frame from which the metadata should be
#'   copied
#'
#' @section Method of class:
#'   \code{\link{DataFrame}}
#'   
DataFrame$set(
   which = "public",
   name = "copyMetaData",
   value = function
      (
         dataFrame
      )
      {
         self$meta <- dataFrame$meta;
         self$metaColumns <- dataFrame$metaColumns;
      }
);

# Method DataFrame$addColumn ####

#' @name DataFrame_addColumn
#' 
#' @title 
#'   Add a column to the data frame
#'   
#' @description 
#'   Adds a column of data and associated metata 
#'   as a variable to the data frame
#'   
#' @param property
#'   The property name for the new columne
#' @param value
#'   The vector of values for the new column
#' @param units
#'   The units for the values for the new column
#' @param dimensions
#'   The dimensions of the variable for the new column
#' 
#' @section Method of class:
#'   \code{\link{DataFrame}}
#'   
DataFrame$set(
   which = "public",
   name = "addColumn",
   value = function
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
);

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
#' @section Methods:
#'   Static:\cr
#'   \code{$constructFromCSV} -
#'     See \code{\link{DataFrame_constructFromCSV}}
#'
#'   \code{$new}\cr
#'   \code{$copyMetaData} -
#'     See \code{\link{DataFrame_copyMetaData}}
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
#' @section Method of class:
#'   \code{\link{DataFrame}}
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
#' @section Method of class:
#'   \code{\link{DataFrame}}
DataFrame$set(
   which = "public",
   name = "copyMetaData",
   value = function(dataFrame)
      {
         self$meta <- dataFrame$meta;
         self$metaColumns <- dataFrame$metaColumns;
      }
);

# Method DataFrame$addColumn ####

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

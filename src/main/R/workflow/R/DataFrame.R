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
DataFrame$constructFromCSV <- function(dataFrame, fileName)
{

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

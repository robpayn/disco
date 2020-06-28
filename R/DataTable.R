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
      
      #' @field metaColumns
      #'   the S3 dataframe containing the column metadata
      metaColumns = NULL,
      
      # Method DataTable.initialize ####
      #
      #' @description 
      #'   Construct a new instance of a data table
      #' 
      #' @param x
      #'   R object indicating the source of information for constructing a data table.
      #'   If the class of the object is a character string, an data table will be attempted
      #'   to be constructed from a standard DataTable csv file.
      #' @param metaColumns
      #'   Optional data frame of metadata for the columns in the data table. Metadata from the
      #'   text file will be used if constructing from a file. Metadata must be provided if
      #'   constructing from a dataframe.
      #'   Defaults to NULL.
      #'   
      initialize = function
      (
         x,
         metaColumns = NULL
      )
      {
         switch(
            class(x),
            character = 
            {
               
               con <- file(
                  description = x,
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
               
               self$metaColumns <- read.table(
                  file = con,
                  sep = ",",
                  nrows = length(metaNames) - 1,
                  col.names = headers,
                  row.names = metaNames[2:length(metaNames)],
                  stringsAsFactors = FALSE
               );
               
               self$data <- read.table(
                  file = con,
                  sep = ",",
                  header = TRUE,
                  comment.char = "#",
                  stringsAsFactors = FALSE
               )
               
               close(con);

            },
            data.frame = 
            {
               self$data <- x;
               if(is.null(metaColumns)) {
                  stop("Argument 'metaColumns' must not be NULL if a dataframe is used to construct a data table.")
               }
            }
         );
         
         if(!is.null(metaColumns)) {
            self$metaColumns <- metaColumns;
         }
         
      },
      
      # Method DataTable$getLength ####
      #      
      #' @description 
      #'   Gets the length of the data table
      #' 
      #' @return 
      #'   The number of rows in the data table
      #'   
      getLength = function()
      {
         return(nrow(self$data));
      },
      
      # Method DataTable$getMetaColumn ####
      #      
      #' @description 
      #'   Gets the metadata for a single variable
      #'   
      #' @param header
      #'   Character string representing the header for the metadata to get
      #' 
      #' @return 
      #'   Vector of character strings containing the column metdata
      #'   
      getMetaColumn = function(header)
      {
         return(self$metaColumns[[header]]);
      },
      
      # Method DataTable$getMetaColumns ####
      #      
      #' @description 
      #'   Gets the metadata for a collection of variables.
      #'   
      #' @param headers
      #'   Optional vector of character strings representing the headers for the metadata to get.
      #'   Defaults to returning metdata for all columns.
      #' 
      #' @return 
      #'   Data frame containing the metdata for the specificed columns
      #'   
      getMetaColumns = function(
         headers = names(self$metaColumns)
      )
      {
         if (length(headers) == 0) {
            return(NULL);
         }
         metaColumns <- self$metaColumns[, headers];
         if (length(headers) == 1) {
            metaColumns <- data.frame(
               metaColumns,
               row.names = row.names(self$metaColumns)
            );
         }
         return(metaColumns);
      },
      
      # Method DataTable$getVariable ####
      #      
      #' @description 
      #'   Gets the vector of values for a variable in the table
      #' 
      #' @param header
      #'   Charater string representing the header for the variable values to get
      #' 
      #' @return 
      #'   Vector of values corresponding to the provided header
      #'   
      getVariable = function(header)
      {
         return(self$data[[header]]);
      },
      
      # Method DataTable$setVariable ####
      #
      #' @description 
      #'   Adds a column of data and associated metata 
      #'   as a variable to the data table
      #'   
      #' @param header
      #'   The header for the property name for the new column
      #' @param values
      #'   The vector of values for the new column
      #' @param metadata
      #'   Optional list of values representing the metadata for the column.
      #'   Defaults to the existing metadata if column already exists.
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      setVariable = function
      (
         header,
         values,
         metadata = self$metaColumns[[header]]
      )
      {
         if(is.null(self$data[[header]]) && is.null(metadata)) {
            stop("DataTable$setVariable must provide metadata for a variable that does not exist.")
         }
         self$data[[header]] <- values;
         self$metaColumns[[header]] <- metadata;
      },
      
      # Method DataTable$filterVarialbes ####
      #
      #' @description 
      #'   Removes all variables except those specified 
      #'   
      #' @param headers
      #'   Vector of character strings representing the variables to keep
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      filterVariables = function(headers)
      {
         self$data <- self$data[, headers];
         self$metaColumns <- self$metaColumns[, headers];
         return(NULL);
      }
      
   )
)

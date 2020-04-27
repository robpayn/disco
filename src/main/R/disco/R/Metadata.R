# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#' @import xml2 
#'
NULL

# R6 Class Metadata ####

#
#' @export
#'
#' @title 
#'   Metadata for a data product (R6 Class)
#'
#' @description 
#'   Provides utilities for managing metadata for a data product
#'
Metadata <- R6Class(
   classname = "Metadata",
   public = list(
      path = NULL,
      xml = NULL,
      currentLayer = NULL,
      
      # Constructor Metadata$initialize() ####
      
      initialize = function(path, baseLayerName = NULL)
      {
         self$path <- path;
         if (!is.null(baseLayerName)) {
            self$createLayerWithMetadata(name = baseLayerName);
         }
      },
      
      # Method Metadata$createLayerWithMetadata() ####
      
      createLayerWithMetadata = function(
         layer = NULL,
         name
      )
      {
         if (is.null(layer)) {
            if (is.null(self$xml)) {
               self$xml <- as_xml_document(
                  x = list(dataLayer = structure(
                     list(metadata = list(
                        who = list(),
                        what = list(),
                        when = list(),
                        where = list(),
                        how = list(),
                        why = list()
                     )),
                     name = name
                  ))
               );
               self$currentLayer <- self$xml;
            } else {
               # PLANNED
               stop("Cannot add a layer to a layer, yet.");
            }
         } else {
            # PLANNED
            stop("Cannot add a layer to a layer, yet.");
         }
      }, # End createLayerWithMetadata method
      
      # Method Metadata$writeXML() ####
      
      writeXML = function()
      {
         write_xml(
            x = self$xml,
            file = self$path
         );
      },
      
      # Method Metadata$getLayer() ####
      
      getLayer = function(layerName)
      {
         if (is.null(layerName)) {
            return(self$currentLayer);
         } else {
            stop("Metadata$getLayer cannot process a non-null layerName, yet.")
         }
      },
      
      # Method Metadata$addMetadataElement() ####
      
      addMetadataElement = function
      (
         element,
         category,
         context,
         description,
         layer
      )
      {
         # Add a description to the element, if provided
         if (!is.null(description)) {
            xml_set_text(x = element, value = description);
         }
         
         # Look for the context element
         contextElement <- xml_find_first(
            x = layer,
            xpath = sprintf(
               "metadata/%s/%s%s",
               category,
               category,
               context
            )
         );
         
         # Check if the context element exists
         if (length(contextElement) != 0) {
            
            # If the context element exists, add the instrument element to it
            xml_add_child(
               .x = contextElement,
               .value = element
            );
            
         } else {
            
            # If the context element does not exist, add the context element with the instrument element
            contextElementList <- list(temp = list());
            names(contextElementList) <- paste0(category, context);
            contextElement <- as_xml_document(contextElementList);

            # Add the element to the context element
            xml_add_child(.x = contextElement, .value = element);
            
            # Add the context element to the metadata
            xml_add_child(
               .x = xml_find_first(
                  x = layer,
                  xpath = paste0("metadata/", category)
               ),
               .value = contextElement
            );
            
         }
      }, # End method addMetadataElement
      
      # Method Metadata$addPersonWho() ####
      
      addPersonWho = function
      (
         whoContext,
         name, 
         org,
         prefContact,
         prefContactType = "email",
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create a person element and add it to the metadata in context
         element = as_xml_document(list(
            person = structure(
               list(),
               name = name,
               org = org,
               prefContact = prefContact,
               prefContactType = prefContactType
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "who",
            context = whoContext,
            description = description,
            layer = layer
         );
         
      }, # End method addPersonWho
      
      # Method Metadata$addVariableWhat() ####
      
      addVariableWhat = function
      (
         whatContext,
         header,
         name,
         units,
         dimensions,
         stat,
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the person element and add it with context
         element = as_xml_document(list(
            variable = structure(
               list(),
               header = header,
               name = name,
               units = units,
               dimensions = dimensions,
               stat = stat
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "what",
            context = whatContext,
            description = description,
            layer = layer
         );
         
      }, # End method addVariableWhat
      
      # Method Metadata$addEntityWhat() ####
      
      addEntityWhat = function
      (
         whatContext,
         entityPath,
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the site element and add it with context
         element <- as_xml_document(list(
            entity = structure(
               list(),               
               path = entityPath
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "what",
            context = whatContext,
            description = description,
            layer = layer
         );
         
      }, # End method addEntityWhat
      
      # Method Metadata$addWindowWhen() ####
      
      addWindowWhen = function
      (
         whenContext,
         startTime,
         endTime,
         timeZone,
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the window element and add it with context
         element <- as_xml_document(list(
            timeWindow = structure(
               list(),               
               startTime = startTime,
               endTime = endTime,
               timeZone = timeZone
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "when",
            context = whenContext,
            description = description,
            layer = layer
         );
         
      }, # End method addWindowWhen
      
      # Method Metadata$addSiteWhere() ####
      
      addSiteWhere = function
      (
         whereContext,
         sitePath,
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the site element
         element <- as_xml_document(list(
            site = structure(
               list(),               
               path = sitePath
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "where",
            context = whereContext,
            description = description,
            layer = layer
         );
         
      }, # End method addSiteWhere
      
      # Method Metadata$addPointWhere() ####
      
      addPointWhere = function
      (
         whereContext,
         source,
         latitude,
         latitudeUnits,
         longitude,
         longitudeUnits,
         elevation = "unknown",
         elevationUnits = "unknown",
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the site element and add it with context
         element <- as_xml_document(list(
            point = structure(
               list(),     
               source = source,
               latitude = latitude,
               latitudeUnits = latitudeUnits,
               longitude = longitude,
               longitudeUnits = longitudeUnits,
               elevation = elevation,
               elevationUnits = elevationUnits
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "where",
            context = whereContext,
            description = description,
            layer = layer
         );
         
      }, # End method addPointWhere
      
      # Method Metadata$addInstrumentHow() ####
      
      addInstrumentHow = function
      (
         howContext,
         name,
         serialNumber,
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the instrument element and add it with context
         element <- as_xml_document(list(
            instrument = structure(
               list(),               
               name = name,
               serialNumber = serialNumber
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "how",
            context = howContext,
            description = description,
            layer = layer
         );
         
      }, # End method addInstrumentHow
      
      # Method Metadata$addMotivationWhy() ####
      
      addMotivationWhy = function
      (
         whyContext,
         projectName,
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the instrument element
         element <- as_xml_document(list(
            motivation = structure(
               list(),               
               projectName = projectName
            )
         ));
         self$addMetadataElement(
            element = element,
            category = "why",
            context = whyContext,
            description = description,
            layer = layer
         );
         
      } # End method addMotivationWhy
      
   ) # End list of public elements
) # End definition of Metadata R6 Class

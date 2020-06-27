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
      
      initialize = function
      (
         path, 
         baseLayerName = NULL
      )
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

      # Method Metadata$readXML() ####
      
      readXML = function()
      {
         self$xml <- read_xml(
            x = self$path
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
      
      # Method Metadata$addPerson() ####
      
      addPerson = function
      (
         context,
         name, 
         org,
         prefContact,
         prefContactType = "email",
         category = "who",
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
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addPerson
      
      # Method Metadata$addVariable() ####
      
      addVariable = function
      (
         context,
         header,
         name,
         units,
         dimensions,
         stat,
         instrument,
         category = "what",
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
               stat = stat,
               instrument = instrument
            )
         ));
         self$addMetadataElement(
            element = element,
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addVariable
      
      # Method Metadata$addEntity() ####
      
      addEntity = function
      (
         context,
         entityPath,
         category = "what",
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
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addEntity
      
      # Method Metadata$addEntityAtSite() ####
      
      addEntityAtSite = function
      (
         entityContext,
         entityPath,
         siteContext,
         sitePath,
         addPoint = FALSE,
         pointContext = NULL,
         source = NULL,
         latitude = NULL,
         latitudeUnits = NULL,
         longitude = NULL,
         longitudeUnits = NULL,
         elevation = "unknown",
         elevationUnits = "unknown",
         entityCategory = "what",
         siteCategory = "where",
         pointCategory = "where",
         description = NULL,
         siteDescription = NULL,
         pointDescription = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the entity element and add it to the metadata in entity context
         element <- as_xml_document(list(
            entity = structure(
               list(),               
               path = entityPath
            )
         ));
         self$addMetadataElement(
            element = element,
            category = entityCategory,
            context = entityContext,
            description = description,
            layer = layer
         );
         
         # Create the site element and add it to the metadata in entity and site context
         self$addSite(
            context = entityContext,
            sitePath = sitePath,
            category = entityCategory,
            description = siteDescription,
            layer = layer
         )
         self$addSite(
            context = siteContext,
            sitePath = sitePath,
            category = siteCategory,
            description = siteDescription,
            layer = layer
         )
         
         # If enabled, create the point element and add it to the metadata in entity and point context
         if (addPoint) {
            self$addPoint(
               context = entityContext,
               source = source,
               latitude = latitude,
               latitudeUnits = latitudeUnits,
               longitude = longitude,
               longitudeUnits = longitudeUnits,
               elevation = elevation,
               elevationUnits = elevationUnits,
               category = entityCategory,
               description = pointDescription,
               layer = layer
            );
            self$addPoint(
               context = pointContext,
               source = source,
               latitude = latitude,
               latitudeUnits = latitudeUnits,
               longitude = longitude,
               longitudeUnits = longitudeUnits,
               elevation = elevation,
               elevationUnits = elevationUnits,
               category = pointCategory,
               description = pointDescription,
               layer = layer
            );
         }
         
      }, # End method addEntityAtSite
      
      # Method Metadata$addFile() ####
      
      addFile = function
      (
         context,
         fileName,
         fileType,
         contentType,
         category = "what",
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the instrument element and add it with context
         element <- as_xml_document(list(
            file = structure(
               list(),               
               fileName = fileName,
               fileType = fileType,
               contentType = contentType
            )
         ));
         self$addMetadataElement(
            element = element,
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addFile
      
      # Method Metadata$addTimeWindow() ####
      
      addTimeWindow = function
      (
         context,
         startTime,
         endTime,
         timeZone,
         category = "when",
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
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addTimeWindow
      
      # Method Metadata$createSiteElement() ####
      
      createSiteElement = function
      (
         sitePath
      )
      {
         # Create the site element
         return(
            as_xml_document(list(
               site = structure(
                  list(),               
                  path = sitePath
               )
            ))
         );
      },
      
      # Method Metadata$addSite() ####
      
      addSite = function
      (
         context,
         sitePath,
         category = "where",
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the site element
         element <- self$createSiteElement(sitePath);
         
         # Add the site element to the where element with context
         self$addMetadataElement(
            element = element,
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addSite
      
      # Method Metadata$createPointElement ####
      
      createPointElement = function
      (
         source,
         latitude,
         latitudeUnits,
         longitude,
         longitudeUnits,
         elevation = "unknown",
         elevationUnits = "unknown"
      )
      {
         # Create the site element and add it with context
         return(
            as_xml_document(list(
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
            ))
         );
      },
      
      # Method Metadata$addPoint() ####
      
      addPoint = function
      (
         context,
         source,
         latitude,
         latitudeUnits,
         longitude,
         longitudeUnits,
         elevation = "unknown",
         elevationUnits = "unknown",
         category = "where",
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the point element
         element <- self$createPointElement(     
            source = source,
            latitude = latitude,
            latitudeUnits = latitudeUnits,
            longitude = longitude,
            longitudeUnits = longitudeUnits,
            elevation = elevation,
            elevationUnits = elevationUnits
         );
         
         # Add the point element to the metadata with context
         self$addMetadataElement(
            element = element,
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addPoint
      
      # Method Metadata$addInstrument() ####
      
      addInstrument = function
      (
         context,
         name,
         serialNumber,
         category = "how",
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
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addInstrument
      
      # Method Metadata$addQAQCCode() ####
      
      addQAQCCode = function
      (
         context,
         header,
         code,
         category = "how",
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the instrument element and add it with context
         element <- as_xml_document(list(
            qaqcCode = structure(
               list(),
               header = header,
               code = code
            )
         ));
         self$addMetadataElement(
            element = element,
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      }, # End method addQAQCCode
      
      # Method Metadata$addProjectEffort() ####
      
      addProjectEffort = function
      (
         context,
         path,
         category = "why",
         description = NULL,
         layerName = NULL,
         layer = self$getLayer(layerName = layerName)
      )
      {
         # Create the instrument element
         element <- as_xml_document(list(
            projectEffort = structure(
               list(),               
               path = path
            )
         ));
         self$addMetadataElement(
            element = element,
            category = category,
            context = context,
            description = description,
            layer = layer
         );
         
      } # End method addProjectEffort
      
   ) # End list of public elements
) # End definition of Metadata R6 Class

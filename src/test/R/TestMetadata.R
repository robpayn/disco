library(R6);
library(xml2);
source(file = "../../main/R/disco/R/Metadata.R");

unlink(x = "./testMetadata", recursive = TRUE);
dir.create(path = "./testMetadata");

metadata <- Metadata$new("./testMetadata/test.xml", baseLayerName = "root");

metadata$addPersonWho(
   performedAction = "CreatedProduct",
   name = "Rob Payn",
   org = "MSU-LRES",
   prefContact = "rpayn@montana.edu"
);
metadata$addPersonWho(
   performedAction = "CreatedProduct",
   name = "Rob Payn",
   org = "MSU-LRES",
   prefContact = "rpayn@montana.edu",
   description = "Awesomness"
);

metadata$addWhatPropertyWasEvaluated(
   header = "par_inst",
   name = "Instantaneous PAR",
   description = "Instananeous photosynthetically active radiation"
);

metadata$addWhatEntityIsDescribed(
   entityPath = "Louse Creek/RR01",
   description = "Blah"
);

metadata$addSiteWhere(
   locationContext = "WasEvaluation",
   sitePath = "USA/Montana/Gallatin River Watershed/Hyalite Creek/Outlet Site"
)
metadata$addSiteWhere(
   locationContext = "WasEvaluation",
   sitePath = "USA/Montana/Gallatin River Watershed/Hyalite Creek/Outlet Site"
)

metadata$addInstrumentHow(
   name = "Blah",
   serialNumber = "1234",
   propertyHeader = "all"
)
metadata$addInstrumentHow(
   name = "CampbellLicor",
   serialNumber = "1235",
   propertyHeader = "all"
)

metadata$addWhyWereDataEvaluated(
   description = "Because I said so"
)

metadata$writeXML()

#'Converts a csv for SORMAS import
#'
#'Converts case data saved as a csv file into a SORMAS-importable csv file
#'@param file_persons csv-file containing person data from sormasdatagen
#'@param file_symptoms csv-file containing symptom data from sormasdatagen
#'@param file_out Name of .csv-file, where output should be saved
#'@param verbose Boolean, if (small) updates should be printed to console
#' data that is not converted: health_authority, reporting_week, infection_date, case_def_id, residencecountry, residenceplace
#' !!!some necessary data for sormas-import is added in by hand: disease, responsibleRegion, responsibleDistrict, healthFacility
sormas_case_conv <- function(file_persons, file_symptoms, file_out, verbose = FALSE) {
  
  if (verbose) {cat("Converting case data\n")}
  
  pers <- read.csv(file_persons, header = TRUE)
  symp <- read.csv(file_symptoms, header = TRUE)
  
  ddmmyyyy <- datespliter(pers$birthdate)
  
  case_data <- data.frame(disease = c("CaseData", rep(c("Coronavirus"),nrow(pers))),#change?
                          externalID = c("CaseData", pers$id),
                          #externalToken = c("CaseData", pers$id),
                          person.firstName = c("Person", pers$firstname),
                          person.lastName = c("Person", pers$lastname),
                          person.sex = c("Person", sex_conv(pers$sex)),
                          person.birthdateDD = c("Person", ddmmyyyy$dd),
                          person.birthdateMM = c("Person", ddmmyyyy$mm),
                          person.birthdateYYYY = c("Person", ddmmyyyy$yyyy),
                          person.approximateAge = c("Person", pers$age),
                          person.presentCondition = c("Person", died_cond_conv(pers$died)),
                          person.deathDate = c("Person", date_conv(pers$reporting_date_death)),
                          person.address.country = c("Location", pers$address_country),
                          person.address.city = c("Location", pers$address_city),
                          person.address.postalCode = c("Location", pers$address_zipcode),
                          person.address.street = c("Location", pers$address_street),
                          person.address.houseNumber = c("Location", pers$address_streetnumber),
                          person.address.longitude = c("Location", pers$longitude),
                          person.address.latitude = c("Location", pers$latitude),
                          person.phone = c("Person", paste0("+", as.character(pers$phonenumber))), #turn phonenumber into char and add "+" at start
                          person.emailAddress = c("Person", pers$email),
                          reportDate = c("CaseData", date_conv(pers$reporting_date)),
                          caseClassification = c("CaseData", cat_clas_conv(pers$case_category, pers$symptomatic)),
                          responsibleRegion = c("CaseData", rep(c("Niedersachsen"),nrow(pers))), #REWORK
                          responsibleDistrict = c("CaseData", rep(c("Niedersachsen-District1"),nrow(pers))), #REWORK
                          healthFacility = c("CaseData", rep(c("NO_FACILITY"),nrow(pers))), #REWORK
                          hospitalization.admittedToHealthFacility = c("Hospitalization", hosp_conv(pers$hospitalized)),
                          hospitalization.admissionDate = c("Hospitalization", date_conv(pers$hospitalization_date)),
                          symptoms.onsetDate = c("Symptoms", date_conv(pers$onset_date))
  )
  
  #append symptom data converted by symp_conv as columns add the end
  if (verbose) {cat("Appending symptoms\n")}
  case_data <- cbind(case_data, symp_conv(pers, symp))
  
  #don't know why SORMAS doesn't import col-names, but this works:
  case_data <- rbind(colnames(case_data),case_data)
  
  case_data[is.na(case_data[,])] <- ""
  #for .csv change first two rows, via [-c(2),] and row.names
  write.table(case_data[-c(2),], file = file_out, row.names = FALSE, col.names = case_data[2,], sep = ";")
}

#'Converts a csv for SORMAS import
#'
#'Converts event data saved as a csv file into a SORMAS-importable csv file
#'@param file_events csv-file containing event data from sormasdatagen
#'@param file_participants csv-file containing participation data from sormasdatagen
#'@param file_persons csv-file containing person data from sormasdatagen
#'@param file_out Name of .csv-file, where output should be saved
#'@param verbose Boolean, if (small) updates should be printed to console
#'@param event_list A character vector, which contains the desired event-ids. if list is empty, all events from file are used
#'@param create_sep_part Boolean, if for every event a separate participant file should be created
#'@param max_part Maximum of participants, which should be included as columns after participated event
#' !!!some necessary data for sormas-import is added in by hand: eventLocation.region, eventLocation.district
sormas_event_conv <- function (file_events, file_participants, file_persons, file_out, verbose = FALSE,
                               event_list = c(), create_sep_part = FALSE, max_part = Inf) {
  
  if (verbose) {cat("Converting event data\n")}
  
  events <- read.csv(file_events, header = TRUE)
  part <- read.csv(file_participants, header = TRUE)
  pers <- read.csv(file_persons, header = TRUE)
  
  #if event_list is empty take all events from import
  if (length(event_list) == 0) {
    event_list <- events$id
  }
  
  #determine events of interest in imported events data
  desired_events <- events$id %in% event_list
  
  #convert event_data, which only regards the event itself
  event_data <- data.frame(#eventStatus = c("Event", rep(c("SIGNAL"),length(event_list))), #other?,
    eventTitle = c("Event", rep(c("Import_Event_Test"),length(event_list))), #other?
    externalId = c("Event",events[desired_events,]$id),
    reportDateTime = c("Event", date_conv(events[desired_events,]$reporting_date)),
    eventLocation.country = c("Location", events[desired_events,]$address_country),
    eventLocation.region = c("Location", rep(c("Niedersachsen"),length(event_list))), #REWORK
    eventLocation.district = c("Location", rep(c("Niedersachsen-District1"),length(event_list))), #REWORK
    eventLocation.city = c("Location", events[desired_events,]$address_city),
    eventLocation.longitude = c("Location", events[desired_events,]$longitude),
    eventLocation.latitude = c("Location", events[desired_events,]$latitude),
    eventLocation.postalCode = c("Location", events[desired_events,]$address_zipcode),
    eventLocation.street = c("Location", events[desired_events,]$address_street),
    eventLocation.houseNumber = c("Location", events[desired_events,]$address_streetnumber),
    srcTelNo = c("Event", paste0("+", as.character(events[desired_events,]$phonenumber))) #turn phonenumber into chr and add "+" at start
  )
  #start including participant data into event_data, each participant has it own set of columns containing
  #information, which is appended at the end of the participated event. Therefore we add the participants in their 'event-order',
  #i.e. first gather data about every first participant of each event and then append them all together to event_data, the proceed
  empty <- FALSE #boolean, if there are part to add
  i <- 1 #determines which participant of each event is included
  while (!empty & i <= max_part) {
    empty <- TRUE #at the beginning of each loop assume emptiness
    #create an participant data.frame with the same row number as event_data
    participant <- data.frame(person.firstName = c("Person", character(length(event_list))),
                              person.lastName = c("Person", character(length(event_list))),
                              person.sex = c("Person", character(length(event_list))),
                              person.birthdateDD = c("Person", numeric(length(event_list))),
                              person.birthdateMM = c("Person", numeric(length(event_list))),
                              person.birthdateYYYY = c("Person", numeric(length(event_list))),
                              person.approximateAge = c("Person", numeric(length(event_list))),
                              person.address.country = c("Location", character(length(event_list))),
                              person.address.city = c("Location", character(length(event_list))),
                              person.address.postalCode = c("Location", character(length(event_list))),
                              person.address.street = c("Location", character(length(event_list))),
                              person.address.houseNumber = c("Location", character(length(event_list))),
                              person.addres.longitude = c("Location", numeric(length(event_list))),
                              person.addres.latitude = c("Location", numeric(length(event_list))),
                              person.externalId = c("Person", character(length(event_list)))
    )
    #loop over the events, which had participants and are part of event_list
    for (e_id in intersect(part$event_id, event_list)) {
      #use them if they contain at least i participants
      if (length(part[part$event_id == e_id,]$participant_id) >= i) {
        empty <- FALSE #if this iteration contains at least one participant it is not empty
        p_id <- part[part$event_id == e_id,]$participant_id[i] #choose the i-th participant of the event
        #convert participant data
        participant[event_data$externalId == e_id,]$person.firstName <- pers[pers$id == p_id,]$firstname
        participant[event_data$externalId == e_id,]$person.lastName <- pers[pers$id == p_id,]$lastname
        participant[event_data$externalId == e_id,]$person.sex <- sex_conv(pers[pers$id == p_id,]$sex)
        ddmmyyyy <- datespliter(pers[pers$id == p_id,]$birthdate)
        participant[event_data$externalId == e_id,]$person.birthdateDD <- ddmmyyyy$dd
        participant[event_data$externalId == e_id,]$person.birthdateMM <- ddmmyyyy$mm
        participant[event_data$externalId == e_id,]$person.birthdateYYYY <- ddmmyyyy$yyyy
        participant[event_data$externalId == e_id,]$person.approximateAge <- pers[pers$id == p_id,]$age
        participant[event_data$externalId == e_id,]$person.address.country <- pers[pers$id == p_id,]$address_country
        participant[event_data$externalId == e_id,]$person.address.city <- pers[pers$id == p_id,]$address_city
        participant[event_data$externalId == e_id,]$person.address.postalCode <- pers[pers$id == p_id,]$address_zipcode
        participant[event_data$externalId == e_id,]$person.address.street <- pers[pers$id == p_id,]$address_street
        participant[event_data$externalId == e_id,]$person.address.houseNumber <- pers[pers$id == p_id,]$address_streetnumber
        participant[event_data$externalId == e_id,]$person.addres.longitude <- pers[pers$id == p_id,]$longitude
        participant[event_data$externalId == e_id,]$person.addres.latitude <- pers[pers$id == p_id,]$latitude
        participant[event_data$externalId == e_id,]$person.externalId <- pers[pers$id == p_id,]$id
      }
    }
    #if participant data isn't empty append it to event_data
    if (!empty) {
      if (verbose) {cat("Appended the ", i, ". participant data of each event \n")}
      event_data <- cbind(event_data, participant)
    }
    i <- i + 1
  }
  
  #don't know why SORMAS doesn't import col-names, but this works:
  event_data <- rbind(colnames(event_data),event_data)
  
  event_data[is.na(event_data[,])] <- ""
  #for .csv change first two rows, via [-c(2),] and row.names
  write.table(event_data[-c(2),], file = file_out, row.names = FALSE, col.names = event_data[2,], sep = ";")
  
  #export participant data if desired
  if (create_sep_part) {
    source("sormas_event_part_conv.R")
    sormas_event_part_conv(file_participants, file_persons, event_list)
  }
}



#'Converts a csv for SORMAS import
#'
#'Converts participant data of events from event_list into SORMAS-importable data csv-files
#'@param file_participants csv-file containing participation data from sormasdatagen
#'@param file_persons csv-file containing person data from sormasdatagen
#'@param event_list Character vector of event_id, for which participant data should be converted
sormas_event_part_conv <- function(file_participants, file_persons, event_list) {
  part <- read.csv(file_participants, header = TRUE)
  pers <- read.csv(file_persons, header = TRUE)
  
  #loop over evet_list
  for (event_id in event_list) {
    cat("exporting participant data of ",event_id,"\n")
    
    #persons, who attended given event
    wanted_persons <- pers$id %in% part[part$event_id == event_id,]$participant_id
    
    ddmmyyyy <- datespliter(pers[wanted_persons,]$birthdate)
    
    participant_data <- data.frame(person.firstName = pers[wanted_persons,]$firstname,
                                   person.lastName = pers[wanted_persons,]$lastname,
                                   person.sex = sex_conv(pers[wanted_persons,]$sex),
                                   person.birthdateDD = ddmmyyyy$dd,
                                   person.birthdateMM = ddmmyyyy$mm,
                                   person.birthdateYYYY = ddmmyyyy$yyyy,
                                   person.approximateAge = pers[wanted_persons,]$age,
                                   person.address.country = pers[wanted_persons,]$address_country,
                                   person.address.city = pers[wanted_persons,]$address_city,
                                   person.address.postalCode = pers[wanted_persons,]$address_zipcode,
                                   person.address.street = pers[wanted_persons,]$address_street,
                                   person.address.houseNumber = pers[wanted_persons,]$address_streetnumber,
                                   person.addres.longitude = pers[wanted_persons,]$longitude,
                                   person.addres.latitude = pers[wanted_persons,]$latitude,
                                   person.externalId = pers[wanted_persons,]$id
    )
    #don't know why SORMAS doesn't import col-names, but this works:
    participant_data <- rbind(colnames(participant_data),participant_data)
    
    #create folder for .csv
    dir.create("event_participants", showWarnings = FALSE)
    write.csv(participant_data, file = paste0("event_participants/", event_id,"_participant_import.csv"), row.names = FALSE)
  }
}


#'Converts a csv for SORMAS import
#'
#'Converts contact data saved as a csv file into a SORMAS-importable csv file
#'@param file_contacts csv-file matching contact-ids with index.ids from sormasdatagen
#'@param file_persons csv-file containing person data from sormasdatagen
#'@param file_sormas_cases csv-file generated as sormas cases export. Must contain "uuid" and "externalID" of cases
#'@param file_out Name of .csv-file, where output should be saved
#'Matching of SORMAS cases and generated cases is achieved over the "externalID"
#'in SORMAS, which matches persons-id of generated data
sormas_contact_conv <- function(file_contacts, file_persons, file_sormas_cases, file_out, verbose = FALSE) {
  
  if (verbose) {cat("Converting contact data\n")}
  
  cont <- read.csv(file_contacts, header = TRUE)
  pers <- read.csv(file_persons, header = TRUE)
  s_cases <- read.table(file_sormas_cases, sep = ";", header = TRUE)
  
  #determine the indexcases of which we have the sormas uuid
  index <- intersect(s_cases[,s_cases[1,] == "externalID"], cont$index_id)
  
  #determine the contacts corresponding to these indexcases
  contact <- cont[cont$index_id %in% index,]$contact_id
  
  ddmmyyyy <- datespliter(pers[pers$id %in% contact,]$birthdate)
  
  #convert contact data
  contact_data <- data.frame(caseIdExternalSystem = c("Contact", character(length(contact))),
                             disease = c("Contact", rep(c("Coronavirus"),length(contact))),
                             contactClassification = c("Contact", rep(c("CONFIRMED"),length(contact))),
                             person.firstName = c("Person", pers[pers$id %in% contact,]$firstname),
                             person.lastName = c("Person", pers[pers$id %in% contact,]$lastname),
                             person.sex = c("Person", sex_conv(pers[pers$id %in% contact,]$sex)),
                             person.birthdateDD = c("Person", ddmmyyyy$dd),
                             person.birthdateMM = c("Person", ddmmyyyy$mm),
                             person.birthdateYYYY = c("Person", ddmmyyyy$yyyy),
                             person.approximateAge = c("Person", pers[pers$id %in% contact,]$age),
                             person.address.country = c("Location", pers[pers$id %in% contact,]$address_country),
                             person.address.city = c("Loaction", pers[pers$id %in% contact,]$address_city),
                             person.address.postalCode = c("Location", pers[pers$id %in% contact,]$address_zipcode),
                             person.address.street = c("Location", pers[pers$id %in% contact,]$address_street),
                             person.address.houseNumber = c("Location", pers[pers$id %in% contact,]$address_streetnumber),
                             person.addres.longitude = c("Location", pers[pers$id %in% contact,]$longitude),
                             person.addres.latitude = c("Person", pers[pers$id %in% contact,]$latitude),
                             person.externalId = c("CaseData", contact)
  )
  #match uuid from index case to contacts
  if (verbose) {cat("Matching contact and index cases\n")}
  for (i_id in index) {
    #contacts, which met index case 
    c_ids <- contact_data$person.externalId %in% cont[cont$index_id == i_id,]$contact_id
    #matching uuid
    contact_data[c_ids,]$caseIdExternalSystem <- s_cases[s_cases[,s_cases[1,] == "externalID"] == i_id, s_cases[1,] == "uuid"]
  }
  
  #don't know why SORMAS doesn't import col-names, but this works:
  contact_data <- rbind(colnames(contact_data),contact_data)
  
  contact_data[is.na(contact_data[,])] <- ""
  #for .csv change first two rows, via [-c(2),] and row.names
  write.table(contact_data[-c(2),], file = file_out, row.names = FALSE, col.names = contact_data[2,], sep = ";")
}



#'Converts sex format
#'
#'Converts sex-formats from "f", "m" and "d" to "FEMALE", "MALE" and "OTHER.
#'Missing data is replaced with "UNKNOWN".
#'@param sexes Vector, where sexes are represented via "f","m","d"
#'@return Character vector, where sexes are represented via "FEMALE", "MALE", "OTHER"
sex_conv <- function(sexes) {
  sexes[sexes == "f"] <- "FEMALE"
  sexes[sexes == "m"] <- "MALE"
  sexes[sexes == "d"] <- "OTHER"
  sexes[is.na(sexes)] <- "UNKNOWN"
  return (sexes)
}

#'Converts death information
#'
#'Converts died-information into condition
#'@param died boolean vector, if person has died
#'@return character vector, containing condition as "DEAD", "ALIVE", "UNKNOWN"
died_cond_conv <- function(died) {
  cond <- character(length(died))
  cond[died] <- "DEAD"
  cond[!died] <- "ALIVE"
  cond[is.na(died)] <- "UNKNOWN"
  return (cond)
}

#'Converts date-formats
#'
#'Converts dates in yyyy-mm-dd format to mm/dd/yyyy format
#'@param dates Character vector, where each entry represents a date in yyyy-mm-dd format
#'@return Character vector, where each entry represents a date in mm/dd/yyyy format
date_conv <- function(dates) {
  dates[is.na(dates)] <- "NA-NA-NA"
  new_dates <- do.call(rbind.data.frame, strsplit(dates, "-"))
  dates <- paste0(new_dates[,2],"/",new_dates[,3],"/",new_dates[,1])
  dates[dates == "NA/NA/NA"] <-""
  return(dates)
}

#'Splits dates in day, month and year
#'
#'Splits dates given in yyyy-mm-dd into data.frame with columns dd, mm, yyyy and changes them to integers
#'@param dates Character vector, where each entry represents a date in yyyy-mm-dd format
#'@return Data frame, where days, months and years are separated in columns
datespliter <- function(dates) {
  new_dates <- do.call(rbind.data.frame, strsplit(dates, "-")) #splits dates at "-" and turns it into a dataframe
  try(new_dates[is.na(new_dates[,])] <- "") #replace NA by ""
  spliteddates <- data.frame(dd = new_dates[,3], mm = new_dates[,2], yyyy = new_dates[,1]) #rearrange and rename dataframe
  return(spliteddates)
}

#'Converts category into classification
#'
#'Merges category and symptomatic into classification
#'@param category character vector, listing category
#'@param symptomatic booelan vector, representing if person is symptomatic
#'@return character vector, listing classifications
cat_clas_conv <- function(category, symptomatic) {
  category[category == "none"] <- "NOT_CLASSIFIED"
  category[category == "suspected"] <- "SUSPECT"
  category[category == "confirmed" & symptomatic] <- "CONFIRMED"
  category[category == "confirmed" & !symptomatic] <- "CONFIRMED_NO_SYMPTOMS"
  category[category == "confirmed" & is.na(symptomatic)] <- "CONFIRMED_UNKNOWN_SYMPTOMS"
  return (category)
}

#'Converts hospitalization boolean
#'
#'@param hosp boolean vector, if person is in hospital
#'@return character vector, with same information
hosp_conv <- function(hosp) {
  hosp_new <- character(length(hosp))
  hosp_new[hosp] <- "YES"
  hosp_new[!hosp] <- "NO"
  return(hosp_new)
}

#'Converts symptoms
#'
#'Creates symptom data.frame for every person, based on info from symptoms
#'
#'The symptoms in the data set are a matched to the individual cases. For the SORMAS
#'template these have to matched with symptoms in SORMAS and formatted differently,
#'so that every row list all (possible) symptoms of a case.
#'@param persons data.frame with info about person-id and if person is symptomatic
#'@param symp data.frame connecting person-id and symptoms (every symptom per person is in a new row)
#'@return Data frame, where for each id from persons and every symptom info is stored via "YES", "NO" and "UNKNOWN"
#'@note There are symptoms in SORMAS, which are not represented in the original data.
symp_conv <- function(persons, symp) {
  n <- nrow(persons)
  symptoms_new <- data.frame(#symptoms.chillsSweats = character(n)),
    symptoms.cough = character(n),
    symptoms.diarrhea = character(n),
    symptoms.difficultyBreathing = character(n),
    symptoms.fever = character(n),
    symptoms.headache = character(n),
    symptoms.musclePain = character(n),
    symptoms.nausea = character(n),
    symptoms.otherNonHemorrhagicSymptoms = character(n),
    symptoms.otherNonHemorrhagicSymptomsText = character(n),
    symptoms.runnyNose = character(n),
    symptoms.soreThroat = character(n),
    #symptoms.acuteRespiratoryDistressSyndrome = character(n),
    #symptoms.pneumoniaClinicalOrRadiologic = character(n),
    symptoms.lossOfTaste = character(n),
    symptoms.lossOfSmell = character(n),
    #symptoms.respiratoryDiseaseVentilation = character(n),
    symptoms.feelingIll = character(n)#,
    #symptoms.pneumoniaClinicalOrRadiologic = character(n),
    #symptoms.fastHeartRate = character(n),
    #symptoms.oxygenSaturationLower94 = character(n),
    #symptoms.shivering = character(n)
  )
  
  #fill every cell with "NO"
  symptoms_new[,] <- "NO"
  
  #if persons symptomatic status is NA, fill every symptom with "UNKNOWN"
  symptoms_new[is.na(persons$symptomatic),] <- rep(c("UNKNOWN"), ncol(symptoms_new))
  
  #for symptomatic persons, fill symptom fitting category with "YES"
  #symp[symp$symptom == "cough",]$person_id: lists person-id with symptom "cough"
  
  #good fitting symptoms
  try(symptoms_new[persons$id %in% symp[symp$symptom == "cough",]$person_id,]$symptoms.cough <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "diarrhea",]$person_id,]$symptoms.diarrhea <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "fever",]$person_id,]$symptoms.fever <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "headache",]$person_id,]$symptoms.headache <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "muscle aches",]$person_id,]$symptoms.musclePain <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "nausea/vomiting",]$person_id,]$symptoms.nausea <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "runny nose",]$person_id,]$symptoms.runnyNose <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "sore throat",]$person_id,]$symptoms.soreThroat <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "loss of smell and/or taste",]$person_id,]$symptoms.lossOfTaste <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "loss of smell and/or taste",]$person_id,]$symptoms.lossOfSmell <- "YES")
  
  #ambiguously fitting symptoms
  try(symptoms_new[persons$id %in% symp[symp$symptom == "fatigue",]$person_id,]$symptoms.feelingIll <- "YES")
  try(symptoms_new[persons$id %in% symp[symp$symptom == "shortness of breath",]$person_id,]$symptoms.difficultyBreathing <- "YES")
  
  #missing symptoms
  missing <- c("chest pain", "confusion", "dizziness", "fainting", "loss of appetite", "skin manifestations", "sputum production")
  #look for persons with symptoms, that are missing above
  try(symptoms_new[persons$id %in% symp[symp$symptom %in% missing,]$person_id,]$symptoms.otherNonHemorrhagicSymptoms <- "YES")
  try(symptoms_new[symptoms_new$symptoms.otherNonHemorrhagicSymptoms == "NO",]$symptoms.otherNonHemorrhagicSymptomsText <- "")
  
  #for each person with additional symptoms, add these symptoms as char
  for (id in symp[symp$symptom %in% missing,]$person_id) {
    #check if id from symp is in person (only for testing with subsection of persons)
    if (id %in% persons$id) {
      symptoms_new[persons$id == id,]$symptoms.otherNonHemorrhagicSymptomsText <- paste(intersect(symp[symp$person_id == id,]$symptom, missing), collapse = ", ")
    }
  }
  symptoms_new <- rbind(rep("Symptom"), symptoms_new)
  return (symptoms_new)
}

library(readr)
library(stringr)
library(dplyr)
library(gender)
library(httr2)
library(jsonlite)
library(tibble)
library(purrr)
library(tidyr)
library(data.table)
library(igraph)
library(fuzzyjoin)
library(ggplot2)


xml_data <- readRDS("./CreatedData/all_people_data.rds")

# Define functions for combining names ################
combine_token_matches <- function(data, min_shared_tokens = 2) {
  setDT(data)
  
  # Split by firm
  firm_list <- split(data, by = "Filer.EIN", keep.by = TRUE)
  
  process_firm <- function(firm_data) {
    unique_names <- unique(firm_data[, .(name_cleaned)])
    n <- nrow(unique_names)
    
    # Build edges for graph
    edges <- list()
    for (i in 1:(n - 1)) {
      name_i <- unique_names$name_cleaned[i]
      tokens_i <- unlist(strsplit(tolower(gsub("[^a-z ]", "", name_i)), "\\s+"))
      
      for (j in (i + 1):n) {
        name_j <- unique_names$name_cleaned[j]
        tokens_j <- unlist(strsplit(tolower(gsub("[^a-z ]", "", name_j)), "\\s+"))
        
        if (length(intersect(tokens_i, tokens_j)) >= min_shared_tokens) {
          edges <- append(edges, list(c(name_i, name_j)))
        }
      }
    }
    
    if (length(edges) > 0) {
      g <- graph_from_edgelist(do.call(rbind, edges), directed = FALSE)
      comps <- components(g)$membership
      
      # Map each name to its cluster's canonical name
      canonical_map <- data.table(
        name_cleaned = names(comps),
        group = comps
      )[, .(canonical_name = name_cleaned[which.min(nchar(name_cleaned))]), by = group]
      
      mapping <- merge(data.table(name_cleaned = names(comps), group = comps),
                       canonical_map, by = "group")[, .(name_cleaned, canonical_name)]
      
      firm_data <- merge(firm_data, mapping, by = "name_cleaned", all.x = TRUE)
      firm_data[, name_cleaned := fcoalesce(canonical_name, name_cleaned)]
      firm_data[, canonical_name := NULL]
    }
    
    return(firm_data)
  }
  
  corrected_list <- lapply(firm_list, process_firm)
  corrected_data <- rbindlist(corrected_list, use.names = TRUE, fill = TRUE)
  
  return(corrected_data)
}

combine_nospace_matches <- function(data) {
  setDT(data)
  
  firm_list <- split(data, by = "Filer.EIN", keep.by = TRUE)
  
  process_firm <- function(firm_data) {
    unique_names <- unique(firm_data[, .(name_cleaned)])
    n <- nrow(unique_names)
    if (n <= 1) return(firm_data)
    
    unique_names[, name_nospace := gsub(" ", "", name_cleaned)]
    
    edges <- list()
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        ns_i <- unique_names$name_nospace[i]
        ns_j <- unique_names$name_nospace[j]
        if (nchar(ns_i) >= 6 && nchar(ns_j) >= 6 &&
            (startsWith(ns_j, ns_i) || startsWith(ns_i, ns_j))) {
          edges <- append(edges, list(c(unique_names$name_cleaned[i], unique_names$name_cleaned[j])))
        }
      }
    }
    
    if (length(edges) > 0) {
      g <- graph_from_edgelist(do.call(rbind, edges), directed = FALSE)
      comps <- components(g)$membership
      
      canonical_map <- data.table(
        name_cleaned = names(comps),
        group = comps
      )[, .(canonical_name = name_cleaned[which.min(nchar(name_cleaned))]), by = group]
      
      mapping <- merge(data.table(name_cleaned = names(comps), group = comps),
                       canonical_map, by = "group")[, .(name_cleaned, canonical_name)]
      
      firm_data <- merge(firm_data, mapping, by = "name_cleaned", all.x = TRUE)
      firm_data[, name_cleaned := fcoalesce(canonical_name, name_cleaned)]
      firm_data[, canonical_name := NULL]
    }
    
    return(firm_data)
  }
  
  corrected_list <- lapply(firm_list, process_firm)
  rbindlist(corrected_list, use.names = TRUE, fill = TRUE)
}

combine_fuzzy_matches <- function(data, max_dist) {
  setDT(data)
  
  # Split by EIN to avoid excessive joins
  firm_list <- split(data, by = "Filer.EIN", keep.by = TRUE)
  
  process_firm <- function(firm_data) {
    if (nrow(firm_data) <= 1) return(firm_data)
    
    unique_names <- unique(firm_data[, .(name_cleaned)])
    
    # Add frequency count
    name_counts <- firm_data[, .(name_count = .N), by = name_cleaned]
    unique_names <- merge(unique_names, name_counts, by = "name_cleaned")
    
    # Compute approximate name matches only within this firm
    matches <- stringdist_inner_join(
      unique_names, unique_names,
      by = "name_cleaned",
      max_dist = max_dist,
      method = "osa"
    ) %>%
      filter(name_cleaned.x < name_cleaned.y) %>%
      mutate(
        correct_name = if_else(
          name_count.x >= name_count.y,
          name_cleaned.x,
          name_cleaned.y
        )
      ) %>%
      select(name_cleaned.x, name_cleaned.y, correct_name) %>%
      pivot_longer(cols = c(name_cleaned.x, name_cleaned.y),
                   values_to = "name_cleaned") %>%
      select(name_cleaned, correct_name) %>%
      filter(name_cleaned != correct_name)
    
    if (nrow(matches) == 0) return(firm_data)
    
    if (any(grepl("morisco", firm_data$name_cleaned))) {
      print(matches)
    }
    
    # Merge corrected names
    setDT(matches)
    firm_data <- merge(firm_data, matches, by = "name_cleaned", all.x = TRUE)
    firm_data[, name_cleaned := fcoalesce(correct_name, name_cleaned)]
    firm_data[, correct_name := NULL]
    
    return(firm_data)
  }
  
  corrected_list <- lapply(firm_list, process_firm)
  corrected_data <- rbindlist(corrected_list, use.names = TRUE, fill = TRUE)
  
  return(corrected_data)
}

# # ── List of unique EINs present in 2013-2014 in xml data ─────────────────────
# ein_list_for_scraping <- xml_data %>%
#   distinct(Filer.EIN) %>%
#   arrange(Filer.EIN)
# 
# cat("Total EINs to collect data for:", nrow(ein_list_for_scraping), "\n")
# 
# # Save to csv for your reference while scraping
# write.csv(ein_list_for_scraping, "./CreatedData/ein_list_for_scraping.csv", row.names = FALSE)

# read in files from manual data extraction
files <- list.files(path = "./CreatedData/manual_990_data/", full.names = TRUE)

early_data <- data.frame()
for (file in files) {
  data <- read_csv(file) %>%
    mutate(ha=as.numeric(ha), return_type = as.character(return_type))
  
  early_data <- bind_rows(early_data, data)
}

early_data <- early_data %>%
  rename(PersonNm = name_cleaned) 

# define lists to look for the name column 
doctor_list <- c("md", "do", "urologist", "mdmba", "doctor", "mdend", "mdpresident", "mdjd", "physician")
other_doctor_list <- c("mbbch", "mbbs", "dds", "dentist", "od", "pharmd", "pt", "radiologist", "physician", "surgeon", "otolaryngologist")
nurse_list <- c("rn", "dnp", "aprn", "crna", "np", "scn", "fnp", "cne", "fnp-c", "apnp", "fnp-", "cenp", "mphrn", "acnp", "msnrnfaan", "crnp", "msnrn",
                "eddrn", "dpn", "drnp", "anp", "edrn", "arnp", "pnp", "cnp", "bcccrn", "crn", "nurse", "arpn", "cnaa")
ha_list <- c("mha", "fache", "mhadrph", "mhsa", "mhcm", "mhs")
remove_list <- c("mbbch", "mbbs", "until", "as of", "eff", "end", "beg", "thru", "through", "osb",
                 "ending", "term", "mph", "deceased", "to", "begin", "left", "see statement", "director", "chair", "hired", "termed",
                 "began", "retired", "from", "resigned", "off", "vp", "ended", "started", "trm", "dir of", "pd by", "provost",
                 "start", "oct", "treasurer", "jan", "st", "chief", "t0", "entered", "interim", "board", "fmr", "int", "trustee",
                 "vice president", "dir", "bh", "vppres", "msn", "mst", "assoc", "departed", "msw", "ceo", "cbe", "secretary", "rsmterm",
                 "effective", "effec", "former", "admininstrator", "consultant", "cnm", "fellow", "beginning", "president",
                 "dr", "bishop", "esq", "jr", "sr", "md", "phd", "dds", "ii", "iii", "iv", "ms", "mr", "do","most rev", "rev", "rn", "edd", "cpa", "od",
                 "the rt", "dnp", "bc", "faan", "mb", "chb", "mbbh", "mbbch", "chcio", "rsm", "part year", "family practice", "major general",
                 "sister", "mha", "magistrate", "- see sch o", "see sch o", "sch o", "dc","the very rev", "the most reverend", "the honorable", "the hon",
                 "general", "vice pres", "fr", "mba", "jd", "ma", "bsn", "cco", "chco", "mpas", "pac", "dsc", "cbs", "chs", "ed d",
                 "rd", "bsbn", "mshrm", "cpm", "ccim", "ecc", "home care", "st john", "ph d", "csj", "non voting", "ex officio",
                 "dmd", "ret maj gen", "sc", "pharm d", "int ceo", "msn", "osf", "mother", "rtscra", "dha", "dh", "evpcao", "evpcoo",
                 "dha", "family medicine", "mshr", "aprn", "cssf", "offcr", "cfo", "chcp", "ccep", "facp", "fhm", "cphq", "op", "cpe", "vd", "dvm",
                 "cma", "drph", "mhp", "pmhnp", "er", "fsgm", "eid", "dpm", "staff acct", "cfp", "cpfo", "pe", "father", "esquire", "phjc", "dnsc",
                 "csfn", "dpth", "br", "fsc", "emhs", "rpa", "facsm", "mn", "adm", "scd", "psyd", "jcdjv", "fp", "msc", "bs", "mfa", "csc", "snd",
                 "pa-c", "nhs", "facs", "vpgeneral couns", "preside", "gen counsel", "pa", "vpcfo", "lmt", "ccvi", "pe", "scc", "rrt", "wvote",
                 "nea-", "ph", "brother", "senior", "facc", "msf", "rph", "facep", "dml", "employed", "member", "dsw", "-april", "april-dec", "facs",
                 "inactive", "dmsc", "jul-", "dec-jun", "-jun", "lnha", "hon", "cc", "joined", "mdboard", "mddirector", "nlh", "presceo", "svp",
                 "lega", "presidentceo", "sp", "of", "fsa", "dmin", "dhsc", "sphr", "csa", "hm", "lcda", "lcdo", "mrs", "ms", "mr", "faap", "sessions",
                 "corporate compliance", "wm", "cso", "thm", "osp", "mpf", "commissioner", "honorable", "mpd", "ne-", "- care", "ambulatory",
                 "treasasst", "secr", "administrative", "pd", "mdiv", "msgr", "dba", "msed", "ba", "sectreas", "sectrint", "prsceo", "mdphd", "ihm",
                 "sen", "facog", "caqsh", "mmm", "fccp", "fc", "msm", "legal", "offi", "fann", "svpcfo", "executive", "dpt", "foo", "network dev", 
                 "cfoasst", "pc-a", "bds", "cme", "pp", "psy d", "vice", "key employe", "senior", "phcns", "fmm", "presidenceo", "mrcp", "lohr",
                 "lmhc", "mppm", "lcsw", "rnc-ob", "fabc", "bcps", "dabr", "csjp", "business", "cfe", "chfp", "counselman", "cpcu", "key", "pbvm",
                 "pres", "directorvp", "chairpers", "cnl", "non-voting", "jul", "vpcfocoo", "financial", "till", "mpa", "gen'l", "counse", "msf",
                 "exec", "dirintrm", "cgma", "fhfma", "frmr", "reverend", "ed", "acm", "fa", "mlt", "cfa", "umms rep", "chairperson", "voting", 
                 "annual life", "jrdo", "ssm", "mdpres", "med", "fabf", "ncpsya", "fac", "pr", "llm", "mshcm", "- mar", "crmc", "cno", "mhcds", 
                 "facr", "senator", "finance", "mso", "rhsj", "mot otrl", "ocn", "lsw", "mmhc", "cfre", "msa", "cws", "dph", "mscprp", "dec-june",
                 "mdms", "mdret", "dpa", "rdh", "faia", "jwc", "chairman", "msmphrncph", "fmp", "physd", "phdcne", "- dec", "then", "clinical",
                 "lpd", "-sept", "-may", "june", "incoming", "elect", "macp", "- reg", "- treas", "strategy", "dmv", "membervp", "jr", "feb-dec", "-aug",
                 "- dec", "cnsl", "empl", "hcomp", "and", "admin", "dec-mar", "rtt", "may-dec", "svpchief", "cic", "mbamsnrn", "msrncnscenp",
                 "dm", "medical", "evp", "patient", "philanthropy", "clinic", "ahmc", "division", "-care div med th", "-care div fin", "-care div ops",
                 "-cvn clinics", "-care div", "chse", "dns", "care division", "- nov", "south meadows", "cmo","pmh", "mpp", "ahdl", "-dec", "cpmsm",
                 "hacp", "coo", "dnpapn", "anp", "pastor", "dnpmsnrn", "mgmt", "secrtreas", "jrv", "vpceo", "scch", "frcp", "ssj", "ncmp", "mscr", "dsf",
                 "mbr", "mspt", "jrdo", "clin", "coo", "svpchief", "sectreasurer", "and", "msha", "mphd", "apa", "ahp", "ee", "contracted", "phr",
                 "iii", "facg", "mdterm", "macp", "lcpc", "ccp", "rnc", "msob", "ceosecretary", "wchd", "cfosr", "cfovp", "cphrm", "cpps", "cooasst",
                 "treasto", "-directorjul-apr", "apr", "fmp", "ops", "scrn", "msnrn", "rnc", "fccm", "mdoff", "informa", "family", "orthopedic", 
                 "ppcme", "august", "partial", "lmh", "evp ballad health", "past", "tmh", "- aug", "evp", "lmhtrh", "chairperson", "treas", "nov", "dec",
                 "rtrtc", "dmo", "truste", "presiden", "fach", "fhf", "year", "prn", "execut", "february", "frm", "- mar", "ncr", "fdtn", "ipd", 
                 "aug", "nov", "feb", "only", "part yr", "care div", "-cur", "- mar", " - care", "- reg", "-c", "- fin", "-directorjul", "-july",
                 "acns", "mpah", "achce", "judge", "rabbi", "rc", "jrphd", "since", "sept", "ex-officio", "jun", "sep", "aca", "nd", "sec", "co", "stm", "jr", "dd", "ch",
                 "vg", "faacvpr", "svcs", "ahhm", "care", "see sch", "sch", "inc", "vcos", "jrpa", "ahuv", "yr", "rahn", "syed", "cos", "ehfph", "ehmc", "msph",
                 "doctor", "-mar", "rp", "dp", "- part", "chfc", "cva", "edp", "abpp", "mcg", "bchphd", "ng", "presdir", "trea", "lssgb", "ret", "- ret", "-ret", "jcd",
                 "jcl", "promoted", "msrn", "by", "dw", "september", "october", "november", "december", "january", "february", "cff", "cpcs", "llc", "phdeff", "cjsp", "apn",
                 "cna", "nonvoting", "nha", "rnmsnmba", "dnprn", "healthcare", "mcch", "varied", "ccm", "ex - officio", "system", "vc", "fmccoo ws mkt", "fmccoo", "ccvi",
                 "company", "srs", "dcm", "staff", "aicp", "mp", "inter hlth inv", "cr", "oda", "- part", "liasion", "rotated", "iiphd", "interventional", "thoracic", 
                 "jrmdphd", "facoep", "mc", "in march", "cmoasst", "fsm", "frcpc", "td", "deemed", "officer", "phdc", "in", "rcrh", "dphil", "cjs", "iii", "pc", "exited",
                 "phh", "esp", "jdllm", "faa", "ballad health", "health", "rtrtc", "jrtermed", "operating", "-directorjul", "evpgen cncl", "mbataru", "cm", "rtr", "mar", "faafp",
                 "- reg", "faoao", "fabfp", "ahfr", "lfache", "july", "jrv", "bus develop", "ceopresident", "cns", "mbacpamha", "jr", "msci", "assistant", "rnmbafache", "epps",
                 "jcmc", "-acute", "evpcfo", "fsso", "-pediatric", "cio", "cfoevp", "human", "associate", "faccp", "ods", "rep")

# Create name_cleaned variable
early_data <- early_data %>%
  mutate(name_cleaned = PersonNm)

# remove "see schedule o" patterns
early_data <- early_data %>%
  mutate(name_cleaned = str_remove(name_cleaned, "see sch o|see statement|see sch j|see sched"))

# Remove punctuation and digits from names
early_data <- early_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "\\.|,|'")) %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "[0-9]+")) %>%
  mutate(name_cleaned = str_squish(name_cleaned))

# update indicator columns (keep 1s, update 0s if string detected)
update_indicator <- function(existing_col, names_vector, title_list) {
  pattern <- paste(title_list, collapse = "|")
  detected <- as.integer(grepl(pattern, names_vector, ignore.case = TRUE))
  pmax(existing_col, detected)  # keeps 1 if either is 1
}

early_data$doctor      <- update_indicator(early_data$doctor,       early_data$PersonNm, doctor_list)
early_data$other_doctor <- update_indicator(early_data$other_doctor, early_data$PersonNm, other_doctor_list)
early_data$nurse       <- update_indicator(early_data$nurse,        early_data$PersonNm, nurse_list)
early_data$ha          <- update_indicator(early_data$ha,           early_data$PersonNm, ha_list)

# remove anything occuring after the words "paid by" or in parentheses
early_data <- early_data %>%
  mutate(name_cleaned = str_remove(name_cleaned, "paid by .+$")) %>%
  mutate(name_cleaned = str_remove(name_cleaned, "\\(.+\\)"))

# remove any titles or extra from name_cleaned column
all_remove_list <- c(doctor_list, other_doctor_list, nurse_list, ha_list, remove_list)

early_data <- early_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, paste(all_remove_list, collapse = "\\b|\\b")))

# Remove dashes that happen at the end
early_data <- early_data %>%
  mutate(name_cleaned = str_squish(name_cleaned)) %>%
  mutate(name_cleaned = str_remove(name_cleaned, "-$")) %>%
  mutate(name_cleaned = str_squish(name_cleaned)) %>%
  mutate(name_cleaned = str_remove(name_cleaned, "-$")) 

# Remove parentheses
early_data <- early_data %>%
  mutate(name_cleaned = str_remove_all(name_cleaned, "(|)"))

# Remove "esq" occuring at the end
early_data <- early_data %>%
  mutate(name_cleaned = str_remove(name_cleaned, "esq$"))


xml_data <- bind_rows(early_data, xml_data)

xml_data <- xml_data %>%
  distinct()

# look at the number of distinct EINs in each year
xml_data %>%
  group_by(TaxYr) %>%
  distinct(Filer.EIN) %>%
  summarise(n())

# drop "former" people
xml_data <- xml_data %>%
  filter(former_ofcr_director_trustee==FALSE) %>%
  select(-former_ofcr_director_trustee) %>%
  filter(!str_detect(TitleTxt, "former|frmr|past|pst|fmr")) %>%
  filter(!str_detect(name_cleaned, "former|frmr|past|pst"))

# drop institutional trustees
xml_data <- xml_data %>%
  filter(institutional_trustee==FALSE) %>%
  select(-institutional_trustee)

# Categorize people into specific positions 

# Assign common executive titles first and don't let them change later
xml_data <- xml_data %>%
  mutate(position = ifelse(str_detect(TitleTxt, "ceo|chief executive|chief exec|cheif exec|c.e.o|exec dir|executive dir") | str_detect(PersonNm, "ceo|chief executive|chief exec|cheif exec|c.e.o"), "ceo", NA)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "administrato"), "administrator", position)) %>%
  mutate(position = ifelse(is.na(position) & (str_detect(TitleTxt, "cfo|chief fin|cheif fin|controller|c.f.o") | str_detect(PersonNm, "cfo|chief fin|cheif fin|controller|c.f.o")), "cfo", position)) %>%
  mutate(position = ifelse(is.na(position) & (str_detect(TitleTxt, "cmo|chief med|chief of clinical staff|cheif med|medical officer") | str_detect(PersonNm, "cmo|chief med|chief of clinical staff|cheif med")), "cmo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "cio|chief info|cheif info|cmio"), "cio", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "cno|chief nurs|cheif nurs"), "cno", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "coo|chief oper|c.o.o|cheif oper"), "coo", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "vp|vice pres|v.p"), "vp", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "exec|chief|cao|cco|general counsel|cmdo|cqo|cro"), "other exec", position))


# assign them as a board member if the only box checked is individual trustee no matter what the title text says 
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position) & individual_trustee_or_director==TRUE & officer_ind==FALSE & key_employee_ind==FALSE & highest_compensated_employee==FALSE, "board", position))

# Assign people who are board members with officer titles within the board
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position) & officer_ind==TRUE & str_detect(TitleTxt, "chair|secretary|treas|board|member|director|trustee"), "board", position)) %>%
  mutate(position = ifelse(is.na(position) & individual_trustee_or_director==TRUE & officer_ind==TRUE & str_detect(TitleTxt, "pres"), "board", position))

# Assign presidents who have high hours to executive president position
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "pres") & avg_hours_per_week>=15, "exec pres", position)) %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "pres"), "board", position))

# Assign physicians who are highest compensated employees
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position) & TitleTxt%in%c("physician", "surgeon", "pharmacist", "rn", "crna", "orthopedics", 
                                                                                                "cardiologist", "hospitalist", "registered nurse",
                                                                                                "physician assistant", "nurse practitioner", "md",
                                                                                                "psychiatrist", "doctor", "orthopedic surgeon",
                                                           "anesthesiologist", "staff physician", "er physician", "general surgeon", "radiologist",
                                                           "neurosurgeon", "physical therapist", "clinical pharmacist", "medical doctor",
                                                           "nurse anesthetist", "aprn", "nurse", "nurse practitioner", "fnp", "nurse practioner", "physicist",
                                                           "medical physicist", "employed physician", "ed physician", "pa", "pediatrician", "family practice physician", "rn ii",
                                                           "attending physician"), "employee", position)) 

# Assign typical key employee roles that aren't necessarily execs
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "division chief|physician chief|medical staff president|medical dir|director of|pharmacy director|pharmacy manager|director pharm|dir pharm|dir of|dir nurs"), "non-exec leader", position)) 

# Anyone left who has director and a positive compensation is probably a more senior leader
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "director") & avg_hours_per_week>=30, "non-exec leader", position))

# catch any baord members that had weird indicator values
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position) & str_detect(TitleTxt, "board member|trustee"), "board", position))

observe <- xml_data %>%
  filter(is.na(position)) 

observe %>% count(TitleTxt, sort=TRUE)

# everyone left is just a very specific specialty physician. assign them all as employees
xml_data <- xml_data %>%
  mutate(position = ifelse(is.na(position), "employee", position))

# Now look at only executives
exec_data <- xml_data %>%
  filter(position%in%c("other exec", "exec pres", "ceo", "cfo", "cno", "cio", "cmo", "coo", "administrator")) %>%
  select(-individual_trustee_or_director, -highest_compensated_employee, -key_employee_ind, -TitleTxt, -officer_ind)

# How many EINs in each year have execs? 
exec_data %>%
  group_by(TaxYr) %>%
  distinct(Filer.EIN) %>%
  summarise(n())

# categorize people into full-time (all hours at this org), part-time (split hours), or no-time (majority hours at related org)
exec_data <- exec_data %>%
  mutate(avg_hours_per_week_rltd_org = ifelse(is.na(avg_hours_per_week_rltd_org),0,avg_hours_per_week_rltd_org)) %>%
  mutate(time_allocation = ifelse(avg_hours_per_week>=.75*(avg_hours_per_week + avg_hours_per_week_rltd_org), "full-time", NA)) %>%
  mutate(time_allocation = ifelse(avg_hours_per_week_rltd_org>=.75*(avg_hours_per_week + avg_hours_per_week_rltd_org), "no-time", time_allocation)) %>%
  mutate(time_allocation = ifelse(is.na(time_allocation), "part_time", time_allocation))

exec_data %>% count(time_allocation)

observe <- exec_data %>%
  filter(time_allocation=="no-time")

#define threshold for "few"
threshold <- 1   # change to 0, 1, 2, etc. depending on your rule

exec_clean <- exec_data %>%
  group_by(Filer.EIN, TaxYr) %>%
  mutate(
    n_working = sum(time_allocation %in% c("full-time", "part-time"), na.rm = TRUE)
  ) %>%
  filter(
    # keep everything if few/no workers
    n_working <= threshold |
      # otherwise drop no-timers
      (n_working > threshold & time_allocation != "no-time")
  ) %>%
  ungroup() %>%
  select(-n_working)

# what's the average team size?
exec_clean %>% group_by(TaxYr, Filer.EIN) %>%
  summarise(obs_count = n()) %>%
  group_by(TaxYr) %>%
  summarise(avg = mean(obs_count), .groups="drop")

# create column for total_compensation
exec_clean <- exec_clean %>%
  rowwise() %>%
  mutate(total_comp = sum(reportable_comp_from_org, reportable_comp_from_rltd_org, other_compensation, na.rm=TRUE)) %>%
  ungroup()

# combine name matches 
# standardize names within the same EIN
exec_clean <- combine_fuzzy_matches(exec_clean, max_dist = 2)

# match names within the same EIN that are similar based on token overlap, only keeping the shorter of the two names
exec_clean <- combine_token_matches(exec_clean, min_shared_tokens = 2)

# catch cases like "jackson david" vs "jacksondavid w" where one name has no space
exec_clean <- combine_nospace_matches(exec_clean)

# try to extract first names so I can run the gender algorithm
# Read in Census data for those boarn in 2000
firstnames <- read_csv(paste0(raw_data_path, "/yob2000.txt"), col_names = FALSE) %>%
  filter(X3>50) %>%
  select(X1, X3) %>%
  rename(name=X1, count=X3) %>%
  mutate(name=tolower(name)) %>%
  group_by(name) %>%
  summarise(count = max(count)) %>%
  ungroup()
add_firstnames <- c("doug", "jef", "cyndy", "waine", "philippe", "jil", "pam", "justino", "beverley", "evalie", "wendall",
                    "krzystof", "berton", "terrie", "matt", "sheri", "dollie", "joette", "bob", "adrina", "seema", "aj",
                    "antonietta", "archie", "philippa", "daphnee", "dona", "chuck", "franca", "phil", "pat", "roddy",
                    "nicolene", "wilfred", "dirk", "lynne", "bud", "felice", "sharita", "gwenn", "delores", "maybelle",
                    "gail", "lauretta", "jj", "dwain", "cj", "katharin", "hal", "ladena", "stan", "rob", "penni", "polly", 
                    "rickie", "sheryl", "luella", "ritchie", "deb", "jeremie", "kraig", "carole", "tonja", "georgiana",
                    "sallie", "thor", "bart", "kerrie", "ward", "cliff", "derrek", "bernadine", "herb", "dotty", "christi",
                    "gregg", "sheri", "sue", "elayne", "chrissy", "phil", "vickie", "phyllis", "ned", "marsha", "clyde",
                    "barb", "bert", "stan", "cliff", "trudy", "patti", "rhoda", "rosy", "barrie", "cheryle", "luann",
                    "maurita", "shari", "val", "dick", "rich", "gayle", "nicki", "julee", "deedra", "denyse", "gretta",
                    "mistie", "carlyle", "zac", "russ", "catie", "jeanene", "patsy", "kathi", "elsbeth", "roseanne", "geoff",
                    "mitch", "cris", "shondra", "roxie", "kip", "mimi", "buddy", "reecia", "claudette", "tammie", 
                    "dorris", "roddy", "lyn", "fran", "anabell", "marlo", "thelma", "vern", "jen", "bud", "suzannah", 
                    "howie", "lonna", "steph", "danile", "halsey", "carole", "katharin", "butch", "tod", "susanne", 
                    "sharlene", "gerry", "wilbur", "ollie", "tadd", "sherri", "georgette", "vicki", "patty", "taisha")
firstnames_list <- append(as.list(firstnames)[["name"]], add_firstnames)

# remove initials and "-"
exec_clean <- exec_clean %>%
  mutate(name_cleaned = str_trim(name_cleaned)) %>%
  mutate(formatted_name = str_replace_all(name_cleaned, "(^|\\s)[a-z](?=\\s|$)", "\\1")) %>%
  mutate(formatted_name = str_squish(formatted_name)) %>%
  mutate(formatted_name = str_replace_all(formatted_name, "-"," ")) %>%
  mutate(formatted_name = str_replace(formatted_name, " van ", " van"))

# next, split phrase up into individual words
exec_clean <- exec_clean %>%
  mutate(formatted_name = str_squish(formatted_name)) %>%
  tidyr::separate_wider_delim(formatted_name, delim=" ", names_sep = ".", too_few = "align_start")

# count up number of times first names occur in the first word of name_cleaned in a firm-year
exec_clean <- exec_clean %>%
  mutate(first_count = ifelse(formatted_name.1 %in% firstnames_list,1,0)) %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(total_names = n()) %>%
  mutate(first_first = sum(first_count)) %>%
  ungroup()

# If the majority of names have a first name located first, record that as the first name (even if there are > 2 names because it shouldn't matter)
exec_clean <- exec_clean %>%
  mutate(first_name = ifelse(first_first/total_names>=.8, formatted_name.1, NA))

# fill first_name
exec_clean <- exec_clean %>%
  group_by(name_cleaned, Filer.EIN) %>%
  fill(first_name, .direction="downup") %>%
  ungroup()

# If the majority of names have a first name located second, record that as the first name
# exclude firms where most first words are single-letter initials ("initial lastname" format)
exec_clean <- exec_clean %>%
  group_by(TaxYr, Filer.EIN) %>%
  mutate(initial_first = sum(nchar(formatted_name.1) == 1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(first_name = ifelse(first_first/total_names<=.2 & is.na(formatted_name.3) & is.na(first_name) &
                               initial_first/total_names < 0.5, formatted_name.2, first_name))

# fill first_name when applicable
exec_clean <- exec_clean %>%
  group_by(name_cleaned, Filer.EIN) %>%
  fill(first_name, .direction="downup") %>%
  ungroup()

# Count up the number of times a first name is found
# If exactly one is found, assign that to first name
exec_clean <- exec_clean %>%
  rowwise() %>%
  mutate(num_first_across = sum(c_across(starts_with("formatted_name.")) %in% firstnames_list)) %>%
  mutate(first_name = if_else(num_first_across == 1 & is.na(first_name),
                              c_across(starts_with("formatted_name.")) %>% .[. %in% firstnames_list] %>% .[1],
                              first_name)) %>%
  ungroup()

# If multiple first names found but the last word is not a first name, pick the first matching word
# (e.g. "mary ann smith" -> "mary"; avoids ambiguity when last word could also be a first name)
exec_clean <- exec_clean %>%
  rowwise() %>%
  mutate(first_name = {
    if (!is.na(first_name) || num_first_across <= 1) {
      first_name
    } else {
      words <- na.omit(c_across(starts_with("formatted_name.")))
      if (length(words) > 0 && !(tail(words, 1) %in% firstnames_list)) {
        matches <- words[words %in% firstnames_list]
        if (length(matches) > 0) matches[1] else first_name
      } else {
        first_name
      }
    }
  }) %>%
  ungroup()

# If the name "mary" is found, assign that as first name
exec_clean <- exec_clean %>%
  mutate(first_name = ifelse(str_detect(name_cleaned, "mary\\b"), "mary", first_name),
         first_name = ifelse(str_detect(name_cleaned, "elizabeth\\b"), "elizabeth", first_name),
         first_name = ifelse(str_detect(name_cleaned, "\\banne\\b"), "anne", first_name),
         first_name = ifelse(str_detect(name_cleaned, "\\bjohn\\b"), "john", first_name))

# look at distinct names that don't have first name assignment
observe <- exec_clean %>%
  filter(is.na(first_name)) %>%
  distinct(name_cleaned, num_first_across)


# Look at whether the same name ever gets assigned different first names in different years
multiple_firstnames <- exec_clean %>%
  filter(!is.na(first_name)) %>%
  group_by(name_cleaned, Filer.EIN) %>%
  summarise(
    unique_first_names = n_distinct(first_name),
    first_names_found = paste(sort(unique(first_name)), collapse = " | ")
  ) %>%
  filter(unique_first_names > 1) %>%
  arrange(desc(unique_first_names)) %>%
  ungroup()
#none

### DATASET 1: run gender algorithms on those with first names
people_has_first <- exec_clean %>%
  filter(!is.na(first_name))

# Use "gender" package to predict gender of each name
genders <- people_has_first %>%
  distinct(first_name) %>%
  rowwise() %>%
  do(results = gender(.$first_name, years = c(1960,2000), method="ssa")) %>%
  mutate(n = nrow(results)) %>%
  filter(n>0) %>%
  do(bind_rows(.$results)) %>%
  select(-year_min, -year_max)

# join back to original data
people_has_first <- people_has_first %>%
  left_join(genders, by=c("first_name"="name")) %>%
  rename(prob_male_r = proportion_male, prob_female_r=proportion_female, gender_r=gender)

### DATASET 2: people with missing first name — run gender algorithms on every name part
people_no_first <- exec_clean %>%
  filter(is.na(first_name))

# pivot all name-part columns long so we can run algorithms on each word
name_parts_no_first <- people_no_first %>%
  select(name_cleaned, starts_with("formatted_name.")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("formatted_name."),
               names_to  = "position",
               values_to = "name_part") %>%
  filter(!is.na(name_part))

# run gender package on all unique name parts
genders_no_first <- name_parts_no_first %>%
  distinct(name_part) %>%
  rowwise() %>%
  do(results = gender(.$name_part, years = c(1960, 2000), method = "ssa")) %>%
  mutate(n = nrow(results)) %>%
  filter(n > 0) %>%
  do(bind_rows(.$results)) %>%
  select(-year_min, -year_max)

# join gender-package results back to name parts
name_parts_no_first <- name_parts_no_first %>%
  left_join(genders_no_first, by = c("name_part" = "name")) %>%
  rename(prob_male_r = proportion_male, prob_female_r = proportion_female, gender_r = gender)

name_parts_no_first <- name_parts_no_first %>%
  group_by(name_cleaned) %>%
  mutate(gender_r = ifelse(length(unique(gender_r))==1, gender_r, NA)) %>%
  mutate(prob_male_r = ifelse(!is.na(gender_r), min(prob_male_r), NA)) %>%
  ungroup() %>%
  distinct(name_cleaned, prob_male_r, gender_r)

# join back to people_no_first
people_no_first <- people_no_first %>%
  left_join(name_parts_no_first, by="name_cleaned")

# Merge dataset 1 and dataset 2
exec_data_withgender <- bind_rows(people_no_first, people_has_first)

# only keep relevant variables
exec_data_withgender <- exec_data_withgender %>%
  select(TaxYr, Filer.EIN, name_cleaned, first_name, position, total_comp, doctor, other_doctor, nurse, ha, time_allocation, gender_r)

# make sure someone is always or never a doctor/nurse
exec_data_withgender <- exec_data_withgender %>%
  group_by(name_cleaned, Filer.EIN) %>%
  mutate(doctor=max(doctor), nurse=max(nurse)) %>%
  ungroup()

summary(exec_data_withgender)


# Create hospital-level summary variables of leadership team
hospital_exec_data <- exec_data_withgender %>%
  group_by(Filer.EIN, TaxYr) %>%
  summarise(
    # Team size
    team_size = n(),
    
    # Percent clinical (doctor, other_doctor, or nurse)
    percent_clinical = mean(pmax(doctor, other_doctor, nurse), na.rm = TRUE),
    
    # Percent MD (doctor only)
    percent_md = mean(doctor, na.rm = TRUE),
    
    # Total team compensation
    total_team_comp = sum(total_comp, na.rm = TRUE),
    
    # CEO/President compensation
    ceo_comp = sum(total_comp[grepl("ceo|exec pres", position, ignore.case = TRUE)], na.rm = TRUE),
    
    # Indicator for having a CMO
    has_cmo = as.integer(any(grepl("cmo", position, ignore.case = TRUE))),
    
    # Percent female
    percent_female = mean(gender_r == "female", na.rm = TRUE),
    
    .groups = "drop"
  )

summary(hospital_exec_data)  

  
  
  

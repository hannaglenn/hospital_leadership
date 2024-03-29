library(readr)
library(dplyr)
library(stringr)
library(tidyr)

source("paths.R")

# read in names data
cleaned_text <- read_rds(paste0(created_data_path, "cleaned_text.rds"))

# First objective is to identify whether someone is part of the board or part of the actual day to day decision making
# if the only position is physician, drop them as they are just a high compensated employee
cleaned_text <- cleaned_text %>%
  filter(!(position1 %in% c("physician", "anesthesiologist", "dermatologist", "surgeon") & is.na(position2)))

# ceo sometimes got stuck in the "extra" text, move it to position 1 (this is always an exec person)
cleaned_text <- cleaned_text %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"ceo\\b"),"ceo",position1)) %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"cfo\\b"),"cfo",position1)) %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"cmo\\b"),"cmo",position1)) %>%
  mutate(position1 = ifelse(!is.na(extra) & str_detect(extra,"cqo\\b"),"cqo",position1))

# First goal: categorize each name as board member or executive (decision maker) ####

# buckets I need to make sure my categories are correct:
# 1. do they have a board member title?
# 2. do they have an "executive" title?
# 3. are they a physician on top of another position?
# 4. president is tricky. have a different indicator
# 5. Keep an indicator for whether they are a vice president *I may drop these as robustness check later)

# make indicators for whether position 1 and position 2 are board or not
cleaned_text <- cleaned_text %>%
  mutate(pos1_board = ifelse(position1 %in% c("board", "trustee", "director", "chair", "vice chair", "chairman", "vice chairman", "treasurer", "secretary", "division chief", "chairperson", "ex officio"),1,0)) %>%
  mutate(pos2_board = ifelse(position2 %in% c("board", "trustee", "director", "chair", "vice chair", "chairman", "vice chairman", "treasurer", "secretary", "division chief", "chairperson", "ex officio"),1,0))

# make indicators for whether position1 and position2 are for sure executive members (leave president out for now)
cleaned_text <- cleaned_text %>%
  mutate(pos1_exec = ifelse(position1 %in% c("medical director", "ceo", "cfo", "system executive", "executive", "coo", "chief medical officer", "chief executive officer", "chief financial officer", "cpo", "cmo", "cno", "cqo", "chief quality officer"), 1, 0)) %>%
  mutate(pos2_exec = ifelse(position2 %in% c("medical director", "ceo", "cfo", "system executive", "executive", "coo", "chief medical officer", "chief executive officer", "chief financial officer", "cpo", "cmo", "cno", "cqo", "chief quality officer"), 1, 0))

# make indicators for whether position1 or position2 indicate president or vice president
cleaned_text <- cleaned_text %>%
  mutate(pos1_pres = ifelse(position1 %in% c("president", "pres", "vp", "vice president", "peesident"), 1, 0)) %>%
  mutate(pos2_pres = ifelse(position2 %in% c("president", "pres", "vp", "vice president", "peesident"), 1, 0)) %>%
  mutate(pos1_vp = ifelse(position1 %in% c("vp", "vice president"), 1, 0)) %>%
  mutate(pos2_vp = ifelse(position2 %in% c("vp", "vice president"), 1, 0))

# make indicators for whether position1 or position2 indicate physician
cleaned_text <- cleaned_text %>%
  mutate(pos1_phys = ifelse(position1 %in% c("physician", "anesthesiologist", "dermatologist", "surgeon"), 1, 0)) %>%
  mutate(pos2_phys = ifelse(position2 %in% c("physician", "anesthesiologist", "dermatologist", "surgeon"), 1, 0)) %>%
  filter(!(pos1_phys==1 & pos2_phys==1))

# create indicator for extra text being informative of hospital leadership
cleaned_text <- cleaned_text %>%
  mutate(extra_text_hosp = ifelse(!is.na(extra) & str_detect(extra, "\\bof\\b|^of\\b|medical staff|mc\\b|officer|senior|center|professional|network|project|hhc|clinic|qual|relation|facilit|manage|support|revenue|strategic|affairs|acute|planning|legal|oficer|region|development|info|develop|counsel|external|hosp|community|chief|finan|service|admin|health|business|care|general|tech|exec|oper|practice|market|med|fiscal|nursing|human|hs\\b|corporate|hr|services|medical|patient"),1,0))

# Conditions on the position of the person
# 1. If either position is exec, the person is exec
# 2. If its board + anything other than exec or president, the person is board
# 3. If it's president + nothing else, with extra text that indicates hospital, they are exec
# 4. If it's president + board member, classify as board
# 5. Anyone left we don't know if they are board or decision makers

cleaned_text <- cleaned_text %>%
  mutate(position = ifelse(pos1_exec==1 | pos2_exec==1, "executive", NA)) %>% #1
  mutate(position = ifelse(is.na(position) & pos1_board==1 & pos2_pres==0, "board", position)) %>% #2
  mutate(position = ifelse(is.na(position) & pos2_board==1 & pos1_pres==0, "board", position)) %>% #2
  mutate(position = ifelse(is.na(position) & pos1_pres==1 & pos2_board==0 & extra_text_hosp==1, "executive", position)) %>% #3
  mutate(position = ifelse(is.na(position) & pos2_pres==1 & pos1_board==0 & extra_text_hosp==1, "executive", position)) %>% #3
  mutate(position = ifelse(is.na(position) & pos1_pres==1 & pos2_board==1, "board", position)) %>% #4
  mutate(position = ifelse(is.na(position) & pos2_pres==1 & pos1_board==1, "board", position)) %>% #4
  mutate(position = ifelse(is.na(position), "president (board or executive)", position)) #5

cleaned_text <- cleaned_text %>%
  mutate(ceo = ifelse(position1=="ceo" | position2 == "ceo" | position1 == "chief executive officer" | position2 == "chief executive officer", 1,0)) %>%
  mutate(ceo = ifelse(is.na(ceo),0,ceo))
  
executive_data <- cleaned_text %>%
  mutate(vp = ifelse(pos1_vp==1 | pos2_vp==1, 1, 0)) %>%
  filter(position == "executive") %>%
  select(ein, year, name, position, title, former, vp, ceo) %>%
  filter(!(former %in% c("past", "former", "past interim")))

# loop through eins to fill in any missing years that happen between nonmissing years
eins <- unique(executive_data$ein)

# create empty data to fill
executive_data_filled <- executive_data %>% filter(year==0)

for (i in seq_along(eins)){
  data <- executive_data %>%
    filter(ein == eins[[i]]) %>%
    group_by(name, position) %>%
    mutate(id = cur_group_id()) %>%
    arrange(year) %>%
    ungroup() 
  
  # complete so that each group has a row in each year (only for now)
  data <- complete(data, year=2009:2016, id)
  
  max_id <- max(data$id)

  for (j in 1:max_id){
    name_data <- data %>%
      filter(id==j) %>%
      mutate(lag = lag(ein),
             lead = lead(ein)) %>%
      fill(lag, .direction="down") %>%
      fill(lead, .direction="up") %>%
      mutate(ein = ifelse(is.na(ein) & !is.na(lag) & !is.na(lead), lag, ein)) 
    
    name_data <- name_data %>%
      select(-lag, -lead) %>%
      filter(!is.na(ein)) %>%
      fill(name, position, .direction="downup")
    
    executive_data_filled <- rbind(executive_data_filled, name_data)
  }
}  

# fill MD
executive_data_filled <- executive_data_filled %>%
  group_by(ein, id) %>%
  fill(title, vp, ceo, .direction="downup") %>%
  ungroup()

# goal 2: identify whether changes occur from year to year ####

# get rid of hospitals who are not present in the data at least for 2010-2014
executive_data_filled <- executive_data_filled %>%
  mutate(in_2010 = ifelse(year==2010,1,NA),
         in_2011 = ifelse(year==2011,1,NA),
         in_2012 = ifelse(year==2012,1,NA),
         in_2013 = ifelse(year==2013,1,NA),
         in_2014 = ifelse(year==2014,1,NA)) %>%
  group_by(ein) %>%
  fill(in_2010, in_2011, in_2012, in_2013, in_2014, .direction="downup") %>%
  ungroup() %>%
  mutate(present_2010_2014 = ifelse(in_2010==1 & in_2011==1 & in_2012==1 & in_2013==1 & in_2014==1, 1, 0)) %>%
  mutate(present_2010_2014 = ifelse(is.na(present_2010_2014), 0 ,present_2010_2014)) 
  # 91% of observations are in the data

# keep a record of those that aren't to see if we can manually fill in some missing info caused by OCR
#executive_data_notin20102014 <- executive_data_filled %>%
  #filter(present_2010_2014==0)

# saveRDS(executive_data_notin20102014, paste0(created_data_path, "executive_data_notin20102014.rds"))
# write.csv(executive_data_notin20102014, paste0(created_data_path, "executive_data_notin20102014.csv"))

# now filter out those who are not in the data for this time period
executive_data_filled <- executive_data_filled %>%
  filter(present_2010_2014==1)

# figure out whether the leadership team has changed in a given year
detect_changes <- executive_data_filled %>% 
  group_by(ein, year) %>% 
  summarize(list_of_ids = paste(sort(unique(id)),collapse=", ")) %>%
  mutate(count=1) %>%
  group_by(ein) %>%
  mutate(lag_list = lag(list_of_ids)) %>%
  ungroup %>%
  mutate(any_change = ifelse(list_of_ids!=lag_list, 1, 0)) %>%
  mutate(any_change = ifelse(is.na(any_change), 0, any_change)) 

detect_changes <- detect_changes %>%
  mutate(year_of_any_change = ifelse(any_change==1, year, NA))

years_of_change <- detect_changes %>%
  group_by(ein) %>%
  summarize(any_change_years = paste(sort(unique(year_of_any_change)), collapse=", ")) %>%
  ungroup()

# create indicators for whether to keep the hospital in the case of using certain samples
years_of_change <- years_of_change %>%
  mutate(no_changes_ever = ifelse(any_change_years=="", 1, 0),
         no_changes_2010_2014 = ifelse(any_change_years=="" | !str_detect(any_change_years, "2010|2011|2012|2013|2014"), 1, 0),
         no_changes_2011_2013 = ifelse(any_change_years=="" | !str_detect(any_change_years, "2011|2012|2013"), 1, 0)) %>%
  select(-any_change_years)

# merge back to executive data filled
executive_data_filled <- executive_data_filled %>%
  left_join(years_of_change, by="ein")

# now try to loosen the definition of change to get a larger sample size

detect_changes <- executive_data_filled %>% 
  group_by(ein, year) %>% 
  summarize(list_of_ids = paste(sort(unique(id)),collapse=", ")) %>%
  mutate(count=1) %>%
  group_by(ein) %>%
  mutate(lag_list = lag(list_of_ids)) %>%
  ungroup %>%
  mutate(small_change = ifelse(str_detect(list_of_ids, lag_list) | str_detect(lag_list, list_of_ids), 0, 1)) 

detect_changes <- detect_changes %>%
  mutate(year_of_small_change = ifelse(small_change==1, year, NA))

years_of_change <- detect_changes %>%
  group_by(ein) %>%
  summarize(small_change_years = paste(sort(unique(year_of_small_change)), collapse=", ")) %>%
  ungroup()

years_of_change <- years_of_change %>%
  mutate(no_small_changes_ever = ifelse(small_change_years=="", 1, 0),
         no_small_changes_2010_2014 = ifelse(small_change_years=="" | !str_detect(small_change_years, "2010|2011|2012|2013|2014"), 1, 0),
         no_small_changes_2011_2013 = ifelse(small_change_years=="" | !str_detect(small_change_years, "2011|2012|2013"), 1, 0)) %>%
  select(-small_change_years)

# merge back to executive data
executive_data_filled <- executive_data_filled %>%
  left_join(years_of_change, by="ein")


# finally, think about changes only in the sense of whether the leadership changed the presence of a doctor or not
executive_data_filled <- executive_data_filled %>%
  mutate(doctor = ifelse(title %in% c("md", "do"), 1, 0)) %>%
  select(-in_2011, -in_2012, -in_2013)

doctors_over_time <- executive_data_filled %>%
  group_by(ein, year) %>%
  mutate(num_doctors = sum(doctor)) %>%
  ungroup() %>%
  distinct(ein, year, num_doctors)

doctors_over_time <- doctors_over_time %>%
  group_by(ein) %>%
  mutate(lag_num_doctors = lag(num_doctors)) %>%
  ungroup() %>%
  mutate(change = ifelse(num_doctors!=lag_num_doctors, 1, 0)) %>%
  mutate(change = ifelse(is.na(change), 0, change)) %>%
  mutate(year_of_change = ifelse(change==1, year, NA))

years_of_change <- doctors_over_time %>%
  group_by(ein) %>%
  summarize(md_change_years = paste(sort(unique(year_of_change)), collapse=", ")) %>%
  ungroup()

years_of_change <- years_of_change %>%
  mutate(no_md_changes_ever = ifelse(md_change_years=="", 1, 0),
         no_md_changes_2010_2014 = ifelse(md_change_years=="" | !str_detect(md_change_years, "2010|2011|2012|2013|2014"), 1, 0),
         no_md_changes_2011_2013 = ifelse(md_change_years=="" | !str_detect(md_change_years, "2011|2012|2013"), 1, 0)) %>%
  select(-md_change_years)

# merge back to executive data
executive_data_filled <- executive_data_filled %>%
  left_join(years_of_change, by="ein")

# figure out whether ceo changed over time
detect_changes <- executive_data_filled %>% 
  filter(ceo==1) %>%
  group_by(ein, year) %>% 
  summarize(list_of_ids = paste(sort(unique(id)),collapse=", ")) %>%
  mutate(count=1) %>%
  group_by(ein) %>%
  mutate(lag_list = lag(list_of_ids)) %>%
  ungroup %>%
  mutate(any_change = ifelse(list_of_ids!=lag_list, 1, 0)) %>%
  mutate(any_change = ifelse(is.na(any_change), 0, any_change)) 

detect_changes <- detect_changes %>%
  mutate(year_of_any_change = ifelse(any_change==1, year, NA))

years_of_change <- detect_changes %>%
  group_by(ein) %>%
  summarize(any_change_years = paste(sort(unique(year_of_any_change)), collapse=", ")) %>%
  ungroup()

# create indicators for whether to keep the hospital in the case of using certain samples
years_of_change <- years_of_change %>%
  mutate(no_ceo_changes_ever = ifelse(any_change_years=="", 1, 0),
         no_ceo_changes_2010_2014 = ifelse(any_change_years=="" | !str_detect(any_change_years, "2010|2011|2012|2013|2014"), 1, 0),
         no_ceo_changes_2011_2013 = ifelse(any_change_years=="" | !str_detect(any_change_years, "2011|2012|2013"), 1, 0)) %>%
  select(-any_change_years)

# merge back to executive data filled
executive_data_filled <- executive_data_filled %>%
  left_join(years_of_change, by="ein")



# make a separate data set of ein, name, title, and years in data to be used for physician name matching
# only keep the ones with no MD changes from 2010-2014
names_data <- executive_data_filled %>%
  filter(no_md_changes_2010_2014==1) %>%
  group_by(ein, name, title) %>%
  summarize(years = paste(sort(unique(year)), collapse=", ")) %>%
  ungroup()

saveRDS(names_data, paste0(created_data_path, "names_data.rds"))

# create ein-level summary stats about number of doctors and number of total of executives
executive_data_filled <- executive_data_filled %>%
  group_by(ein, year) %>%
  mutate(count=1) %>%
  mutate(total_execs = sum(count),
         total_docs = sum(doctor)) %>%
  ungroup()

# aggregate to the hospital level
ein_leadership_changes_data <- executive_data_filled %>%
  distinct(ein, year, no_changes_ever, no_changes_2010_2014, no_changes_2011_2013, no_small_changes_ever, no_small_changes_2010_2014, no_small_changes_2011_2013,
           no_md_changes_ever, no_md_changes_2010_2014, no_md_changes_2011_2013, no_ceo_changes_ever, no_ceo_changes_2010_2014, 
           no_ceo_changes_2011_2013, total_execs, total_docs)

saveRDS(ein_leadership_changes_data, paste0(created_data_path, "ein_leadership_changes_data(temp).rds"))

num <- ein_leadership_changes_data %>%
  distinct(ein)
  # up to 911 eins after manually fixing some OCR mess ups









# load packages
library(tidyverse)
library(lme4)
library(coda)
library(here)

# set directory
### Path variables ----
here_path <- here::here() # Just using here for finding root path directories. It follows additional heuristics

### Some of the subdirectories
code_path <- here::here("04_-_data_manipulation", "scripts", "R", "text_exposure") # Using the base R function file.path
docs_path <- here::here("04_-_data_manipulation", "doc", "text_exposure")
data_path <- here::here("04_-_data_manipulation", "data", "raw_data", "text_exposure")
figs_path <- here::here("04_-_data_manipulation", "output")

# Read in the files
accuracy <- readr::read_csv(file = file.path(data_path,'Cleaned_211_All_accuracy.csv'))
data <- readr::read_csv(file = file.path(data_path, 'Cleaned_211_Correct.csv'))

# Rename the data column name and change variable types
data <- data |>
    janitor::clean_names()

accuracy <- accuracy |>
    janitor::clean_names()

# scale the scores and change the variable type to numeric
data$art_z <- as.numeric(scale(data$art_score_value))
data$re_z <- as.numeric(scale(data$re_score))

accuracy$art_z <- as.numeric(scale(accuracy$art_score_value))
accuracy$re_z <- as.numeric(scale(accuracy$re_score))

#code the comparison contrasts by assigning dummy coding
data$easy_hard <- as.numeric(with(data,
                                  ifelse(sentence_type == "Active" | SentenceType == "Passive",
                                         "-1",
                                         "1")))
data$easy <- as.numeric(with(data,
                             ifelse(sentence_type == "Active", "-1",
                                    ifelse(sentence_type == "Passive", "1",
                                           "0"))))
data$hard <-as.numeric(with(data,
                           ifelse(sentence_type == "SRC", "-1",
                                  ifelse(sentence_type == "ORC", "1",
                                         "0"))))

data$linear_trend <-as.numeric(with(data,
                                   ifelse(sentence_type == "Active", "-3",
                                          ifelse(sentence_type == "Passive", "-1",
                                                 ifelse(sentence_type == "SRC", "1",
                                                        "3")))))

accuracy$easy_hard <-
    as.numeric(with(accuracy,
                    ifelse(sentence_type == "Active" | sentence_type == "Passive",
                           "-1",
                           "1")))
accuracy$easy <-
    as.numeric(with(accuracy,
                    ifelse(sentence_type == "Acive", "-1",
                           ifelse(sentence_type=="Passive", "1",
                                  "0"))))
accuracy$hard <- as.numeric(with(accuracy,
                                 ifelse(sentence_type == "SRC", "-1",
                                        ifelse(sentence_type == "ORC", "1",
                                               "0"))))
accuracy$linear_trend <-
    as.numeric(with(accuracy,
                    ifelse(sentence_type == "Active", "-3",
                           ifelse(sentence_type == "Passive", "-1",
                                  ifelse(sentence_type =="SRC", "1",
                                         "3")))))


#code exploratory treatment contrast with Active sentences set as a baseline

data$condition <- as.factor(with(data,
                                 ifelse(sentence_type == "Active", "1",
                                        ifelse(sentence_type == "Passive", "2",
                                               ifelse(sentence_type =="SRC", "3",
                                                      "4")))))

accuracy$condition <-
    as.factor(with(accuracy,
                   ifelse(sentence_type == "Active", "1",
                          ifelse(sentence_type == "Passive", "2",
                                 ifelse(sentence_type == "SRC", "3",
                                        "4")))))


data$sentence_type <- as.factor(data$sentence_type)
accuracy$sentence_type <- as.factor(accuracy$sentence_type)




#Response time raw and log transformed vs ART and RE in separate models

art_orthogonal <-
    glmer(reading_time_ms ~ easy_hard * art_z + easy * art_z + hard * art_z +
              (1 | item_type) + (1 | participant_code),
          data = data)
summary(art_orthogonal)

art_orthogonal_log <-
    lmer(reading_time_log ~ ses_factor + easy_hard * art_z + easy * art_z + hard * art_z +
             (1 | item_type) + (1 | participant_code),
         data = data)
summary(art_orthogonal_log)


re_orthogonal <- lmer(reading_time_ms ~ easy_hard * re_z + easy * re_z + hard * re_z +
                          (1 | item_type) + (1 | participant_code),
                      data = data)
summary(re_orthogonal)

re_orthogonal_log <-
    lmer(reading_time_log ~ easy_hard * re_z + easy * re_z + hard * re_z +
             (1 | item_type) + (1 | participant_Code),
         data = data)
summary(re_orthogonal_log)


#both ART and RE in one model
art_re_orthogonal_three_way <-
    lmer(reading_time_ms ~ easy_hard * art_z + easy * art_z + hard * art_z +
             easy_hard * re_z + easy * re_z + hard * re_z +
             (1 | item_type) + (1 | participant_code),
         data = data)
summary(art_re_orthogonal_three_way)


art_re_orthogonal_three_way_log <-
    lmer(reading_time_log ~ easy_hard * art_z + easy * art_z+ hard * art_z +
             easy_hard * re_z + easy * re_z + hard * re_z +(1 | item_type) +
             (1 | participant_code),
         data = data)
summary(art_re_orthogonal_three_way_log)

#exploratory  analyses - treatment contrast
art_treatment_raw <- lmer(reading_time_ms ~ condition * art_z * ses_factor +
                              (1 | item_type) + (1 | participant_code),
                          data = data)
summary(art_treatment_raw)

art_treatment_log <- lmer(reading_time_log ~ condition * art_z * ses_factor +
                              (1 | item_type) + (1 | participant_code),
                          data = data)
summary(art_treatment_log)

re_treatment_raw <- lmer(reading_time_ms ~ condition * re_z * ses_factor +
                             (1 | item_type) + (1 | participant_code),
                         data = data)
summary(re_treatment_raw)
re_treatment_log <- lmer(reading_time_log ~ condition * re_z * ses_factor +
                             (1 | item_type) + (1 | participant_code),
                         data =data)
summary(re_treatment_log)

art_re_treatment_three_way <-
    lmer(reading_time_ms ~ condition * art_z + condition * re_z +
             (1 | item_type) + (1 | participant_code), data = data)
summary(art_re_treatment_three_way)
confint(art_re_treatment_three_way)

art_re_treatment_three_way_log <-
    lmer(reading_time_log ~ condition * art_z + condition * re_z +
             (1 | item_type) + (1 | participant_code),
         data = data)
summary(art_re_treatment_three_way_log)
confint(art_re_treatment_three_way_log)

anova(art_treatment_raw, art_re_treatment_three_way)

#Accuracy -

art_orthogonal <-
    glmer(accuracy ~ easy_hard * art_ z + easy * art_z + hard * art_z +
              (1 | item_type) + (1 | participant_code),
          data = accuracy,
          family = binomial)
summary(art_orthogonal)
confint(art_orthogonal)

re_orthogonal <-
    glmer(accuracy ~ easy_hard * re_z + easy * re_z + hard * re_z +
              (1 | item_type) + (1 | participant_code),
          data = accuracy,
          family = binomial)
summary(re_orthogonal)
confint(re_orthogonal)

art_treatment_accuracy <-
    glmer(accuracy ~ condition * art_z * ses_factor + (1 | item_type) + (1 | participant_code),
          data = accuracy,
          family = binomial)
summary(art_treatment_accuracy)

re_treatment_accuracy <-
    glmer(accuracy ~ condition * re_z * ses_factor + (1 | item_type) + (1 | participant_code),
          data = accuracy,
          family = binomial)
summary(re_treatment_accuracy)


art_re_orthogonal_three_way <-
    glmer(accuracy ~ easy_hard * art_z + easy * art_z + hard * art_z +
              easy_hard * re_z + easy * re_z + hard * re_z + (1 | item_type),
          data = accuracy,
          family = binomial)
summary(art_re_orthogonal_three_way)
confint(art_re_orthogonal_three_way)

art_re_treatment_three_way <-
    glmer(accuracy ~ condition * art_z + condition * re_z + (1 | item_type),
          data = accuracy,
          family = binomial)
summary(art_re_treatment_three_way)
confint(art_re_treatment_three_way)

anova(art_treatment_accuracy, art_re_treatment_three_way)

art_re_orthogonal_three_way_lm <-
    lm(accuracy ~ easy_hard * art_z * re_z + easy * art_z * re_z + hard * art_z * re_z,
       data = accuracy)
summary(art_re_orthogonal_three_way_lm)
confint(art_re_orthogonal_three_way_lm)

art_re_treatment_three_way_lm <-
    lm(accuracy ~ condition * art_z + condition * re_z,
       data = accuracy)
summary(art_re_treatment_three_way_lm)

re_treatment_lm <- lm(accuracy ~ condition * re_z,
                      data = accuracy)
summary(re_treatment_lm)




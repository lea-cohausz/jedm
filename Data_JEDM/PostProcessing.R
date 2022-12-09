
library(readr)
library(dplyr)
library(stringr)

data <- read_csv("/Exp.csv")

colnames(data) <- c("ID", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10")

data$F1 <- gsub("[()]", "", data$F1)
data$F2 <- gsub("[()]", "", data$F2)
data$F3 <- gsub("[()]", "", data$F3)
data$F4 <- gsub("[()]", "", data$F4)
data$F5 <- gsub("[()]", "", data$F5)
data$F6 <- gsub("[()]", "", data$F6)
data$F7 <- gsub("[()]", "", data$F7)
data$F8 <- gsub("[()]", "", data$F8)
data$F9 <- gsub("[()]", "", data$F9)
data$F10 <- gsub("[()]", "", data$F10)

data$F1 <- gsub("['']", "", data$F1)
data$F2 <- gsub("['']", "", data$F2)
data$F3 <- gsub("['']", "", data$F3)
data$F4 <- gsub("['']", "", data$F4)
data$F5 <- gsub("['']", "", data$F5)
data$F6 <- gsub("['']", "", data$F6)
data$F7 <- gsub("['']", "", data$F7)
data$F8 <- gsub("['']", "", data$F8)
data$F9 <- gsub("['']", "", data$F9)
data$F10 <- gsub("['']", "", data$F10)



data$keep <- NA
data$keep[str_sub(data$F1, start = -1) == 1] <- 1
data$keep[str_sub(data$F1, start = -1) == 0] <- 0

data <- data %>% filter(keep == 1)

data$F1 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F1)
data$F2 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F2)
data$F3 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F3)
data$F4 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F4)
data$F5 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F5)
data$F6 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F6)
data$F7 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F7)
data$F8 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F8)
data$F9 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F9)
data$F10 <- gsub("^([^,]*,[^,]*),.*", "\\1", data$F10)

library(tidyr)
data <- separate(data = data, sep = ",", col = F1, into = c("FeatureF1", "EffectF1"))
data <- separate(data = data, sep = ",", col = F2, into = c("FeatureF2", "EffectF2"))
data <- separate(data = data, sep = ",", col = F3, into = c("FeatureF3", "EffectF3"))
data <- separate(data = data, sep = ",", col = F4, into = c("FeatureF4", "EffectF4"))
data <- separate(data = data, sep = ",", col = F5, into = c("FeatureF5", "EffectF5"))
data <- separate(data = data, sep = ",", col = F6, into = c("FeatureF6", "EffectF6"))
data <- separate(data = data, sep = ",", col = F7, into = c("FeatureF7", "EffectF7"))
data <- separate(data = data, sep = ",", col = F8, into = c("FeatureF8", "EffectF8"))
data <- separate(data = data, sep = ",", col = F9, into = c("FeatureF9", "EffectF9"))
data <- separate(data = data, sep = ",", col = F10, into = c("FeatureF10", "EffectF10"))

data$FeatureF1 <- gsub( " .*$", "", data$FeatureF1)


data$F1[str_detect(data$FeatureF1, "parC4", negate = FALSE) == TRUE] <- "parC4"
data$F1[str_detect(data$FeatureF1, "parC3", negate = FALSE) == TRUE] <- "parC3"
data$F2[str_detect(data$FeatureF2, "parC4", negate = FALSE) == TRUE] <- "parC4"
data$F2[str_detect(data$FeatureF2, "parC3", negate = FALSE) == TRUE] <- "parC3"
data$F2[str_detect(data$FeatureF2, "parC2", negate = FALSE) == TRUE] <- "parC2"
data$F3[str_detect(data$FeatureF3, "parC4", negate = FALSE) == TRUE] <- "parC4"
data$F3[str_detect(data$FeatureF3, "parC3", negate = FALSE) == TRUE] <- "parC3"
data$F3[str_detect(data$FeatureF3, "parC2", negate = FALSE) == TRUE] <- "parC2"
data$F4[str_detect(data$FeatureF4, "befC4", negate = FALSE) == TRUE] <- "befC4"
data$F4[str_detect(data$FeatureF4, "befC2", negate = FALSE) == TRUE] <- "befC2"
data$F4[str_detect(data$FeatureF4, "PrevFail", negate = FALSE) == TRUE] <- "PrevFail"
data$F4[str_detect(data$FeatureF4, "befFC5", negate = FALSE) == TRUE] <- "befFC5"


data$F5[str_detect(data$FeatureF5, "befC2", negate = FALSE) == TRUE] <- "befC2"
data$F5[str_detect(data$FeatureF5, "befFC6", negate = FALSE) == TRUE] <- "befFC6"
data$F5[str_detect(data$FeatureF5, "befC4", negate = FALSE) == TRUE] <- "befC4"
data$F5[str_detect(data$FeatureF5, "PrevFail", negate = FALSE) == TRUE] <- "PrevFail"
data$F5[str_detect(data$FeatureF5, "PrevDO", negate = FALSE) == TRUE] <- "PrevDO"
data$F5[str_detect(data$FeatureF5, "befDOC6", negate = FALSE) == TRUE] <- "befDOC6"
data$F5[str_detect(data$FeatureF5, "befFC5", negate = FALSE) == TRUE] <- "befFC5"
data$F5[str_detect(data$FeatureF5, "befDOC5", negate = FALSE) == TRUE] <- "befDOC5"
data$F5[str_detect(data$FeatureF5, "befC3", negate = FALSE) == TRUE] <- "befC3"

data$F6[str_detect(data$FeatureF6, "befC2", negate = FALSE) == TRUE] <- "befC2"#
data$F6[str_detect(data$FeatureF6, "befFC6", negate = FALSE) == TRUE] <- "befFC6"#
data$F6[str_detect(data$FeatureF6, "befC4", negate = FALSE) == TRUE] <- "befC4"#
data$F6[str_detect(data$FeatureF6, "PrevFail", negate = FALSE) == TRUE] <- "PrevFail"#
data$F6[str_detect(data$FeatureF6, "PrevDO", negate = FALSE) == TRUE] <- "PrevDO"#
data$F6[str_detect(data$FeatureF6, "befDOC6", negate = FALSE) == TRUE] <- "befDOC6"#
data$F6[str_detect(data$FeatureF6, "befFC5", negate = FALSE) == TRUE] <- "befFC5"#
data$F6[str_detect(data$FeatureF6, "befDOC5", negate = FALSE) == TRUE] <- "befDOC5"#
data$F6[str_detect(data$FeatureF6, "befC3", negate = FALSE) == TRUE] <- "befC3"#

data$F7[str_detect(data$FeatureF7, "befC2", negate = FALSE) == TRUE] <- "befC2"#
data$F7[str_detect(data$FeatureF7, "befFC6", negate = FALSE) == TRUE] <- "befFC6"#
data$F7[str_detect(data$FeatureF7, "befC4", negate = FALSE) == TRUE] <- "befC4"
data$F7[str_detect(data$FeatureF7, "PrevFail", negate = FALSE) == TRUE] <- "PrevFail"#
data$F7[str_detect(data$FeatureF7, "PrevDO", negate = FALSE) == TRUE] <- "PrevDO"#
data$F7[str_detect(data$FeatureF7, "befDOC6", negate = FALSE) == TRUE] <- "befDOC6"#
data$F7[str_detect(data$FeatureF7, "befFC5", negate = FALSE) == TRUE] <- "befFC5"#
data$F7[str_detect(data$FeatureF7, "befDOC5", negate = FALSE) == TRUE] <- "befDOC5"#
data$F7[str_detect(data$FeatureF7, "befC3", negate = FALSE) == TRUE] <- "befC3"#

data$F8[str_detect(data$FeatureF8, "befC2", negate = FALSE) == TRUE] <- "befC2"#
data$F8[str_detect(data$FeatureF8, "befFC6", negate = FALSE) == TRUE] <- "befFC6"#
data$F8[str_detect(data$FeatureF8, "befC4", negate = FALSE) == TRUE] <- "befC4"#
data$F8[str_detect(data$FeatureF8, "PrevFail", negate = FALSE) == TRUE] <- "PrevFail"#
data$F8[str_detect(data$FeatureF8, "PrevDO", negate = FALSE) == TRUE] <- "PrevDO"#
data$F8[str_detect(data$FeatureF8, "befDOC6", negate = FALSE) == TRUE] <- "befDOC6"#
data$F8[str_detect(data$FeatureF8, "befFC5", negate = FALSE) == TRUE] <- "befFC5"#
data$F8[str_detect(data$FeatureF8, "befDOC5", negate = FALSE) == TRUE] <- "befDOC5"#
data$F8[str_detect(data$FeatureF8, "befC3", negate = FALSE) == TRUE] <- "befC3"#

data$F9[str_detect(data$FeatureF9, "befC2", negate = FALSE) == TRUE] <- "befC2"#
data$F9[str_detect(data$FeatureF9, "befFC6", negate = FALSE) == TRUE] <- "befFC6"#
data$F9[str_detect(data$FeatureF9, "befC4", negate = FALSE) == TRUE] <- "befC4"
data$F9[str_detect(data$FeatureF9, "PrevFail", negate = FALSE) == TRUE] <- "PrevFail"#
data$F9[str_detect(data$FeatureF9, "PrevDO", negate = FALSE) == TRUE] <- "PrevDO"#
data$F9[str_detect(data$FeatureF9, "befDOC6", negate = FALSE) == TRUE] <- "befDOC6"#
data$F9[str_detect(data$FeatureF9, "befFC5", negate = FALSE) == TRUE] <- "befFC5"#
data$F9[str_detect(data$FeatureF9, "befDOC5", negate = FALSE) == TRUE] <- "befDOC5"#
data$F9[str_detect(data$FeatureF9, "befC3", negate = FALSE) == TRUE] <- "befC3"#

data$F10[str_detect(data$FeatureF10, "befC2", negate = FALSE) == TRUE] <- "befC2"#
data$F10[str_detect(data$FeatureF10, "befFC6", negate = FALSE) == TRUE] <- "befFC6"#
data$F10[str_detect(data$FeatureF10, "befC6", negate = FALSE) == TRUE] <- "befC6"
data$F10[str_detect(data$FeatureF10, "PrevFail", negate = FALSE) == TRUE] <- "PrevFail"#
data$F10[str_detect(data$FeatureF10, "PrevDO", negate = FALSE) == TRUE] <- "PrevDO"#
data$F10[str_detect(data$FeatureF10, "befDOC6", negate = FALSE) == TRUE] <- "befDOC6"#
data$F10[str_detect(data$FeatureF10, "befFC5", negate = FALSE) == TRUE] <- "befFC5"#
data$F10[str_detect(data$FeatureF10, "befDOC5", negate = FALSE) == TRUE] <- "befDOC5"#
data$F10[str_detect(data$FeatureF10, "befC3", negate = FALSE) == TRUE] <- "befC3"#

data$EffectF1 <- as.numeric(data$EffectF1)
data$EffectF2 <- as.numeric(data$EffectF2)
data$EffectF3 <- as.numeric(data$EffectF3)
data$EffectF4 <- as.numeric(data$EffectF4)
data$EffectF5 <- as.numeric(data$EffectF5)
data$EffectF6 <- as.numeric(data$EffectF6)
data$EffectF7 <- as.numeric(data$EffectF7)
data$EffectF8 <- as.numeric(data$EffectF8)
data$EffectF9 <- as.numeric(data$EffectF9)
data$EffectF10 <- as.numeric(data$EffectF10)

data$F1[data$EffectF1 < 0] <- "None"
data$F2[data$EffectF2 < 0] <- "None"
data$F3[data$EffectF3 < 0] <- "None"
data$F4[data$EffectF4 < 0] <- "None"
data$F5[data$EffectF5 < 0] <- "None"
data$F6[data$EffectF6 < 0] <- "None"
data$F7[data$EffectF7 < 0] <- "None"
data$F8[data$EffectF8 < 0] <- "None"
data$F9[data$EffectF9 < 0] <- "None"
data$F10[data$EffectF10 < 0] <- "None"

data2 <- data %>%
  select(ID, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10)

write_csv(data2, "/Step2_InputData.csv")








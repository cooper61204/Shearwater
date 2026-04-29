df <- data.frame(
  effort = c(5, 8, 0, 12),
  gear = c("longline", "trawl", "purse seine", "longline"),
  stringsAsFactors = FALSE
)

out <- standardize_fishing_effort(df) #function takes in df as input

standardize_fishing_effort(5)
standardize_fishing_effort("abc") #function errors if not give df as input
standardize_fishing_effort(list(effort = 1, gear = "longline"))

df_missing_effort <- data.frame(
  gear = c("longline", "trawl"),
  stringsAsFactors = FALSE
)
standardize_fishing_effort(df_missing_effort) #function errors if effort column is missing

df_missing_gear <- data.frame(
  effort = c(1, 2),
  stringsAsFactors = FALSE
)
standardize_fishing_effort(df_missing_gear) #make sure function errors if gear column is missing

df_string_effort <- data.frame(
  effort = c("5", "8", "0", "12"),
  gear = c("longline", "trawl", "purse seine", "longline"),
  stringsAsFactors = FALSE
)
#test to check that function can convert column to numeric
out_convert_to_numeric <- standardize_fishing_effort(df_string_effort)

df_non_numeric_effort <- data.frame(
  effort = c("5", "bad", "7"),
  gear = c("longline", "trawl", "trawl"),
  stringsAsFactors = FALSE
)
#if a string such as "bad" is in effort column function will convert to 0
out_convert_non_numeric <-  standardize_fishing_effort(df_non_numeric_effort)
out_convert_non_numeric

df_effort_missing_values <- data.frame(
  effort = c(5, NA, 7),
  gear = c("longline", "trawl", "trawl"),
  stringsAsFactors = FALSE
)
#convert missing values to 0
effort_missing_values <- standardize_fishing_effort(df_effort_missing_values)
effort_missing_values

df_neg_effort <- data.frame(
  effort = c(5, -3, 7),
  gear = c("longline", "trawl", "trawl"),
  stringsAsFactors = FALSE
)
#negative effort values become 0
effort_neg_value <- standardize_fishing_effort(df_neg_effort)
effort_neg_value

df_gear_lower <- data.frame(
  effort = c(1, 2, 3),
  gear = c("LongLine", "TRAWL", "Purse Seine"),
  stringsAsFactors = FALSE
)
"convert capital letters in the gear col to lowercase"
gear_lower <- standardize_fishing_effort(df_gear_lower)
gear_lower

df_gear_trimmed <- data.frame(
  effort = c(1, 2, 3),
  gear = c(" longline ", "trawl  ", "  purse seine"),
  stringsAsFactors = FALSE
)
# successful dropped rows which have a space removed
gear_trimmed <- standardize_fishing_effort(df_gear_trimmed)
gear_trimmed

df_gear <- data.frame(
  effort = c(1, 2, 3),
  gear = c("drifting longline", "trawl", "purse seine"),
  stringsAsFactors = FALSE
)

gear_map <- c("drifting longline" = "longline")
# function can correctly take in gear map and change name of gears in map and leave the others unchanged
gear_test <- standardize_fishing_effort(df_gear, gear_map = gear_map)
gear_test

df_log <- data.frame(
  effort = c(1, 2, 3),
  gear = c("drifting longline", "trawl", "purse seine"),
  stringsAsFactors = FALSE
)
#function can apply log transformations to effort column
log_test <- standardize_fishing_effort(df_log, log_transform = TRUE)
log_test

df_std <- data.frame(
  effort = c(1, 2, 3, 4),
  gear = c("a", "a", "b", "b"),
  stringsAsFactors = FALSE
)
#standardize the effort col to mean 0 and sd 1
std_test <- standardize_fishing_effort(df_std, standardize_effort = TRUE)
std_test

df_ind_effort <- data.frame(
  effort = c(5, 5, 5, 5),
  gear = c("a", "a", "b", "b"),
  stringsAsFactors = FALSE
)
#correctly standardizing to 0 when all values identica
ind_effort_test <- standardize_fishing_effort(df_ind_effort)
ind_effort_test

df_all_na <- data.frame(
  effort = c(NA, NA, NA),
  gear = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
#correctly standrizes to 0 when all effort values are NA
test_all_na <- standardize_fishing_effort(df_all_na)
test_all_na

df_extra <- data.frame(
  longitude = c(1, 2, 3),
  latitude = c(4, 5, 6),
  effort = c(2, 4, 6),
  gear = c("longline", "trawl", "trawl"),
  stringsAsFactors = FALSE
)
#function correctly handles extra columns besides gear and effort
extra_test <- standardize_fishing_effort(df_extra)
extra_test

df_custom <- data.frame(
  fishing_hours = c(3, 6, 9),
  gear_type = c("Longline", "TRAWL", "TRAWL"),
  stringsAsFactors = FALSE
)

#Function can corretly handle gear and effort cols which are named something different
custom_test <- standardize_fishing_effort(
  df_custom,
  effort_col = "fishing_hours",
  gear_col = "gear_type"
)
custom_test

df_empty <- data.frame(
  effort = numeric(0),
  gear = character(0),
  stringsAsFactors = FALSE
)

empty_tes <- standardize_fishing_effort(df)
empty_tes

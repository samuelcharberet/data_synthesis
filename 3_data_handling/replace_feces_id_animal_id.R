################################ NETSTO PROJECT ################################

# This script replaces faeces and animal group ID wrongfully filled
# It was executed one time
# It is not intended to excectuing it anymore
# Authors : Samuel Charberet
# date : 28/06/2024

library(dplyr)

# Loading the ICP data
data_me = read.csv(
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020.csv"
  )
)

# The animal group ids and faeces ids are correct in the CHN data
rows_correct_ids = which(data_me$component_name %in% "C")


# The animal group ids and faeces ids are incorrect in the ICP data
rows_incorrect_ids = which(data_me$component_name %in% c("P", "Mg", "K"))

# The sample ID helps us reattribute the right animal group ids and faeces ids to the ICP data

for (i in rows_incorrect_ids) {
  id_to_look_for = data_me[i, ]$comments # The ID appears in the 'comment' colummn
  if (any(data_me[rows_correct_ids, ]$comments == id_to_look_for)) {
    # If the sample ID exist somewhere else in the data, we use it to rightfully fill in
    correct_row = which(data_me$comments == id_to_look_for)[1]
    data_me[i, ]$animal_group_ID = data_me[correct_row, ]$animal_group_ID
    data_me[i, ]$feces_ID = data_me[correct_row, ]$feces_ID
  }
  else {
    #IF it does not exist, we create one
    data_me[i, ]$animal_group_ID = max(data_me$animal_group_ID) + 1
    data_me[i, ]$feces_ID = max(data_me$feces_ID) + 1
  }
  #The list of correct ids is updated accordingly
  rows_correct_ids = c(rows_correct_ids, i)
}

# I write an updated file
write.csv(
  data_me,
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020_corrected_ids.csv"
  ),
  row.names = F
)

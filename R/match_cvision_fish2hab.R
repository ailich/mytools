#' Matches fish recorded in C-Vision to closest habitat frame
#'
#' Matches fish recorded in C-Vision to closest habitat frame. Returns a tibble of species by site counts.
#' @param hab_totalframe vector of total frame number of habitat observation
#' @param merged_cvision_csv output of merge_cvision_csv
#' @param include_framenum whether or not to include frame number in output table
#' @import dplyr
#' @import magrittr
#' @export
match_cvision_fish2hab<-function(hab_totalframe, merged_cvision_csv, include_framenum=FALSE){
  fish_names<- sort_species(unique(merged_cvision_csv$Fish_Type))
  new_df<- matrix(nrow = length(hab_totalframe), ncol=length(fish_names)+1) %>% as_tibble() #Species by Site Matrix
  names(new_df)<- c("Total_Frame", fish_names)
  new_df<- sapply(X = new_df, as.integer) %>% as_tibble
  new_df$Total_Frame<- hab_totalframe
  merged_cvision_csv$closest_hab_frame<- NA_integer_
  for (i in 1:nrow(merged_cvision_csv)) {
    diffs<- abs(merged_cvision_csv$Total_Frame[i]-hab_totalframe)
    idx<- which(diffs==min(diffs))[1]
    merged_cvision_csv$closest_hab_frame[i]<-hab_totalframe[idx]
  }
  grouped_counts<- merged_cvision_csv %>% group_by(Fish_Type, closest_hab_frame) %>% count() %>% ungroup() #Group counts by Fish Type and nearest habitat frame
  for (i in 1:nrow(grouped_counts)) { #Fill in the species by site matrix
    row_num<- which(new_df$Total_Frame==grouped_counts$closest_hab_frame[i])
    col_name<- grouped_counts$Fish_Type[i]
    fish_count<- grouped_counts$n[i]
    new_df[row_num, col_name]<- fish_count
  }
  new_df[is.na(new_df)]<- 0
  if (include_framenum==FALSE){new_df<- new_df %>% select(-c(Total_Frame))}
  return(new_df)
  }

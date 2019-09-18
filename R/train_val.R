#' Split Habitat Data into training and Validation Data
#'
#' Splits data into training and validation samples. Uses stratified random sampling without replacement. Will randomly draw the specified proportion without replacement from each class to generate validation data. Training set is the original data with the observations from validation data removed. Returns a list containing training data as the first element and validation data as the second element. The list elements will be the same data type as the input of my_data
#' @param my_data a spatialpoints dataframe containing, dataframe, or tibble containing all data
#' @param hab_col number specifying the column to check
#' @param prop_val proportion of samples to be used in validation
#' @param seed sampling is initiated by set.seed(seed) if input is not NULL. This will generate consistent results
#' @import dplyr
#' @import magrittr
#' @import tibble
#' @export

train_val<- function(my_data, hab_col, prop_val, seed=NULL){
  isadataframe<- is.data.frame(my_data) & !is_tibble(my_data)
  isatibble<- is_tibble(my_data)
  isaspdf<- (class(my_data)=="SpatialPointsDataFrame")[1]
  if (isaspdf){prj4_str<- my_data@proj4string} #Save prj4 string
  if (isadataframe|isaspdf){my_data<- as_tibble(my_data)} #Convert to tibble
  og_name<-names(my_data)[hab_col] #store for later
  names(my_data)[hab_col]<-"hab_type"
  my_data<- my_data %>% mutate(Row_ID=1:NROW(my_data)) #Primary key for all tables
  if (!is.null(seed)){set.seed(seed)}
  val<- my_data %>% group_by(hab_type)%>% sample_frac(size=prop_val,replace = FALSE) %>% ungroup() #Create Validation Set
  training<- my_data[-val$Row_ID,] #Put data not in validation set in training set
  names(val)[hab_col]<- og_name #reset name to original
  names(training)[hab_col]<- og_name
  val<- val %>% select(-c(Row_ID)) #Remove Row ID's
  training<- training %>% select(-c(Row_ID))
  if (isadataframe){
    val<- as.data.frame(val)
    training<- as.data.frame(training)
  } #Convert to original data type (dataframe)
  if (isaspdf){
    val<- SpatialPointsDataFrame(coords = cbind(val$coords.x1, val$coords.x2), data = as.data.frame(select(val, -c(coords.x1,coords.x2))), proj4string =prj4_str)
    training<- SpatialPointsDataFrame(coords = cbind(training$coords.x1, training$coords.x2), data = as.data.frame(select(training, -c(coords.x1,coords.x2))), proj4string =prj4_str)
  } #Convert to original data type (SpatialPoints Dataframe)
  output<- list(training,val) #Combine as list
  names(output)<- c("training", "validation")
  return(output)
  }

#' Generates diagnostic table assessing drift of HD CBASS cameras
#'
#' Generates diagnostic table assessing drift of HD CBASS cameras.
#' Outputs:
#' Transect - Name of Transect
#' st_time - Start time of transect
#' end_time- End time of transect
#' Vid_drift - Estimated drift between video and real time in seconds (calculated as video duration - transect duration. Negative indicates that the video is shorter than the true time of the transect.)
#' fps_mosaic - fps at which video was mosaiced
#' fps_rec -  fps at which frames were recorded
#' i & j - counters for loops (useful for debugging)
#' table_issue - Lets you know if there were any issues detected with the camera table
#' n_frames_diff - Number of frames recorded in table minus number of images in picture directory
#' correction_factor - If substantial vid_drift you can multiply time in video by this to get real time into the transect
#' transect_dur_mins - Duration of transect in minutes
#' vid_dur_mins - Duration of videos in minutes
#' n_frames - number of frames in video (images of size zero at the end of transect are not counted as frames)
#' max_frame_diff - The value with the maximum magnitude of time difference between two subsequent frames in seconds
#' table_path - path where camera table was read from
#' @param CBASS_dir directory of CBASS files
#' @param camera_folder_name name of camera folder (e.g. "blackfly," "colorfly," "avt")
#' @param table_name name of camera tables (e.g. "blackfly.tsv")
#' @param vid_length length of video files in minutes
#' @param has_colnames Logical specifying whether camera tables hav column names
#' @param fixed_tables_dir Dataframe of fixed tables with first column as transect directory and second column as the name of the sub_directory (Use if some tables broken and you want to rerun with these tables)
#' @param fixed_tables name of fixed tables (e.g "blackfly_fixed.tsv")
#' @param max_diff_thresh Threshold in seconds for magnitude of difference in seconds between subsequent frames to replace correction factor for Vid_drift with NA
#' @param Media_Info_Path Path to Media Info program
#' @import dplyr
#' @import readr
#' @import magrittr
#' @import stringr
#' @import lubridate
#' @export
#'
check_camera_drift<- function(CBASS_dir, camera_folder_name= "blackfly", table_name= "", vid_length=1, has_colnames= TRUE, fixed_tables_dir= NULL,fixed_tables=NULL, max_diff_thresh = 2, Media_Info_Path = "C:/Users/socce/Documents/Grad School/Software/Media_Info/MediaInfo.exe"){

  warn_behavior<- getOption("warn")
  options(warn=1) #Print warnings as they happen

  if (!is.null(fixed_tables)){
    names(fixed_tables)<- c(transect_dir, sub_dirs)
    fixed_tables<- fixed_tables %>% mutate(full_dir = paste(transect_dir, sub_dirs, sep="_"))}

  output<- tibble(Transect=NA_character_, st_time= NA, end_time = NA, Vid_drift=NA_real_, fps_mosaic = NA_real_, fps_rec = NA_real_, i=NA_integer_, j= NA_integer_, table_issue = NA_character_, n_frames_diff = NA_integer_, correction_factor=NA_real_,transect_dur_mins = NA_real_, vid_dur_mins = NA_real_, n_frames=NA_integer_, max_frame_diff= NA_real_, table_path = NA_character_)[0,]

  CBASS_transects<- list.files(CBASS_dir) #Get all CBASS Transect Names from Cruise
  CBASS_transects<- CBASS_transects[!grepl(pattern = "\\.", CBASS_transects)] #Removes non transect files (e.g. pdf files) from transect list
  CBASS_transects<- CBASS_transects[!grepl(pattern = "Sensor Excel", CBASS_transects)] #Remove Sensor Excel File from transect list

  for (i in 1:length(CBASS_transects)) {
    transect_dir<- paste(CBASS_dir, CBASS_transects[i], sep="/")
    print(paste0("Begin ", transect_dir, " (",i,"/",as.character(length(CBASS_transects)),")"))
    check_val<- sum(grepl(pattern = camera_folder_name, list.files(transect_dir))) > 0 & sum(grepl(pattern = "tables", list.files(transect_dir))) > 0
    if(check_val==FALSE){ #Check necessary files exist
      warning(paste(transect_dir, "does not have necessary directories"))
      next}
    list.files(transect_dir)
    sub_dirs<- list.files(paste(transect_dir, camera_folder_name, sep="/"))
    sub_dirs<- sub_dirs[!grepl(pattern = "\\.", sub_dirs)] #Remove stray files

    if(length(sub_dirs)==0){sub_dirs=""} #Deal with no subdirs
    sub_dirs
    for (j in 1:length(sub_dirs)) { #Loop through subdirectories
      print(paste("    ", "subdirectory", sub_dirs[j]))
      vid_files<- sort(list.files(path = paste(transect_dir, camera_folder_name, sub_dirs[j], sep="/"), pattern = "\\.avi$", full.names = TRUE))
      if(length(vid_files)==0){
        warning("no videos in dir")
        next}

      pic_files<- sort(list.files(path = paste(transect_dir, camera_folder_name, sub_dirs[j], sep="/"), pattern = "\\.jpg$", full.names = TRUE))

      if(paste(basename(transect_dir), sub_dirs[j], sep="_") %in% fixed_tables$full_dir){
        table_dir<- paste(fixed_tables_dir, basename(transect_dir), sub_dirs[j], sep="/")
        curr_table_name<- fixed_tables$table_name[which(fixed_tables$full_dir==paste(basename(transect_dir), sub_dirs[j], sep="_"))]
      } else {
        table_dir<- paste(transect_dir, "tables", sub_dirs[j], sep="/")
        curr_table_name<- table_name}
      curr_table_name
      if ((!file.exists(table_dir)) & (length(sub_dirs)==1)){
        table_dir<- paste(transect_dir, "tables", sep="/")
      } #Account for if no subdir in table folder (Feb 2016)
      if(sum(grepl(pattern= curr_table_name, x = list.files(table_dir)))==0){
        warning(paste("no", curr_table_name))
        next}
      pic_table<- read_tsv(paste(table_dir, curr_table_name, sep="/"), col_names =  has_colnames, col_types = "Tdc")
      if(nrow(pic_table)==0){
        warning(paste(curr_table_name, "is empty"))
        next}
      if (!has_colnames){
        names(pic_table)<- c("timestamp", "u_second",  "file_path")
      }

      table_issue<- check_camera_table(pic_table, display_warning = FALSE)
      table_issue
      if ((table_issue =="reset") | (table_issue =="pattern")){
        warning(paste(curr_table_name, "is", table_issue))
        new_row<- tibble(Transect=paste0(CBASS_transects[i], "_", sub_dirs[j]), st_time= NA, end_time = NA, Vid_drift=NA_real_, fps_mosaic = NA_real_, fps_rec = NA_real_, i=i, j=j, table_issue=table_issue, n_frames_diff= NA_integer_, correction_factor=NA_real_, transect_dur_mins = NA_real_, vid_dur_mins = NA_real_, n_frames =NA_integer_, max_frame_diff= NA_real_, table_path = paste(table_dir, curr_table_name, sep="/"))
        output<- bind_rows(output, new_row)
        next
      }
      if (table_issue =="jumbled"){warning(paste(curr_table_name, "is", table_issue))}

      my_pattern<- detect_cam_pattern(pic_table$file_path)
      my_pattern
      pic_table2<- pic_table %>% filter(grepl(pattern = my_pattern, x = file_path))
      pic_table2<- pic_table2 %>% mutate(exact_time = timestamp+ microseconds(u_second))
      pic_table2<- pic_table2 %>% mutate(diff=NA_real_)
      pic_table2<- pic_table2 %>% arrange(file_path)
      pic_table2<- pic_table2 %>% mutate(lag_time= lag(exact_time, n = 1)) %>% mutate(diff=as.numeric(difftime(exact_time, lag_time, units = "secs")))
      max_frame_diff<- pic_table2$diff[which.max(abs(pic_table2$diff))]

      n_frames2<- length(pic_files)
      n_frames<- sum(grepl(pattern = my_pattern, pic_table$file_path))
      n_frames_diff<- n_frames-n_frames2
      n_frames_diff
      if(n_frames_diff!=0){
        warning("n_frames in table is not equal to n_frames in pic dir")
        new_row<- tibble(Transect=paste0(CBASS_transects[i], "_", sub_dirs[j]), st_time= NA, end_time = NA, Vid_drift=NA_real_, fps_mosaic = NA_real_, fps_rec = NA_real_, i=i, j=j, table_issue=table_issue, n_frames_diff= n_frames_diff, correction_factor=NA_real_, transect_dur_mins = NA_real_, vid_dur_mins = NA_real_, n_frames=NA_integer_, max_frame_diff= max_frame_diff, table_path = paste(table_dir, curr_table_name, sep="/"))
        output<- bind_rows(output, new_row)
        next
      }

      k<- length(pic_files)+1
      pic_size<-0
      while (pic_size==0) {
        k<- k-1
        pic_info<- file.info(pic_files[k])
        pic_size<- pic_info$size
      } #See if images at end are of zero size
      pic_files<- pic_files[1:k] #Remove images of zero size
      pic_table<- pic_table[1:(which(basename(tail(pic_files,1)) ==  basename(pic_table$file_path))),] #Remove corresponding images from table

      n_frames_adj<- sum(grepl(pattern = my_pattern, pic_table$file_path)) #Update n_frames after removing those of zero size

      st_idx<- min(which(grepl(pattern = my_pattern, x = pic_table$file_path)))
      end_idx<- max(which(grepl(pattern = my_pattern, x = pic_table$file_path)))

      if(!str_extract(string = pic_table$file_path[end_idx], pattern = my_pattern)== str_extract(string = pic_files[length(pic_files)], pattern = my_pattern)) {#Check that last frame in picture folder matches with table
        warning("last_pic in table is not the same as last in picture dir")
        new_row<- tibble(Transect=paste0(CBASS_transects[i], "_", sub_dirs[j]), st_time= NA, end_time = NA, Vid_drift=NA_real_, fps_mosaic = NA_real_, fps_rec = NA_real_, i=i, j=j, table_issue=table_issue, n_frames_diff= n_frames_diff, correction_factor=NA_real_,transect_dur_mins = NA_real_, vid_dur_mins = NA_real_, n_frames=NA_integer_, max_frame_diff= max_frame_diff, table_path = paste(table_dir, curr_table_name, sep="/"))
        output<- bind_rows(output, new_row)
        next}

      st_time<-  pic_table$timestamp[st_idx] + microseconds(pic_table$u_second[st_idx])
      end_time_real<-  pic_table$timestamp[end_idx] + microseconds(pic_table$u_second[end_idx])
      transect_dur_secs<- as.numeric(difftime(end_time_real,st_time, units = "secs"))

      last_vid_path<- vid_files[length(vid_files)] #path of last video
      last_vid_num<- last_vid_path %>%
        basename() %>%
        str_extract_all("\\d+", simplify = TRUE) %>%
        as.vector() %>%
        tail(1) %>%
        as.numeric()

      last_vid_info<- video_info(vid_path = last_vid_path, Media_Info_Path = Media_Info_Path)

      fps_mosaic<- n_frames_adj/((last_vid_num*vid_length*60)+ parse_video_time(last_vid_info))
      fps_rec<- n_frames_adj/(transect_dur_secs)

      vid_dur_secs<- (last_vid_num*vid_length*60) + parse_video_time(last_vid_info)

      correction_factor<- transect_dur_secs/vid_dur_secs
      Vid_drift<- vid_dur_secs - transect_dur_secs

      new_row<- tibble(Transect=paste0(CBASS_transects[i], "_", sub_dirs[j]), st_time= st_time, end_time = end_time_real, Vid_drift = Vid_drift, fps_mosaic = fps_mosaic, fps_rec = fps_rec, i=i, j=j, table_issue=table_issue, n_frames_diff= n_frames_diff, correction_factor=correction_factor, transect_dur_mins = transect_dur_secs/60, vid_dur_mins = vid_dur_secs/60, n_frames=n_frames_adj, max_frame_diff= max_frame_diff, table_path = paste(table_dir, curr_table_name, sep="/"))
      output<- bind_rows(output, new_row)
    }
  }
  output$correction_factor[abs(output$max_frame_diff) > max_diff_thresh]<- NA_real_ #Remove correction factor id max_frame_diff is greater than threshold as substantial part of calculated Vid_drift is probably due to clock resynching
  options(warn=warn_behavior)
  return(output)
}

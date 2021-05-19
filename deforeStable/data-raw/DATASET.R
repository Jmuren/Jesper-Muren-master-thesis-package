## code to prepare datasets goes here

library(deforeStable)

# dir = "C:\\Users\\Jesper\\Desktop\\Jesper\\Master\\EDA\\Data_Google_Plugin\\"
dir = "/home/d/Codes/deforestation/automatic-deforestation-detector/Data_Google_Plugin/"

#### Composing the data ####
Forest_list <- setdiff(list.files(path=dir, pattern = "[fF]orest"), list.files(path=dir, pattern = "de[fF]orest"))

City_list <- c(list.files(path = dir, pattern = "Goteborg"), list.files(path = dir, pattern = "Malmo"),
               list.files(path = dir, pattern = "Stockholm"), list.files(path = dir, pattern = "Halmstad"),
               list.files(path = dir, pattern = "Helsingborg"), list.files(path = dir, pattern = "Vasteras"))[-c(5:6)]

Farm_list <- c(list.files(path=dir, pattern = "[Ff]arm"))


#Combine image names into matrix with Forest and Not Forest labels
geoimages_desc <- rbind(cbind(Im = Forest_list, label =rep("Forest", length(Forest_list))),
                 cbind(Im = City_list, label = rep("Not Forest", length(City_list))),
                 cbind(Im = Farm_list, label = rep("Not Forest", length(Farm_list))))

#Forest_data <- plyr::llply(Forest_list, read_data_raster, dir = dir)


# saving data into the packages

geoimages <- lapply(X=as.vector(geoimages_desc[,'Im']), FUN=read_data_raster, dir=dir)

usethis::use_data(geoimages, compress='xz', overwrite = TRUE)
usethis::use_data(geoimages_desc, overwrite = TRUE)

# ................................................................................................#
# ......................................MSWEP.....................................................#
#.............................. Separation by Coordinates ........................................#
# ................................................................................................#


# Introduction

# NetCDF is a widely used format for exchanging or distributing climate data. they contain metada-#
# ta that describes what is contained in a file. MSWEP (Multi-Source Weighted-Ensemble Precipita- #
# tion) is a fully global historical precipitation dataset (1979-2019) (http://www.gloh2o.org/).  #
# the file is monthly peparated and have data all the globe by 0.1 degree of resolution. 

# This data is available in netCDF file and monthly separeted data and was dowloaded from 
# http://hydrology.princeton.edu/.
# Sometimes it is necessary to "slice" the data content to handle with this big data. It is commom #
# to slice the data by coordinates and this Code was done to this propose.

# 1 - Libraries
library(ncdf4)
library(RNetCDF)

# 2 - Input Coordinates to Croping data

# It is necessary to input the "frame" to slice the data by coordinate.
# the MSWEp data was built in WGS 1984 CRS. This folowing coordinates is the Latin america frame  #
# to the WGS 84 System.

top = 12.46
botton = -59.49
left = -92.01
right = -26.27 # Latin America
  
# 3 - Input and output  Directory
# Seting The Local Directory
data_directory = choose.dir(caption = "Choose MSWEP data directory") # MSWEP data directory
data_ex_directory = choose.dir(caption = "Choose Output directory,it must be different of MSWEP data directory")
# Output directory, must be different of data_directory.

files = list.files(data_directory) # list of MSWEP files

# 4 - Process
  for(k in 1:length(files)){
    # 4.1 directories and Files
    file_directory = paste0(data_directory,"\\",files[k]) # MSWEP sinle file directory
    file_nc = open.nc(file_directory) # open MSWEP data - It will open a NetCDF format in R
    file = read.nc(file_nc) # Read the data - It will transform CDF file in a large list
                            # it will take a long time (~ 2 min).

    # 4.2  Creating the coordinates indexes (vector position)
    LonIdx = which(file$lon>=left & file$lon<=right) # it will create the data indexes to the Longitude!
    # The data must be between the left and right longitude cordinates
    LatIdx = which(file$lat>=botton & file$lat<=top) # same way to the Latitude
    
    # 4.3 creating a new Netcdf file
    filename = paste0(data_ex_directory,"\\Latin_America_",files[k]) # New file directory
    lon1 = ncdim_def("longitude", "degrees",file$lon[LonIdx]) # x coordinates - Longitude
    # It will only extract values by the coordinates defined by the LonIdx. 
    lat2 = ncdim_def("latitude", "degrees", file$lat[LatIdx]) # y coordinates - Latitudes - same way!
    time = ncdim_def("Time","Days after 1899-12-31", file$time, unlim=TRUE) # extract all time serie
    Precip = ncdim_def("Precipitation", "mm/3h", file$precipitation[LonIdx,LatIdx,]) # extract precipitation
    # It is important to perhaps the Longitude and Latitude indexes in array and the empty space to collect 
    # all precipitation by the time[LonIdx,LatIdx,])
    var_precipitation = ncvar_def(name = "Precipitation", units = "mm/3h", 
                                  dim = list(lon1, lat2, time), missval = NA, 
                                  longname="MSWEP_data_for_latin_america")
    # The Variable creation in memory. It is a preliminary set to file creation. 
   
    nc_create(filename, list(var_precipitation))# Storage in the disc - it takes a long time too.
  }
    

# ................................................................................................#
# ......................................MSWEP.....................................................#
#.............................. Separation by Coordinates,  ......................................#
#.................................file aggregation and ...........................................#
# ............................... Selection by raster.............................................#
# ................................................................................................#

# The NetCDF file and R operations
# NetCDF is a widely used format for exchanging or distributing climate data.  NetCDF files are  #
# self-describing: they contain metadata that describes what is contained in a file. NetCDF fi-  #
# les are also machine-independent because can be transferred among servers and computers that   #
# are running different operating systems, without having to convert the files in some way.      #
# (https://pjbartlein.github.io/REarthSysSci/netCDF.html). R has the capability handle netCDF    #
# files, using the RNetCDF and ncdf4 packages.

library(ncdf4)
library(RNetCDF)

# Another Packages will be used in this article to handle mapping and time data
library(raster)
library(rgdal)
library(lubridate)

# 2. MSWEP Data
# ............................... Selection by raster.............................................#
# Multi-Source Weighted-Ensemble Precipitation (MSWEP) is a fully global historical precipitation #
# dataset (1979-2019) with a 3-hourly temporal and 0.1° spatial resolution(http://www.gloh2o.org/ #
# This data is available in netCDF file and monthly separeted data. These data was dowloaded from #
# http://hydrology.princeton.edu/. 

# The File is very large and maybe will take a lot of time for reading and executing boolean ope- #
# tors. We suggest a shorter data volume to run this code. The Data reduce by coordinate process  #
# can be checked in 
# https://github.com/felipekbernardi/MSWEP/blob/main/MSWEP_%20Data_slice_by_coordinates.R .

# So, it is necessary to load these file home directory. 

Data_directory = choose.dir(caption = "Select the Directory contained the MSWEP data") # Windown to 
# set directory in windows system.
files = list.files(Data_directory) # Create a list of file names from directory

#3. selection of MSWEP data from a grid
# The coordinates can be taken from a rastermap or shapefile. In this code is used a raster base. #

Raster_dir = choose.files(caption = "## Select the raster to clip the MSWEP data ##")
map = raster(Raster_dir)

# How MSWEP Projection is WGS-84, the raster will be converted to this projection system.
map = projectRaster(map,crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# MSWEP data spacial resolution is 0.1 degree. To cover entirely region of interese the MSWEP ex- #
# tenrion must be a little bit bigger than it. The Mswep scene will be created with an set of     #
# buffered grid space (0.2 degrees).

Map_extend = extent(map) # Extract map extention

# Extracting the Raser Limits
# Top, Botton, Left and Right
top = round(ymax(Map_extend),1)+0.2
botton = round(ymax(Map_extend),1)-0.2
left = round(xmin(Map_extend),1)-0.2
right = round(xmin(Map_extend),1)+0.2

# 4. Masking and grouping data
# The MSWEP data can be read, one by one, and can be selected the grid to mask the data and it   #
# will be separated by coordinates (long, lat). So, the output colunm names will be lon x lat    #

for(k in 1:length(files)){ # run to all files in the directory  
  file_directory = paste0(Data_directory,"\\",files[k]) # MSWEP directory
  file_nc = open.nc(file_directory) # open MSWEP data - It will open a NetCDF format in R
  file = read.nc(file_nc) # Read the data - It will transform CDF file in a large list #
  
  # Creating the coordinates indexes (vector position)

  LonIdx= which( file$longitude>=left & file$longitude<=right)
  LatIdx= which( file$latitude>=botton & file$latitude<=top)
  
  # as List
  Lon = file$longitude[LonIdx]
  Lat = file$latitude[LatIdx]
  
  # Variable
  # var_name_mswep = names(file[4])# Variable Name. Can't be a list!
  
  # Split the Mswep data with local coordinates
  Rain_3h = file$Precipitation[LonIdx, LatIdx,] # it will spend long time!
  
  # define Data dimensions
  dimensions = dim(Rain_3h) # 1 = lon, 2 = lat, 3 = Time Period
  
  # Data from the File
  if(k==1){ # It is necessary to create the Grouped file in first turn!
    # Create the temporal serie
    initial_date = as.POSIXct(as.character("01-01-1979"),format="%d-%Om-%Y", origin="1970-01-01 UTC", tz = "GMT")
    final_date = as.POSIXct(as.character("31-10-2017"),format="%d-%Om-%Y", origin="1970-01-01 UTC", tz = "GMT")
    serie_period = interval(initial_date,final_date)
    lenght_timeserie <- time_length(serie_period,"hour")
    period = initial_date + hours(seq(from = 0,lenght_timeserie,by = 3))
    period = data.frame(numerica = as.numeric(period), date_time = period)
    
    # Creating the exit file
    grouped_data = data.frame(date_time = period)
    grouped_data = cbind(grouped_data,matrix(ncol= dimensions[1]*dimensions[2]))
    
    # Creating the exit file names
    lon_names = paste("x_",Lon) # Standard Longitude name
    lat_names = paste("y_", Lat) # Standard Latitude name
    names = c() #vector to save the data names
    
    for(x in 1:(length(Lon))){ #the sequence data! it cant be changed!
      for(y in 1: length(Lat)){
        coord_n = paste0(lon_names[x],"_",lat_names[y])
        names = c(names, coord_n)
      } # end for y
    }# end for x
    colnames(grouped_data) = c("numerica", "date_time", names)
  }# end if k==1 (first run!)
  
  # Creating data storage for kth file
  data_file = data.frame(matrix(ncol= dimensions[1]*dimensions[2]+2, nrow = dimensions[3])) # Data Lenght
  lon_names = paste("x_",Lon) # Standard Longitude name
  lat_names = paste("y_", Lat) # Standard Latitude name
  names = c() # vector to save the data names
  
  for(x in 1:(length(Lon))){ #the sequence data!
    for(y in 1: length(Lat)){
      coord_n = paste0(lon_names[x],"_",lat_names[y])
      names = c(names, coord_n)
    } # end for y
  }# end for x
  colnames(data_file) = c("Date_time","Numeric_date", names)
  
  # Data Organizer
  for(x in 1:(dimensions[1])){ # the sequence data!
    for(y in 1: (dimensions[2])){ 
      for(z in 1:(dimensions[3])){
        data_file[z,2+((x-1)*dimensions[1])+y]= Rain_3h[x,y,z]
      }# end for z
    } # end for y
  }# end for x
  
  # creating date time to Mswep data file
  # MSWEP data reference 
  reference_date = as.POSIXct(as.character("31-12-1899"),format="%d-%Om-%Y", origin="1970-01-01 UTC")
  
  # first file date
  date_time_mswep_inicial = as_datetime(file$Time[1]*24*60*60, origin = "1899-12-31 UTC",tz = "GMT")
  date_time_mswep_final = as_datetime(file$Time[length(file$Time)]*24*60*60, origin = "1899-12-31 UTC",tz = "GMT") #Observation: datetime command works in seconds, so it is necessary modify this time. 
  serie_period = interval(date_time_mswep_inicial,date_time_mswep_final) 
  lenght_timeserie = time_length(serie_period,"hour") #Time serie Lenght by hourly time
  period = date_time_mswep_inicial + hours(seq(from = 0,lenght_timeserie,by = 3))
  
  data_file$Date_time = period
  data_file$Numeric_date = as.numeric(period)
  
  # Grouping Data
  Indx_date = which((grouped_data$numerica>=as.numeric(date_time_mswep_inicial))&(grouped_data$numerica<=as.numeric(date_time_mswep_final)))
  
  grouped_data[Indx_date,c(3:ncol(grouped_data))]= data_file[,c(3:ncol(data_file))]
  
  close.nc(file_nc)# Closing to not broken!
} # end main for (files)

write.csv(grouped_data, choose.files(caption = "Create the output file. Dont forget the '.csv' extention"),row.names = F)




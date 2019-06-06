	library(sf) 
	library(ncdf4)
	library(raster)
	library(rasterVis)
	library(RColorBrewer)
	library(ncdf.tools)
	library(ggplot2)
	library(ggthemes)
	library(zoo)
  	
  	shp_path <- "C:/Users/Geoffrey/Documents/Rdata/"
	shp_name <- "ne_110m_admin_0_countries.shp"
	shp_file <- paste(shp_path, shp_name, sep="")

	ncpath <- "C:/Users/Geoffrey/Documents/Rdata/"
	ncname <- "soilw.mon.ltm.v2.nc"  
	ncfname <- paste(ncpath, ncname, sep="")
	soil <- raster(ncfname, varname="soilw")


	mapTheme <- rasterTheme(region=brewer.pal(8,"Reds"))
	levelplot(soil, margin=T, par.settings=mapTheme)
  
   ![](Basic.png)
   
   ncpath <- "C:/Users/Geoffrey/Documents/Rdata/"
	ncname <- "soilw.mon.ltm.v2.nc"    
	ncfname <- paste(ncpath, ncname, sep="")
	dname <- "soilw"

	monthly <- stack(ncfname)
	names(monthly) <- c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")

	pngfile <- "months.png"
	png(pngfile, width=729, height=729)
	levelplot(monthly , cuts=11, pretty=T, 
                 	col.regions=(rev(brewer.pal(10,"RdBu"))))
   
   ![](months.png)
   
  	ncpath <- "C:/Users/Geoffrey/Documents/Rdata/"
	ncname <- "soilw.mon.ltm.v2.nc"   
	ncfname <- paste(ncpath, ncname, sep="")
	dname_mon <- "soilw"

	ncin <- nc_open(ncfname)
	print(ncin)

	lon <- ncvar_get(ncin,"lon")
	nlon <- dim(lon)
	head(lon)

	lat <- ncvar_get(ncin,"lat")
	nlat <- dim(lat)
	head(lat)

	time <- ncvar_get(ncin,"time")
	tunits <- ncatt_get(ncin,"time","units")
	nt <- dim(time)
	print (c(nlon, nlat, nt))

	tustr <- strsplit(tunits$value, " ")
	ptime <- convertDateNcdf2R(time, unlist(tustr)[1], 
                           origin = as.POSIXct(unlist(tustr)[3],                                                tz = "UTC"), time.format = "%Y-%m-%d")
	head(time); tail(time)

	head(ptime); tail(ptime)

	SST_mon <- ncvar_get(ncin,dname_mon)
	dlname_mon <- ncatt_get(ncin,dname_mon,"long_name")
	dunits_mon <- ncatt_get(ncin,dname_mon,"units")
	fillvalue_mon <- ncatt_get(ncin,dname_mon,"_FillValue")
	dim(SST_mon)

	nc_close(ncin)

	temp_lat <- rep(NA, nlat)
	temp_lat[1:nlat] <- lat[nlat:1]
	temp_lat

	lat <- temp_lat

	temp_array <- array(NA, dim = c(nlon, nlat, nt))
	temp_array[1:nlon, 1:nlat, 1:nt] <-  SST_mon[1:nlon, nlat:1, 1:nt] 
	SST_mon <- temp_array

	temp_lon <- rep(NA, nlon)
	temp_lon[1:(nlon/2)] <-  lon[((nlon/2)+1):nlon] - 360.0
	temp_lon[((nlon/2)+1):nlon] <-  lon[1:(nlon/2)]
	temp_lon
	lon <- temp_lon

	temp_array <- array(NA, dim = c(nlon, nlat, nt))
	temp_array[1:(nlon/2), 1:nlat, 1:nt] <-  SST_mon[((nlon/2)+1):nlon, nlat:1, 1:nt] 
	temp_array[((nlon/2)+1):nlon, 1:nlat, 1:nt] <-  SST_mon[1:(nlon/2), nlat:1, 1:nt]
	SST_mon <- temp_array

	n <- 2
	grid <- expand.grid(lon=lon, lat=lat)
	levelplot(SST_mon[,, n] ~ lon * lat, data=grid, cuts=11, pretty=T, 
                 col.regions=(rev(brewer.pal(10,"RdBu"))))
   
   ![](Hollzernew.png)
   
	SST_mon <- SST_mon[,, 1:(nt-3)]
	dim(SST_mon)

	time <- time[1:(nt-3)]
	ptime <- ptime[1:(nt-3)]
	head(ptime); tail(ptime)

	nt <- nt - 3

	SST_path <- "C:/Users/Geoffrey/Documents/Rdata/"
	SST_ltm_name <- "soilw.mon.ltm.v2.nc"
	SST_ltm_file <- paste(SST_path, SST_ltm_name, sep="")
	dname_ltm <- "soilw"

	ncin <- nc_open(SST_ltm_file)

	time <- ncvar_get(ncin,"time")
	tunits <- ncatt_get(ncin,"time","units")
	nm <- dim(time)
	nm; tunits

	SST_ltm <- ncvar_get(ncin,dname_ltm)
	dlname_ltm <- ncatt_get(ncin,dname_ltm,"long_name")
	dunits_ltm <- ncatt_get(ncin,dname_ltm,"units")
	fillvalue_ltm <- ncatt_get(ncin,dname_ltm,"_FillValue")
	dim(SST_ltm)
	nc_close(ncin)

	temp_array <- array(NA, dim = c(nlon, nlat, nm))
	temp_array[1:nlon, 1:nlat, 1:nm] <-  SST_ltm[1:nlon, nlat:1, 1:nm] 
	SST_ltm <- temp_array

	temp_lon <- rep(NA, nlon)
	temp_lon[1:(nlon/2)] <-  lon[((nlon/2)+1):nlon] - 360.0
	temp_lon[((nlon/2)+1):nlon] <-  lon[1:(nlon/2)]
	temp_lon
	lon <- temp_lon

	temp_array <- array(NA, dim = c(nlon, nlat, nt))
	temp_array[1:(nlon/2), 1:nlat, 1:nt] <-  SST_ltm[((nlon/2)+1):nlon, nlat:1, 1:nt]
	temp_array[((nlon/2)+1):nlon, 1:nlat, 1:nt] <-  SST_ltm[1:(nlon/2), nlat:1, 1:nt]
	SST_ltm <- temp_array

	n <- 8
	grid <- expand.grid(lon=lon, lat=lat)
	levelplot(SST_ltm[,, n] ~ lon * lat, data=grid, cuts=11, pretty=T, 
                 col.regions=(rev(brewer.pal(10,"RdBu")))) 	
  
   ![](Hollzer1new.png)
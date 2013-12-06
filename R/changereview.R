
## here you should put your file path

reshape.change  <- function(fileName){
	if(missing(fileName))
		fileName = system.file(package='changereview','data','ulink.json')
	ll <- unlist(fromJSON(fileName),recursive=F)
	nn <- c("kind","project","branch","subject","status","created","updated","_number", "owner")
	dat <- rbindlist(lapply(ll, "[",nn))
	## format dates
	DT <- dat[,	 `:=` ( updated = as.POSIXct(strftime(updated, "%Y-%m-%d %H:%M:%OS0")),
											created = as.POSIXct(strftime(created, "%Y-%m-%d %H:%M:%OS0")))
						]
	DT <- dat[status == 'MERGED' & !grepl('api$',project)
						][,`:=` ( duration = as.numeric(updated - created),
											proj.family = {
												ll <- strsplit(project,'-')
												factor(sapply(ll,function(x)
													if(length(x)==1)x else x[2]))
											})]
	
	DT
}


plot.change <- function(fileName){
	DT <- reshape.change(fileName)
	
	p1 <- ggplot(DT,aes(x=duration)) +
		geom_histogram(aes(fill=proj.family)) + 
		scale_x_log10() +
		xlab('duration in seconds') + 
		ggtitle('Distribution of change duration')
	
	p2 <- ggplot(DT, aes(owner, project)) + 
		geom_tile(aes(fill = duration)) + 
		theme(axis.text.x=element_text(angle=90)) +
		scale_fill_gradient(low = "blue",  high = "red")
	
	print(p1)
	print(p2)
	list(histo=p1,
			 heatmap=p2)
}






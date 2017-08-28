#define a function to convert a given edf
convert_edf = function(file,.pb=NULL){
	if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
	system(
		command = paste("./edf2asc",file)
	)
	system(
		command = paste("rm",file)
	)
	file = gsub('edf','asc',file)
	system(
		command = paste("gzip --fast",file)
	)
}

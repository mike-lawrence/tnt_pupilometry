#define a function to process a given asc
pre_process_eye_data = function(x,.pb=NULL){
	if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
	if(!file.exists(gsub('.asc.gz','_blinks.txt.gz',x))){
		#read everything
		a = readLines(x)

		#remove header
		a = a[substr(a,1,2)!="**"]

		#remove calibration headers
		a = a[substr(a,1,7)!=">>>>>>>"]

		#remove blank lines
		a = a[a!=""]

		#remove lines that start with blanks (from clibration)
		a = a[substr(a,1,1)!=" "]
		a = a[substr(a,1,1)!="\t"]

		#isolate & write msgs
		subset = (substr(a,1,3)=="MSG") | (substr(a,1,5)=="START") | (substr(a,1,5)=="END")
		msgs = a[subset]
		write(msgs,file = gsub('.asc.gz','_msgs.txt',x))
		system(
			command = paste("gzip -f --fast",gsub('.asc.gz','_msgs.txt',x))
		)
		rm(msgs)
		a = a[!subset]
		gc()

		#isolate & write inputs
		subset = substr(a,1,5)=="INPUT"
		inputs = a[subset]
		write(inputs,file = gsub('.asc.gz','_inputs.txt',x))
		system(
			command = paste("gzip -f --fast",gsub('.asc.gz','_inputs.txt',x))
		)
		rm(inputs)
		a = a[!subset]
		gc()

		#isolate & write samples
		subset = substr(a,nchar(a)-2,nchar(a))=="..."
		samples = a[subset]
		write(samples,file = gsub('.asc.gz','_samples.txt',x))
		system(
			command = paste("gzip -f --fast",gsub('.asc.gz','_samples.txt',x))
		)
		rm(samples)
		a = a[!subset]
		gc()

		#isolate & write saccades
		subset = substr(a,1,5)=="ESACC"
		saccades = a[subset]
		write(saccades,file = gsub('.asc.gz','_saccades.txt',x))
		system(
			command = paste("gzip -f --fast",gsub('.asc.gz','_saccades.txt',x))
		)
		rm(saccades)
		a = a[!subset]
		gc()

		#isolate & write blinks
		subset = substr(a,1,6)=="EBLINK"
		temp = a[subset]
		write(temp,file = gsub('.asc.gz','_blinks.txt',x))
		system(
			command = paste("gzip -f --fast",gsub('.asc.gz','_blinks.txt',x))
		)
		rm(temp)
		a = a[!subset]
		gc()
	}

}

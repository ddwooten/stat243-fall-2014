#!/bin/bash

echo "****************************************"
echo "Begin Script"

#Set URL shell variable, will be useful

URL=http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/

#Check for the html index file. If absent, retrieve
if [ ! -e index.html ]; then
	echo "Index file missing. Retreiving index file"
	wget $URL
else
	echo "Index file present. Process index file."
fi

#Create temporary storage file
touch foo.txt

#grep through the index file and feed lines into cut for appropriate trimming
grep ".txt\"" index.html | cut -d"\"" -f8 > foo.txt

#Now we loop through the file, foo.txt, which contains the names of all of
#our text files.

while read p; do
#check to see if the file already exists, if not, download
	if [ ! -e $p ]; then
		echo "Retrieving file "$p
		wget $URL$p
	else
		echo "File already exists. Abort download"
	fi
done <foo.txt

#Clean up the local directory
rm -f foo.txt

echo "End Script"
echo "****************************************"

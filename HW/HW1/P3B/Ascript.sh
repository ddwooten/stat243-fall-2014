#!/bin/bash
#Script to accomplish part B of question 3

echo "***********************************"
echo "Script Begin"

#We check to see if cn14.zip exists and if not, we download it
if [ ! -e cn14.zip ]; then
	echo "Retreiving file"
	wget ftp://ftp.fec.gov/FEC/2014/cn14.zip
else
	echo "cn14.zip file already downloaded"
fi

#We check to see if indiv14.zip exists and if not, we download it
if [ ! -e indiv14.zip ]; then
	echo "Retreiving file"
	wget ftp://ftp.fec.gov/FEC/2014/indiv14.zip
else
	echo "indiv14.zip file already downloaded"
fi

#We check to see if cn14.zip has been unzipped, if not, do so
if [ ! -e cn.txt ]; then
	echo "Uniziping file"
	unzip cn14.zip
else
	echo "File already unziped"
fi

#We check to see if indiv14.zip is unzipped, if not, do so
if [ ! -e itcont.txt ]; then
	echo "Uniziping file"
	unzip indiv14.zip
else
	echo "File already unziped"
fi

#Receive name to search for from user
echo "Please enter candidate's last name"

read name

echo "You have input candidate" $name

echo "Searching for" $name"'s ID number"

#Search for ID number of given candidate name
ID=$(grep -i -m 1 "|$name" cn.txt | cut -d'|' -f10)

echo $name"'s ID number is" $ID

#This grep will count the number of times that
# ID shows up, effectively the number of contributions
# nation wide
nat=$(grep -ci "$ID|" itcont.txt)

#This grep will do the same as above with the additional
# critiera of CA as the state
cali=$(grep -i "$ID|" itcont.txt | grep -ic "CA|")

echo "The number of national contributors to "$name"'s campaign is "$nat
echo "The number of California contributors to "$name"'s campaign is "$cali

echo "Script End"
echo "***********************************"

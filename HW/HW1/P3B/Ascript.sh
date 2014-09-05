#!/bin/bash
#Script to accomplish part B of question 3

echo "***********************************"
echo "Script Begin"

if [ ! -e cn14.zip ]; then
	echo "Retreiving file"
	wget ftp://ftp.fec.gov/FEC/2014/cn14.zip
else
	echo "cn14.zip file already downloaded"
fi

if [ ! -e indiv14.zip ]; then
	echo "Retreiving file"
	wget ftp://ftp.fec.gov/FEC/2014/cn14.zip
else
	echo "indiv14.zip file already downloaded"
fi

if [ ! -e cn.txt ]; then
	echo "Uniziping file"
	unzip cn14.zip
else
	echo "File already unziped"
fi

if [ ! -e itcont.txt ]; then
	echo "Uniziping file"
	unzip indiv14.zip
else
	echo "File already unziped"
fi

echo "Script End"
echo "***********************************"

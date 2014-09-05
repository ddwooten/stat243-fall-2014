#!/bin/bash

echo "Running script"

if [ -e *.tex ]; then
	echo "Tex file exists"
else
	echo "Tex file doesn't exit"
fi

if 4>2; then
	echo "Math works"
else
	echo "Math fails"
fi

ls

echo "Script end"

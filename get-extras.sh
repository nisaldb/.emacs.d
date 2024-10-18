#!/usr/bin/env bash

if [ ! -d "./extra" ]; then
	mkdir extra
fi

cd extra

if [ ! -d "no-littering" ]; then
	echo "Downloading no-littering..."
	git clone https://github.com/emacscollective/no-littering.git no-littering
	echo "Downloaded: no-littering"
fi

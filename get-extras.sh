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

if [ ! -d "compat" ]; then
	echo "Downloading compat..."
	git clone https://github.com/emacs-compat/compat.git compat
	echo "Downloaded: compat"
fi

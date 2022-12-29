#!/bin/bash
# keep above line to select correct syntax in editor

if [ ! -e "/etc/os-release" ]; then
	echo "This machine does not support /etc/os-release"
	ID="unknown"
	VERSION_ID="0.0"
fi
source /etc/os-release

linux_distribution_name() {
	echo $ID
}

linux_distribution_version() {
	echo $VERSION_ID
}


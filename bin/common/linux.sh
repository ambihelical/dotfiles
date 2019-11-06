#!/bin/bash
# keep above line to select correct syntax in editor

# return the name of the current linux distribution
linux_distribution_name() {
	local distro="Unknown"
	if hash lsb_release 2> /dev/null; then
		 local release=( $(lsb_release -ds) )
		 distro=${release[0]}
		 distro=${release//\"}
	fi
	echo $distro
}

# return the linux distribution as a comparable integer
# ISSUE: this only works for Ubuntu's numbering convention,
# Other distributions may not be so readable if the numbers
# must stay comparable.
linux_distribution_version() {
	local version="0000"
	if hash lsb_release 2> /dev/null; then
		 local release=$(lsb_release -rs)
		 version=${release//.}
		 version=${version:0:4}
	fi
	echo $version
}

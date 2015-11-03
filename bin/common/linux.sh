
# return the name of the current linux distribution
linux_distribution_name() {
	distro="Unknown"
	if hash lsb_release 2> /dev/null; then
		 release=( $(lsb_release -ds) )
		 distro=${release[0]}
	fi
	echo $distro
}

# return the linux distribution as a 4 digit number
linux_distribution_version() {
	version="0000"
	if hash lsb_release 2> /dev/null; then
		 release=( $(lsb_release -ds) )
		 version=${release[1]//.}
	fi
	echo $version
}

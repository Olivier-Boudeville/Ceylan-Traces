#!/bin/sh

# Copyright (C) 2022-2026 Olivier Boudeville
#
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
#
# This file is part of the Ceylan-Traces library (see
# http://traces.esperide.org).


ext=".traces"

help_short_opt="-h"
help_long_opt="--help"

usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}]: opens the most recent trace file found in the current directory."


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit 0

fi

viewer="$(which v 2>/dev/null)"

if [ ! -x "${viewer}" ]; then

	echo "  Error, no 'v' viewer found (so Ceylan-Hull is not in PATH)." 1>&2

	exit 5

fi


last_file="$(/bin/ls -1 -rt *${ext} 2>/dev/null | tail -n 1)"

if [ -f "${last_file}" ]; then

	"${viewer}" "${last_file}"

else

	echo "(no local trace file found)"

fi

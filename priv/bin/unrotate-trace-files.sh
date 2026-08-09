#!/bin/sh

# Copyright (C) 2026-2026 Olivier Boudeville
#
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
#
# This file is part of the Ceylan-Traces library (see
# http://traces.esperide.org).

help_short_opt="-h"
help_long_opt="--help"

view_short_opt="-v"
view_long_opt="--view"

usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}] [${view_short_opt}|${view_long_opt}]: \"unrotates\" (i.e. decompresses and unsplits in the right order) the archives of the rotated trace files (e.g. 'my_foo.traces.*.2026-8-8-at-*.xz') found in the current directory, in order to reconstruct the corresponding single, full, uncompresssed original overall trace file (e.g. 'my_foo.traces') that can be readily processed by a trace supervisor.

Note that this script is expected to detect if multiple runs of the target application have been made in the current directory and their rotated traces have not been removed, so that they are not included in the generated overall trace file.

If the ${view_short_opt} or ${view_long_opt} option is specified, these resulting merged traces will be automatically displayed by a trace supervisor.
"


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit

fi


view=1

if [ "$1" = "${view_short_opt}" ] || [ "$1" = "${view_long_opt}" ]; then

	shift
	#echo "(trace viewing enabled)"

	# Relying on 'v', provided by Ceylan-Hull:
	viewer_exec="$(which v 2>/dev/null)"

	#viewer_exec="$(which logmx.sh 2>/dev/null)"

	if [ ! -x "${viewer_exec}" ]; then

		echo "  Error, trace viewing selected, yet not trace viewer found." 1>&2

		exit 3

	fi

	view=0

fi


if [ ! $# -eq 0 ]; then

	echo "  Error, extra parameter(s) specified.
${usage}" 1>&2

	exit 5

fi


unxz_exec="$(which unxz 2>/dev/null)"

if [ ! -x "${unxz_exec}" ]; then

	echo "  Error, no 'unxz' executable found." 1>&2

	exit 8

fi



# Expecting a single original trace file like 'foo' in
# foo.traces.1.2026-8-8-at-18h-32m-11s.xz:
#
# (sorting by rotation count rather than by timestamps, and by value, with -V,
# rather than alphabetically, so that trace_rotation_test.traces.3 is before
# trace_rotation_test.traces.20)
#
#archives="$(/bin/ls -1 *.traces.*.xz 2>/dev/null)"
archives="$(printf "%s\n" *.traces.*.xz | sort -V)"
#echo "archives = $archives"
printf "%s\n" aa.* | sort -V
if [ -z "${archives}" ]; then

	printf " Error, no archive trace file (*.traces.*.xz) found in the local directory." 1>&2

	exit 10

fi


# As a final space may linger:
trace_base="$(for arc in ${archives}; do echo "${arc}" | sed 's|\..*||1'; done | sort -u | sed 's|[[:space:]]$||1')"

# For example "trace_rotation_test":
#echo "trace_base = $trace_base"

prefix_count="$(echo "${trace_base}" | wc -l)"
#echo "prefix_count = ${prefix_count}"

if [ ! "${prefix_count}" = "1" ]; then

	printf " Error, ${prefix_count} different prefixes found for trace files (instead of a single one):\n${trace_base}" 1>&2

	exit 15

fi

target_file="${trace_base}.traces"

printf "Unrotating the following local trace archives for the auto-detected '${trace_base}' trace prefix:\n${archives}\n\n"

for arc in ${archives}; do

	# Check that no clash with a former rotated trace archive with the same
    # rotation count arises:
	#
	# (e.g. just extract the (first) "1" in
	# trace_rotation_test.traces.1.2026-8-9-at-9h-42m-38s.xz)
    #
    # (note that we want to support any starting rotation count)
	#
	rot_count="$(echo "${arc}" | sed "s|^${target_file}\.||1" | sed 's|\..*||1')"
	#echo "for arc = ${arc}, rot_count = ${rot_count}."

	arc_count="$(/bin/ls -1 "${target_file}.${rot_count}".*.xz | wc -l)"
	#echo "arc_count = ${arc_count}"

	if [ ! "${arc_count}" = "1" ]; then

	  echo "  Error, multiple (${arc_count}) trace archives with the same rotation count (${rot_count}) have been found (see ${trace_base}.${rot_count}.*.xz). Trace archives of past runs must be colliding, remove the unwanted ones first." 1>&2

	  exit 20

	fi

	printf " - uncompressing ${arc}\n"
	if ! "${unxz_exec}" --force --keep "${arc}" 1>/dev/null; then

		echo " Error, the uncompressing of '${arc}' failed." 1>&2

		exit 20

	fi

done


trace_files="$(for arc in ${archives}; do echo "${arc}" | sed 's|\.xz$||1'; done)"
#echo "trace_files = ${trace_files}"

if [ -e "${target_file}" ]; then

	/bin/rm -f "${target_file}"

fi

touch "${target_file}"
printf "\nAll archives uncompressed, merging them in '${target_file}'.\n"

if ! cat ${trace_files} > "${target_file}"; then

	echo " Error, the uncompressing of '${arc}' failed." 1>&2

	exit 5

fi


if [ $view -eq 0 ]; then

	echo "The '${target_file}' is ready, displaying it now..."

	"${viewer_exec}" "${target_file}" 1>/dev/null &

else

	echo "The '${target_file}' is ready!"

fi

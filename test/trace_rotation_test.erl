% Copyright (C) 2021-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Traces library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Sunday, January 17, 2021.


% Unit tests for the rotation of the trace file.
-module(trace_rotation_test).


-include("traces_for_tests.hrl").



% Runs the test.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_debug( "Hello Traces, preparing to rotate!" ),

	TraceAggPid = class_TraceAggregator:get_aggregator(),

	% Calling the request version:
	TraceAggPid ! { rotateTraceFileSync, [], self() },

	?test_info( "Waiting for the acknowledgement of trace rotation." ),

	BinFilePath = receive

		{ wooper_result, { trace_file_rotated, BinRotatedFilePath } } ->
			?test_info_fmt( "Trace rotation acknowledged, result in '~s'.",
							[ BinRotatedFilePath ] ),
			BinRotatedFilePath

	end,

	?test_debug_fmt( "Removing '~s'.", [ BinFilePath ] ),
	file_utils:remove_file( BinFilePath ),

	?test_debug_fmt( "End of test for ~s.", [ ?MODULE ] ),

	?test_stop.

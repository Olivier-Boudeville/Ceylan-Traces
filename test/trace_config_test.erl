% Copyright (C) 2021-2025 Olivier Boudeville
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
% Creation date: Wednesday, September 15, 2021.

-module(trace_config_test).

-moduledoc """
Allows to test the **configuration** of traces and of the related tools
(e.g. trace supervisors), for example regarding encoding.
""".


% Test target:
-include("traces_for_tests.hrl").


% Runs the test.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_debug( "Hello debug!" ),

	case executable_utils:is_batch() of

		true ->
			?test_warning( "Running in batch mode." );

		false ->
			?test_warning( "Running in interactive mode." )

	end,

	% Pipe being the field separator used internally for traces:
	?test_debug( "Testing a message with pipes: AA|BB|CC|" ),

	?test_debug( "Testing a message with non-Latin1 characters: "
				 "àâäéèêëîïôöùûü" ),

	?test_debug_fmt( "End of test for ~ts.", [ ?MODULE ] ),

	?test_stop.

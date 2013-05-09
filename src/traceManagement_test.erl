% Copyright (C) 2003-2013 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.


% Unit tests for the implementation of trace management.
%
% See the following modules:
%  - class_TraceAggregator
%  - class_TraceSupervisor
%
% Note: trace services are among the most generic services offered, they are
% used in the vast majority of tests but this one, as the purpose of this test
% is actually to test traces by themselves (cannot use the trace system to test
% the trace system!).
-module(traceManagement_test).


-define(Tested_modules,[ class_TraceEmitter, class_TraceAggregator,
	class_TraceSupervisor, class_TraceListener ]).


% For trace facilities:
-include("traces_for_tests.hrl").



-define(Prefix,"--> ").



% Run the tests.
%
% Note: this is the only test that does not use the trace functionalities for
% its own behaviours (since it is the subject of the test).
run() ->

	io:format( ?Prefix "Testing module ~w.~n", [ ?Tested_modules ] ),
	
	io:format( ?Prefix "Starting Trace system, with a trace aggregator "
		"and, if requested, a trace supervisor.~n" ),
	?test_start,
	
	
	case init:get_argument('-batch') of
	
		{ok,_} ->
			io:format( ?Prefix "Running in batch mode.~n" );

		_ ->
			io:format( ?Prefix "Running in interactive mode.~n" )
			
	end,
	
	
	io:format( ?Prefix "Creating a new TestTraceEmitter.~n" ),
	
	Name = "I am a test emitter of traces",
	
	% Should not trigger the launch of another global aggregator:
	% (as test_start triggers a *synchronous* aggregator):
	MyTraceEmitter = class_TestTraceEmitter:synchronous_new_link(Name),	
		
	?test_fatal(   "This is a test of the fatal priority for tests."   ),
	?test_error(   "This is a test of the error priority for tests."   ),
	?test_warning( "This is a test of the warning priority for tests." ),
	?test_info(    "This is a test of the info priority for tests."    ),
	?test_trace(   "This is a test of the trace priority for tests."   ),
	?test_debug(   "This is a test of the debug priority for tests."   ),
	
	
	?test_fatal_fmt( "This is a test of the ~w priority for tests.", [fatal]),
	?test_error_fmt( "This is a test of the ~w priority for tests.", [error] ),
	?test_warning_fmt( "This is a test of the ~w priority for tests.", 
					  [warning] ),
	?test_info_fmt( "This is a test of the ~w priority for tests.", [info] ),
	?test_trace_fmt( "This is a test of the ~w priority for tests.", [trace] ),
	?test_debug_fmt( "This is a test of the ~w priority for tests.", [debug] ),
	
	
	io:format( ?Prefix 
		"Requesting the TestTraceEmitter to send some traces.~n" ),
	
	% Wait until there is an answer for this trace emitter:
	MyTraceEmitter ! {sendTraces,[],self()},
	
	receive
	
		{wooper_result,ok} ->
			io:format( ?Prefix "Traces sent.~n" )	
						
	end,

	ExpectedFirstBinaryName = list_to_binary(Name),
	
	MyTraceEmitter ! {getName,[],self()},
	receive
	
		{wooper_result,ExpectedFirstBinaryName} ->
			?test_info( "Correct name returned." )
			
	end,
	
	NewName = "This is my new name",
	
	MyTraceEmitter ! {setName,[NewName]},

	ExpectedSecondBinaryName = list_to_binary(NewName),

	MyTraceEmitter ! {getName,[],self()},
	receive
	
		{wooper_result,ExpectedSecondBinaryName} ->
			?test_info( "Correct new name returned." )
			
	end,
	
   	io:format( ?Prefix "Deleting this TestTraceEmitter.~n" ),
	
	MyTraceEmitter ! delete,

	?test_stop.


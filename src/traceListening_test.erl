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


% Unit tests for the implementation of trace listening.
%
% See the following modules:
%  - class_TraceListener
%  - class_TraceSupervisor
%  - class_TraceAggregator
% Made to be executed while the traceManagement_test is running.
-module(traceListening_test).


-define( Tested_modules, [ class_TraceListener, class_TraceAggregator,
	class_TraceSupervisor ] ).


% For trace facilities:
-include("traces_for_tests.hrl").


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").


-define(Prefix,"--> ").


% ?test_stop should not be used here as its wait_for_any_trace_supervisor macro
% would wait for a non-launched supervisor.


send_traces( 0 ) ->
	ok;

send_traces( Count ) ->
	?test_trace_fmt( "Emitting trace #~B.", [Count] ),
	send_traces( Count-1 ).



send_timed_traces( 0 ) ->
	ok;

send_timed_traces( Count ) ->
	?test_trace_fmt( "Emitting timed trace #~B.", [Count] ),
	timer:sleep(100),
	send_timed_traces( Count-1 ).



% The real code of the test, in a separate function to avoid an indentation
% offset.
test_actual_body() ->

	[_H,NodeName] = string:tokens( atom_to_list(node()), "@" ),

	TargetVMName = lists:flatten(
		io_lib:format( "traceManagement_run-~s@~s",
					  [system_utils:get_user_name(), NodeName] ) ),

	io:format( ?Prefix "Connecting to '~s'.~n", [TargetVMName]),

	case net_adm:ping( list_to_atom(TargetVMName) ) of

		pong ->
			ok;

		pang ->
			io:format( ?Prefix "Error, the trace management test "
				"should already be running.~n For example, execute "
				"'make traceManagement_run' in another terminal.~n" ),
			throw( {no_trace_aggregator_to_listen,TargetVMName} )

	end,

	% Otherwise the remote node could not be known before use:
	global:sync(),

	io:format( ?Prefix "Globally registered names: ~w.~n",
		[global:registered_names()]),

	AggregatorName = ?trace_aggregator_name,
	io:format( ?Prefix "Looking up aggregator by name: ~s.~n",
		[AggregatorName] ),

	AggregatorPid = case global:whereis_name( AggregatorName ) of

		Pid when is_pid(Pid) ->
			Pid

	end,

	io:format( ?Prefix "Sending initial traces to force "
		"a real synchronization.~n" ),
	send_traces( 50 ),

	% No ?test_start: we will be using the aggregator from the node named
	% 'traceManagement_run'.
	io:format( ?Prefix "Creating a test trace local listener.~n" ),
	MyTraceListener = class_TraceListener:synchronous_new_link(AggregatorPid),

	send_timed_traces( 20 ),

	% Could wait here for any event before stopping.

	io:format( ?Prefix "Deleting this test trace listener.~n" ),

	MyTraceListener ! delete,

	% To ensure the message has been sent before the VM shuts down:
	timer:sleep(500),

	?test_info_fmt( "End of test for module(s) ~w.", [ ?Tested_modules ]  ),
	check_pending_wooper_results(),
	test_finished().



% Run the tests.
run() ->

	io:format( ?Prefix "Testing module ~w. "
		"'make traceManagement_run' supposed to be already executed.~n",
		[ ?Tested_modules ] ),


	case init:get_argument('-batch') of

		{ok,_} ->
			io:format( ?Prefix "Running in batch mode, no traceManagement_test "
				"supposed to be running, nothing done.~n" ),
			io_lib:format( "End of test for module(s) ~w.",
				[ ?Tested_modules ] ),
			check_pending_wooper_results(),
			test_finished();

		_ ->
			io:format( ?Prefix "Running in interactive mode, "
				"'make traceManagement_run' supposed to be already running.~n"
			),
			test_actual_body()

	end.

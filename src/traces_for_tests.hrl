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


% Defines some macros and functions useful for trace-using tests.


% To avoid warnings if not used:
-export([ test_receive/0, test_failed/1, check_pending_wooper_results/0,
		 test_finished/0 ]).



% For testFinished:
-include("test_facilities.hrl").


% For TraceFilename:
-include("traces.hrl").



% For TracingActivated:
%-include("class_TraceEmitter.hrl").




-ifdef(TracingActivated).


-define( test_fatal(Message),
	class_TraceEmitter:send_from_test( fatal, Message )
).


-define( test_fatal_fmt( MessageFormat, FormatValues ),
	class_TraceEmitter:send_from_test( fatal,
						 io_lib:format( MessageFormat, FormatValues ) )
).



-define( test_error(Message),
	class_TraceEmitter:send_from_test( error, Message )
).


-define( test_error_fmt( MessageFormat, FormatValues ),
	class_TraceEmitter:send_from_test( error,
						 io_lib:format( MessageFormat, FormatValues ) )
).



-define( test_warning(Message),
	class_TraceEmitter:send_from_test( warning, Message )
).


-define( test_warning_fmt( MessageFormat, FormatValues ),
	class_TraceEmitter:send_from_test( warning,
						 io_lib:format( MessageFormat, FormatValues ) )
).



-define( test_info(Message),
	class_TraceEmitter:send_from_test( info, Message )
).


-define( test_info_fmt( MessageFormat, FormatValues ),
	class_TraceEmitter:send_from_test( info,
						 io_lib:format( MessageFormat, FormatValues ) )
).



-define( test_trace(Message),
	class_TraceEmitter:send_from_test( trace, Message )
).


-define( test_trace_fmt( MessageFormat, FormatValues ),
	class_TraceEmitter:send_from_test( trace,
						 io_lib:format( MessageFormat, FormatValues ) )
).



-define( test_debug(Message),
	class_TraceEmitter:send_from_test( debug, Message )
).


-define( test_debug_fmt( MessageFormat, FormatValues ),
	class_TraceEmitter:send_from_test( debug,
						 io_lib:format( MessageFormat, FormatValues ) )
).



-else.


% Here TracingActivated is not defined: traces are disabled.


% Allows to avoid warnings about variables not be used when traces are disabled:
test_trace_disabled(_) ->
	test_trace_disabled.

test_trace_disabled(_,_) ->
	test_trace_disabled.



% Message is returned, as otherwise some variables in calling code could be
% determined as unused, and thus would trigger a warning:

-define( test_fatal(Message), test_trace_disabled(Message) ).

-define( test_error(Message), test_trace_disabled(Message) ).

-define( test_warning(Message), test_trace_disabled(Message) ).

-define( test_info(Message), test_trace_disabled(Message) ).

-define( test_trace(Message), test_trace_disabled(Message) ).

-define( test_debug(Message), test_trace_disabled(Message) ).



-define( test_fatal_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).

-define( test_error_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).

-define( test_warning_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).

-define( test_info_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).

-define( test_trace_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).

-define( test_debug_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).


-endif.



-ifdef(TracingActivated).


-define( test_start,
	% Create first, synchronously (to avoid race conditions), a trace aggregator
	% (false is to specify a non-private i.e. global aggregator).
	%
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be already created at start-up).
	%
	% Goes back to the beginning of line:
	io:format( "~n" ),
	TestIsBatch = case init:get_argument('-batch') of
		{ok,_} -> true; _ -> false
	end,
	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		?TraceFilename, ?TraceType, ?TraceTitle, _TraceIsPrivate=false,
		TestIsBatch ),
	?test_info_fmt( "Testing module(s) ~w.", [ ?Tested_modules ] ),
	% Defined in class_TraceSupervisor.hrl:
	?init_trace_supervisor
).



-define( test_stop,
	?test_info_fmt( "End of test for module(s) ~w.", [ ?Tested_modules ] ),
	% Defined in class_TraceSupervisor.hrl:
	?wait_for_any_trace_supervisor,
	TraceAggregatorPid ! {synchronous_delete,self()},
	receive

		{deleted,TraceAggregatorPid} ->
			ok

	end,
	check_pending_wooper_results(),
	test_finished()
).



-define( test_stop_without_waiting_for_trace_supervisor,
	?test_info_fmt( "End of test for module(s) ~w.", [ ?Tested_modules ] ),
	TraceAggregatorPid ! {synchronous_delete,self()},
	receive

		{deleted,TraceAggregatorPid} ->
			ok

	end,
	check_pending_wooper_results(),
	test_finished()
).



-else.


% Here, even if traces are deactivated, a trace aggregator is created, as
% actors expect to find one at start-up.
% However no trace supervisor is needed here.

-define( test_start,
	% Create first, synchronously (to avoid race conditions), a trace
	% aggregator (false is to specify a non-private i.e. global aggregator).
	% Goes back to the beginning of line:
	io:format( "~n" ),
	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
	  ?TraceFilename, ?TraceType, ?TraceTitle, _TraceIsPrivate=false,
	  _TraceIsBatch=true ),
	?test_info_fmt( "Testing module(s) ~w.", [ ?Tested_modules ] )
).



-define( test_stop,
	?test_info_fmt( "End of test for module(s) ~w.", [ ?Tested_modules ] ),
	TraceAggregatorPid ! {synchronous_delete,self()},
	receive

		{deleted,TraceAggregatorPid} ->
			ok

	end,
	check_pending_wooper_results(),
	test_finished()
).



-define( test_stop_without_waiting_for_trace_supervisor,
	?test_info_fmt( "End of test for module(s) ~w.", [ ?Tested_modules ] ),
	TraceAggregatorPid ! {synchronous_delete,self()},
	receive

		{deleted,TraceAggregatorPid} ->
			ok

	end,
	check_pending_wooper_results(),
	test_finished()
).


-endif.



% Helper function to write receive clauses in simulation cases (ex: tests) which
% cannot interfere with trace supervision.
%
% Returns the received value.
%
% Ex: Pid ! {getBaz,[],self()}, MyBaz = test_receive(), ...
%
test_receive() ->
	receive
		{wooper_result,V} when V /= monitor_ok ->
			V
	end.



% Handles a test failure.
test_failed(Reason) ->
	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable list.
	Message = io_lib:format( "Test failed for module(s) ~w, reason: ~s.~n",
		[ ?Tested_modules, Reason ] ),
	error_logger:error_msg( Message ),
	?test_fatal( Message ),
	% Needed, otherwise error_logger will not display anything:
	timer:sleep(500),
	erlang:error( "Test ~s failed.", [ ?MODULE ]).




% Displays and flushes all remaining WOOPER results.
% Defines here, since uses a trace.
check_pending_wooper_results() ->
	receive

		{wooper_result,AResult} ->
			?test_info_fmt( "Following WOOPER result was unread: ~w.~n",
						   [AResult] ),
			check_pending_wooper_results()

	after

		0 ->
			ok

	end.

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


% Extension to be used for trace file names:
-define( TraceExtension, ".traces" ).


% Per-test trace file (must be defined before the TraceSupervisor include):
-define( TraceFilename, ( atom_to_list(?MODULE) ++ ?TraceExtension ) ).



% Defines the type of requested execution traces.

% The trace type can be either:
%
% - log_mx_traces, for LogMX-compliant traces (the default): then the trace
% aggregator will use a proper encoding so that the Ceylan Java trace parser,
% plugged into LogMX, allows this tool to be used with Ceylan
%
% - {text_traces,TraceTextOutputType} for more basic text-based traces: then the
% trace aggregator will do its best to format the traces as a human-readable
% trace text file; this is mostly useful when LogMX cannot be used for any
% reason, like needing to generate a report; TraceTextOutputType can be:
%
%   - 'text_only', if wanting to have traces be directly written to disk as pure
%   yet human-readable text
%
%   - 'pdf', if wanting to read finally the traces in a generated PDF file
%
% Note:
%
% - if you change (ex: comment/uncomment) the trace type, then you must
% recompile your modules to take it into account
%
% - check in the class_TraceEmitter.hrl file whether TracingActivated is defined
%
-ifndef(TraceType).
	-define(TraceType,log_mx_traces).
	%-define(TraceType,{text_traces,pdf}).
	%-define(TraceType,{text_traces,text_only}).

-endif.



% Defines the trace title (ex: for PDF output), if not already specified:
-ifndef(TraceTitle).
	-define(TraceTitle,"Ceylan").
-endif.



% For supervisor macros (ex: init_trace_supervisor):
-include("class_TraceSupervisor.hrl").


% For TracingActivated:
-include("class_TraceEmitter.hrl").


% Defines some macros to emit standalone traces, i.e. not from a TraceEmitter,
% and not for test purpose (ex: when writing classical, non-OOP, code).
%
% Note: using 'notify' instead of 'send' to prevent name clashes.

% Usage: '?notify_debug( "Starting!" )'



-ifdef(TracingActivated).

-define( app_start,
	% Create first, synchronously (to avoid race conditions), a trace aggregator
	% (false is to specify a non-private i.e. global aggregator).
	%
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be already created at start-up).
	%
	% Goes back to the beginning of line:
	io:format( "~n" ),
	AppIsBatch = case init:get_argument('-batch') of
		{ok,_} -> true; _ -> false
	end,
	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		?TraceFilename, ?TraceType, ?TraceTitle, _TraceIsPrivate=false,
		TestIsBatch ),
	% Defined in class_TraceSupervisor.hrl:
	?init_trace_supervisor
).


-define( app_stop,
	% Defined in class_TraceSupervisor.hrl:
	?wait_for_any_trace_supervisor,
	TraceAggregatorPid ! {synchronous_delete,self()},
	receive

		{deleted,TraceAggregatorPid} ->
			ok

	end,
	check_pending_wooper_results(),
	app_finished()
).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Fatal, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal(Message),
		 class_TraceEmitter:send_standalone( fatal, Message ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_standalone( fatal, Message,
											 MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send_standalone( fatal, Message,
											 MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Subsection for Fatal, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( fatal,
							  io_lib:format(Message,FormatValues) ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_standalone( fatal,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_fatal_fmt_full( Message, FormatValues, MessageCategorization,
								Tick ),
		 class_TraceEmitter:send_standalone( fatal,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Error, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_error(Message),
		 class_TraceEmitter:send_standalone( error, Message ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_error_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_standalone( error, Message,
											 MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_error_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send_standalone( error, Message,
											 MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Subsection for Error, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_error_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( error,
							  io_lib:format(Message,FormatValues) ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_error_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_standalone( error,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_error_fmt_full( Message, FormatValues, MessageCategorization,
								Tick ),
		 class_TraceEmitter:send_standalone( error,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Warning, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_warning(Message),
		 class_TraceEmitter:send_standalone( warning, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_warning_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_standalone( warning, Message,
											 MessageCategorization )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_warning_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send_standalone( warning, Message,
											 MessageCategorization, Tick )
).




% Subsection for Warning, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_warning_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( warning,
							  io_lib:format(Message,FormatValues) )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_warning_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_standalone( warning,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_warning_fmt_full( Message, FormatValues, MessageCategorization,
								Tick ),
		 class_TraceEmitter:send_standalone( warning,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Info, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_info(Message),
		 class_TraceEmitter:send_standalone( info, Message )
).


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_info_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_standalone( info, Message,
											 MessageCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_info_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send_standalone( info, Message,
											 MessageCategorization, Tick )
).




% Subsection for Info, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_info_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( info,
							  io_lib:format(Message,FormatValues) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_info_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_standalone( info,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_info_fmt_full( Message, FormatValues, MessageCategorization,
								Tick ),
		 class_TraceEmitter:send_standalone( info,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Trace, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_trace(Message),
		 class_TraceEmitter:send_standalone( trace, Message )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_trace_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_standalone( trace, Message,
											 MessageCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_trace_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send_standalone( trace, Message,
											 MessageCategorization, Tick )
).


% Subsection for Trace, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_trace_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( trace,
							  io_lib:format(Message,FormatValues) )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_trace_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_standalone( trace,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_trace_fmt_full( Message, FormatValues, MessageCategorization,
								Tick ),
		 class_TraceEmitter:send_standalone( trace,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Debug, without formatting.


% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
-define( notify_debug(Message),
		 class_TraceEmitter:send_standalone( debug, Message )
).



% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
-define( notify_debug_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send_standalone( debug, Message,
											 MessageCategorization )
).



% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
-define( notify_debug_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send_standalone( debug, Message,
											 MessageCategorization, Tick )
).



% Subsection for Debug, with formatting.


% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
-define( notify_debug_fmt( Message, FormatValues ),
		 class_TraceEmitter:send_standalone( debug,
							  io_lib:format(Message,FormatValues) )
).



% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
-define( notify_debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send_standalone( debug,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).


% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
-define( notify_debug_fmt_full( Message, FormatValues, MessageCategorization,
								Tick ),
		 class_TraceEmitter:send_standalone( debug,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).













-else.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traces are disabled here.
% This 'else' branch will be used iff TracingActivated is not defined above.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_fatal(Message),
		 ?send_fatal( undefined, Message )
).


-define( notify_fatal_cat( Message, MessageCategorization ),
		 ?send_fatal_cat( undefined, Message, MessageCategorization )
).


-define( notify_fatal_full( Message, MessageCategorization, Tick ),
		 ?send_fatal_full( State, Message, MessageCategorization, Tick )
).



-define( notify_fatal_fmt( Message, FormatValues ),
		 ?send_fatal_fmt( undefined, Message, FormatValues )
).


-define( notify_fatal_fmt_cat( Message, FormatValues, MessageCategorization ),
		 ?send_fatal_fmt_cat( undefined, Message, FormatValues,
		 					  MessageCategorization )
).


-define( notify_fatal_fmt_full( Message, FormatValues, MessageCategorization,
		 Tick ),
		 ?send_fatal_fmt_full( undefined, Message, FormatValues,
		 					   MessageCategorization, Tick )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_error(Message),
		 ?send_error( undefined, Message )
).


-define( notify_error_cat( Message, MessageCategorization ),
		 ?send_error_cat( undefined, Message, MessageCategorization )
).


-define( notify_error_full( Message, MessageCategorization, Tick ),
		 ?send_error_full( State, Message, MessageCategorization, Tick )
).



-define( notify_error_fmt( Message, FormatValues ),
		 ?send_error_fmt( undefined, Message, FormatValues )
).


-define( notify_error_fmt_cat( Message, FormatValues, MessageCategorization ),
		 ?send_error_fmt_cat( undefined, Message, FormatValues,
		 					  MessageCategorization )
).


-define( notify_error_fmt_full( Message, FormatValues, MessageCategorization,
		 Tick ),
		 ?send_error_fmt_full( undefined, Message, FormatValues,
		 					   MessageCategorization, Tick )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_warning(Message),
		 trace_disabled( Message )
).


-define( notify_warning_cat( Message, MessageCategorization ),
		 trace_disabled( Message, MessageCategorization )
).


-define( notify_warning_full( Message, MessageCategorization, Tick ),
		 trace_disabled( Message, MessageCategorization, Tick )
).



-define( notify_warning_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_warning_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( Message, FormatValues, MessageCategorization )
).


-define( notify_warning_fmt_full( Message, FormatValues, MessageCategorization,
		 		 Tick ),
		 trace_disabled( Message, FormatValues, MessageCategorization, Tick )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_info(Message),
		 trace_disabled( Message )
).


-define( notify_info_cat( Message, MessageCategorization ),
		 trace_disabled( Message, MessageCategorization )
).


-define( notify_info_full( Message, MessageCategorization, Tick ),
		 trace_disabled( Message, MessageCategorization, Tick )
).



-define( notify_info_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_info_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( Message, FormatValues, MessageCategorization )
).


-define( notify_info_fmt_full( Message, FormatValues, MessageCategorization,
		 		 Tick ),
		 trace_disabled( Message, FormatValues, MessageCategorization, Tick )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_trace(Message),
		 trace_disabled( Message )
).


-define( notify_trace_cat( Message, MessageCategorization ),
		 trace_disabled( Message, MessageCategorization )
).


-define( notify_trace_full( Message, MessageCategorization, Tick ),
		 trace_disabled( Message, MessageCategorization, Tick )
).



-define( notify_trace_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_trace_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( Message, FormatValues, MessageCategorization )
).


-define( notify_trace_fmt_full( Message, FormatValues, MessageCategorization,
		 		 Tick ),
		 trace_disabled( Message, FormatValues, MessageCategorization, Tick )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_debug(Message),
		 trace_disabled( Message )
).


-define( notify_debug_cat( Message, MessageCategorization ),
		 trace_disabled( Message, MessageCategorization )
).


-define( notify_debug_full( Message, MessageCategorization, Tick ),
		 trace_disabled( Message, MessageCategorization, Tick )
).



-define( notify_debug_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( Message, FormatValues, MessageCategorization )
).


-define( notify_debug_fmt_full( Message, FormatValues, MessageCategorization,
		 		 Tick ),
		 trace_disabled( Message, FormatValues, MessageCategorization, Tick )
).


-endif.

% End of the TracingActivated branch.

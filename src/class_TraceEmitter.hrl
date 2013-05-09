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


% This header centralizes notably all macros related to the sending of traces
% from a class_TraceEmitter.

% See also traces.hrl, for the standalone sending of traces.


% Conventions section.


% Technical identifier will be the PID of the trace emitter.

% Name will be the name specified at the creation of the trace emitter.

% EmitterCategorization will the one specified at the creation of the trace
% emitter. It could be deduced from superclasses as well, although it would be
% generally uselessly long and would cause issues in case of multiple
% inheritance.

% Execution timestamp (tick) will be either specified in the send macro (a long
% integer or 'none'), or set to following default execution timestamp:
-define( DefaultExecutionTimestamp, "unknown" ).



% User timestamp will be the current date, as determined by the trace emitter.

% Location will be the current Erlang node, as determined by the trace emitter.

% MessageCategorization will be either specified in the send macro, or
% set the default categorization.

% Following built-in message categorizations are available:

-define( system, "System" ).

	-define( management, ?system".Management" ).

		-define( application_start,    ?management".ExecutionStart" ).
		-define( application_resume,   ?management".ExecutionResume" ).
		-define( application_continue, ?management".ExecutionContinue" ).
		-define( application_stop,     ?management".ExecutionStop" ).
		-define( application_save,     ?management".ExecutionSave" ).
		-define( application_load,     ?management".ExecutionLoad" ).

	-define( time,      ?system".Time" ).
	-define( random,    ?system".Random" ).
	-define( lifecycle, ?system".Lifecycle" ).


-define( execution, "Execution" ).

	-define( update, ?execution".Update" ).
	-define( state,  ?execution".State" ).

-define( DefaultMessageCategorization, ?execution".Uncategorized" ).


% Priority will be determined from the name of the chosen macro: fatal, error,
% warning, info, trace or debug.

% Message will be directly specified in the macro call.


-ifndef(TraceEmitterCategorization).
	-define(TraceEmitterCategorization,"NotCategorized").
-endif.



% Section dedicated to trace emitters that are not WOOPER-based and dedicated
% to tests.
% See also: test_constructs.hrl.
-define(DefaultTestMessageCategorization,"Test.Uncategorized").


% Section dedicated to trace emitters that are not WOOPER-based and dedicated
% to classical functions (as opposed to methods from class_TraceEmitter).
% See also: traces.hrl.
-define(DefaultStandaloneMessageCategorization,"Standalone.Uncategorized").





% Comment the next line if wanting to disable the trace output:
-define(TracingActivated,).



-ifdef(TracingActivated).


% The type of trace output (ex: LogMX, PDF, etc.) is defined in the traces.hrl
% file.


% The first version of macros uses an explicit state.
%
% The second version of macros uses an implicit state, named 'State', as, except
% in constructors, WOOPER conventions imply such a state exists and, provided
% informations stored in state have not changed (notably emitter name and
% categorization), the initial state declared in a method can be used instead of
% any newer one.



% Selecting a macro implies selecting a level of priority.

% An issue is that the definition of Erlang macros does not take into account
% arities, thus LOG(X) and LOG(X,Y) cannot be defined without a name clash.
% This explains why the trace informations have to be specified between brakets:
% using LOG([X,Y]) instead of LOG(X,Y).
% Anonymous functions could be used as well.
% See http://www.nabble.com/question%3A-macro-definition-tt14840873.html

% For each type of trace (fatal, error, warning, etc.), a few variations of the
% set of specified trace informations are supported.


% Previous (deprecate, not supported anymore) mode of operation.

% Previously, for a given type of trace K (ex: K might be 'fatal', or 'error',
% etc.), we could use three variations of the trace sending, knowing that the
% send_K macro requires a state to be specified and the K macro passes
% implicitly a state called 'State':
%
% Three trace informations:
%   ?send_K([ MyState,Message,MessageCategorization,Tick ])
%   Example: ?send_warning([ MyState,"This is my message", ?application_load,
% 132455 ])
%
%   ?K([Message,MessageCategorization,Tick])
%   Example: ?warning([ "This is my message", ?application_load, 132455 ])
%
% Two trace informations: tick is laking, will be added if found in emitter,
% (could be a real tick or 'none' is execution not started), otherwise 'unknown'
% will be used.
%   ?send_K([ MyState,Message,MessageCategorization ])
%   Example: ?send_warning([ MyState,"This is my message", ?application_load ])
%
%   ?K([Message,MessageCategorization])
%   Example: ?warning([ "This is my message", ?application_load ])
%
% One trace information: tick is laking, will be added if found in emitter,
% (could be a real tick or 'none' is execution not started), otherwise 'unknown'
% will be used; MessageCategorization is lacking too,
% DefaultMessageCategorization will be used instead.
%
%   ?send_K([MyState,Message])
%   Example: ?send_warning([ MyState,"This is my message" ])
%
%   ?K([Message])
%   Example: ?warning([ "This is my message" ]) %



% New mode of operation.

% The possibility of specifying in the macro call either 1, 2 or 3 arguments was
% seldom used. Instead we prefered defining more compact forms. Taking 'trace'
% as an example:
%
%  - '?trace([ "Hello" ])' becomes '?trace( "Hello" )': no more internal list
%  - '?trace([ "Hello", "My Category" ])' becomes
% '?trace_cat( "Hello", "My Category" )' ('cat' stands for 'categorized')
%  - '?trace([ "Hello", "My Category", 125 ])' becomes
% '?trace_full( "Hello", "My Category", 125 )'
%
% Similarly, the use of io_lib:format involved too much typing, so we defined
% shorter forms instead. Taking 'trace' again as an example:
%
%  - '?trace([ io_lib:format( "Hello ~w.", [V] ) ])' becomes
% '?trace_fmt( "Hello ~w.", [V] )' (most frequently used form; 'fmt' stands for
% 'format')
%  - '?trace([ io_lib:format( "Hello ~w.", [V] ), "My Category" ])' becomes
% '?trace_fmt_cat( "Hello ~w.", [V], "My Category" )'
%  - '?trace([ io_lib:format( "Hello ~w.", [V] ), "My Category", 125 ])'
% becomes '?trace_fmt_full( "Hello ~w.", [V], "My Category", 125 )'
%
% (knowing we cannot define macros with same name but different arity)



% Some delay are added when error traces are sent, so that they can be stored
% before the virtual machine is stopped, should it happen (ex: if an exception
% is thrown).
% Delays should better be replaced by synchronous operations.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Fatal, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal( State, Message ),
		 class_TraceEmitter:send( fatal, State, Message ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal(Message),
		 class_TraceEmitter:send( fatal, State, Message ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( fatal, State, Message,
								  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( fatal, State, Message,
								  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_full( State, Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( fatal, State, Message,
								  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( fatal, State, Message,
								  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Subsection for Fatal, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( fatal, State,
							  io_lib:format(Message,FormatValues) ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( fatal, State,
							  io_lib:format(Message,FormatValues) ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send( fatal, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( fatal, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
-define( send_fatal_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),
		 class_TraceEmitter:send( fatal, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).



% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
-define( fatal_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 class_TraceEmitter:send( fatal, State,
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


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error( State, Message ),
		 class_TraceEmitter:send( error, State, Message ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
-define( error(Message),
		 class_TraceEmitter:send( error, State, Message ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( error, State, Message,
								  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
-define( error_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( error, State, Message,
								  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_full( State, Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( error, State, Message,
								  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
-define( error_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( error, State, Message,
								  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).




% Subsection for Error, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( error, State,
							  io_lib:format(Message,FormatValues) ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
-define( error_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( error, State,
							  io_lib:format(Message,FormatValues) ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send( error, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
-define( error_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( error, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
-define( send_error_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),
		 class_TraceEmitter:send( error, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick ),
		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 timer:sleep(100)
).


% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
-define( error_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 class_TraceEmitter:send( error, State,
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


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning( State, Message ),
		 class_TraceEmitter:send( warning, State, Message )
).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
-define( warning(Message),
		 class_TraceEmitter:send( warning, State, Message )

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( warning, State, Message,
								  MessageCategorization )
).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
-define( warning_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( warning, State, Message,
								  MessageCategorization )
).




% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning_full( State, Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( warning, State, Message,
								  MessageCategorization, Tick )
).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
-define( warning_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( warning, State, Message,
								  MessageCategorization, Tick )
).








% Subsection for Warning, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( warning, State,
							  io_lib:format(Message,FormatValues) )
).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
-define( warning_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( warning, State,
							  io_lib:format(Message,FormatValues) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send( warning, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
-define( warning_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( warning, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
-define( send_warning_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),
		 class_TraceEmitter:send( warning, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).


% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
-define( warning_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 class_TraceEmitter:send( warning, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
 ).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Info, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
-define( send_info( State, Message ),
		 class_TraceEmitter:send( info, State, Message )
).


% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
-define( info(Message),
		 class_TraceEmitter:send( info, State, Message )

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
-define( send_info_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization )
).


% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
-define( info_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization )
).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
-define( send_info_full( State, Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization, Tick )
).


% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
-define( info_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( info, State, Message,
								  MessageCategorization, Tick )
).






% Subsection for Info, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
-define( send_info_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( info, State,
							  io_lib:format(Message,FormatValues) )
).


% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
-define( info_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( info, State,
							  io_lib:format(Message,FormatValues) )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
-define( send_info_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send( info, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).


% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( info, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).






% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
-define( send_info_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),
		 class_TraceEmitter:send( info, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).


% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
-define( info_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 class_TraceEmitter:send( info, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
 ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Trace, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit
% state.
-define( send_trace( State, Message ),
		 class_TraceEmitter:send( trace, State, Message )
).


% Sends a trace of 'trace' type with specified parameters and implicit use of
% a variable named 'State'.
-define( trace(Message),
		 class_TraceEmitter:send( trace, State, Message )

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit
% state.
-define( send_trace_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( trace, State, Message,
								  MessageCategorization )
).


% Sends a trace of 'trace' type with specified parameters and implicit use of
% a variable named 'State'.
-define( trace_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( trace, State, Message,
								  MessageCategorization )
).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit
% state.
-define( send_trace_full( State, Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( trace, State, Message,
								  MessageCategorization, Tick )
).


% Sends a trace of 'trace' type with specified parameters and implicit use of
% a variable named 'State'.
-define( trace_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( trace, State, Message,
								  MessageCategorization, Tick )
).








% Subsection for Trace, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit
% state.
-define( send_trace_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( trace, State,
							  io_lib:format(Message,FormatValues) )
).


% Sends a trace of 'trace' type with specified parameters and implicit use of
% a variable named 'State'.
-define( trace_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( trace, State,
							  io_lib:format(Message,FormatValues) )
).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit
% state.
-define( send_trace_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send( trace, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).


% Sends a trace of 'trace' type with specified parameters and implicit use of
% a variable named 'State'.
-define( trace_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( trace, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit
% state.
-define( send_trace_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),
		 class_TraceEmitter:send( trace, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).


% Sends a trace of 'trace' type with specified parameters and implicit use of
% a variable named 'State'.
-define( trace_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 class_TraceEmitter:send( trace, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
 ).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Debug, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit
% state.
-define( send_debug( State, Message ),
		 class_TraceEmitter:send( debug, State, Message )
).


% Sends a debug of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
-define( debug(Message),
		 class_TraceEmitter:send( debug, State, Message )

).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit
% state.
-define( send_debug_cat( State, Message, MessageCategorization ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization )
).


% Sends a debug of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
-define( debug_cat( Message, MessageCategorization ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization )
).






% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit
% state.
-define( send_debug_full( State, Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization, Tick )
).


% Sends a debug of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
-define( debug_full( Message, MessageCategorization, Tick ),
		 class_TraceEmitter:send( debug, State, Message,
								  MessageCategorization, Tick )
).





% Subsection for Debug, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit
% state.
-define( send_debug_fmt( State, Message, FormatValues ),
		 class_TraceEmitter:send( debug, State,
							  io_lib:format(Message,FormatValues) )
).


% Sends a debug of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
-define( debug_fmt( Message, FormatValues ),
		 class_TraceEmitter:send( debug, State,
							  io_lib:format(Message,FormatValues) )
).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit
% state.
-define( send_debug_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 class_TraceEmitter:send( debug, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).


% Sends a debug of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
-define( debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 class_TraceEmitter:send( debug, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization )
).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit
% state.
-define( send_debug_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),
		 class_TraceEmitter:send( debug, State,
							  io_lib:format(Message,FormatValues),
							  MessageCategorization, Tick )
).


% Sends a debug of 'debug' type with specified parameters and implicit use of
% a variable named 'State'.
-define( debug_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 class_TraceEmitter:send( debug, State,
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



% trace_disabled functions are defined to avoid warnings about variables not
% being used.

% Hopefully they will be inlined, and then optimized out as a whole by the
% compiler.

% If not using the trace_disabled functions, deactivating the traces will result
% in variables in classes possibly being declared unused (warning).
%
% Using these functions will cause another problem: if not all macro arities are
% used in that class, the remaining trace_disabled functions will be declared
% themselves as unused.
%
% Exporting them is not a solution, as WOOPER defined already some functions,
% thus no additional exports can be made. And the trace emitter include and the
% WOOPER one cannot be permuted (as this header defines functions as well).
%
% Specifying the parameters 'as are' instead of wrapping them in a
% trace_disabled function (ex: 'State, Message' instead of
% 'trace_disabled(State,Message)' results in the following warning:
% 'Warning: a term is constructed, but never used'



% Used to be exported (otherwise will be themselves determined 'unused'),
% however as explained above could not mix with WOOPER exports:
%-export([ trace_disabled/1, trace_disabled/2, trace_disabled/3,
%trace_disabled/4 ]).

% Forced inlining so that trace_disabled functions are optimized out.
%
% It was finally commented out, as it triggered for each trace macro:
% "Warning: a term is constructed, but never used" when traces were deactivated.
% We believe that nonetheless these local do-nothing functions will be optimized
% out by the compiler.
%-compile( {inline,[ trace_disabled/1, trace_disabled/2, trace_disabled/3,
%				   trace_disabled/4, trace_disabled/5 ] } ).


trace_disabled( _ ) ->
 	trace_disabled.


trace_disabled( _, _ ) ->
	trace_disabled.


trace_disabled( _, _, _ ) ->
	trace_disabled.


trace_disabled( _, _, _, _ ) ->
 	trace_disabled.


trace_disabled( _, _, _, _, _ ) ->
 	trace_disabled.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Fatal, without formatting.


% Most important trace categories cannot be disabled:

-define( send_fatal( State, Message ),
		io:format( "Fatal trace message (although traces are disabled): ~s.~n",
				   [Message] ),
		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		timer:sleep(100),
		trace_disabled( State )
).


-define( fatal(Message),
		 ?send_fatal( State, Message )
).





-define( send_fatal_cat( State, Message, MessageCategorization ),
		 ?send_fatal( State, Message ++ "(" ++ MessageCategorization ++ ")" )
).


-define( fatal_cat( Message, MessageCategorization ),
		 ?send_fatal_cat( State, Message, MessageCategorization )
).





-define( send_fatal_full( State, Message, MessageCategorization, Tick ),
		 ?send_fatal( State, Message ++ "(" ++ MessageCategorization
		 			 ++ io_lib:format( ") at tick ~w", [Tick] ) )
).


-define( fatal_full( Message, MessageCategorization, Tick ),
		 ?send_fatal_full( State, Message, MessageCategorization, Tick )
).





% Subsection for Fatal, with formatting.


-define( send_fatal_fmt( State, Message, FormatValues ),
		 ?send_fatal( State, io_lib:format(Message,FormatValues) )
).


-define( fatal_fmt( Message, FormatValues ),
		 ?send_fatal_fmt( State, Message, FormatValues )
).





-define( send_fatal_fmt_cat( State, Message, FormatValues,
		 					 MessageCategorization ),
		 ?send_fatal( State, io_lib:format(Message,FormatValues)
		 			 ++ "(" ++ MessageCategorization ++ ")" )
).


-define( fatal_fmt_cat( Message, FormatValues, MessageCategorization ),
		 ?send_fatal_fmt_cat( State, Message, FormatValues,
		 					  MessageCategorization )
).





-define( send_fatal_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		 ?send_fatal( State, io_lib:format(Message,FormatValues)
		 			 ++ "(" ++ MessageCategorization
					 ++ io_lib:format( ") at tick ~w", [Tick] ) )
).


-define( fatal_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 ?send_fatal_fmt_full( State, Message, FormatValues,
		 					   MessageCategorization, Tick )
).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Error, without formatting.


% Most important trace categories cannot be disabled:

-define( send_error( State, Message ),
		io:format( "Error trace message (although traces are disabled): ~s.~n",
				   [Message] ),
		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		timer:sleep(100),
		trace_disabled( State )
).


-define( error(Message),
		?send_error( State, Message )
).





-define( send_error_cat( State, Message, MessageCategorization ),
		?send_error( State, Message ++ "(" ++ MessageCategorization ++ ")" )
).


-define( error_cat( Message, MessageCategorization ),
		?send_error_cat( State, Message, MessageCategorization )
).




-define( send_error_full( State, Message, MessageCategorization, Tick ),
		?send_error( State, Message ++ "(" ++ MessageCategorization
					++ io_lib:format( ") at tick ~w", [Tick] ) )
).


-define( error_full( Message, MessageCategorization, Tick ),
		?send_error_full( State, Message, MessageCategorization, Tick )
).








% Subsection for Error, with formatting.


-define( send_error_fmt( State, Message, FormatValues ),
		 ?send_error( State, io_lib:format(Message,FormatValues) )
).


-define( error_fmt( Message, FormatValues ),
		 ?send_error_fmt( State, Message, FormatValues )
).




-define( send_error_fmt_cat( State, Message, FormatValues,
		 					 MessageCategorization ),
		 ?send_error( State, io_lib:format(Message,FormatValues)
		 			 ++ "(" ++ MessageCategorization ++ ")" )
).


-define( error_fmt_cat( Message, FormatValues, MessageCategorization ),
		 ?send_error_fmt_cat( State, Message, FormatValues,
		 					  MessageCategorization )
).








-define( send_error_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		 ?send_error( State, io_lib:format(Message,FormatValues)
		 			 ++ "(" ++ MessageCategorization
					 ++ io_lib:format( ") at tick ~w", [Tick] ) )
).


-define( error_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 ?send_error_fmt_full( State, Message, FormatValues,
		 					   MessageCategorization, Tick )
).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Warning, without formatting.



-define( send_warning( State, Message ),
		trace_disabled( State, Message )
).


-define( warning(Message),
		trace_disabled( State, Message )
).




-define( send_warning_cat( State, Message, MessageCategorization ),
		trace_disabled( State, Message, MessageCategorization )
).


-define( warning_cat( Message, MessageCategorization ),
		trace_disabled( State, Message, MessageCategorization )
).




-define( send_warning_full( State, Message, MessageCategorization, Tick ),
		trace_disabled( State, Message, MessageCategorization, Tick )
).


-define( warning_full( Message, MessageCategorization, Tick ),
		trace_disabled( State, Message, MessageCategorization, Tick )
).





% Subsection for Warning, with formatting.


-define( send_warning_fmt( State, Message, FormatValues ),
		trace_disabled( State, Message, FormatValues )
).


-define( warning_fmt( Message, FormatValues ),
		trace_disabled( State, Message, FormatValues )
).



-define( send_warning_fmt_cat( State, Message, FormatValues,
		 					 MessageCategorization ),
		trace_disabled( State, Message, FormatValues,
		 					 MessageCategorization )
).


-define( warning_fmt_cat( Message, FormatValues, MessageCategorization ),
		trace_disabled( State, Message, FormatValues, MessageCategorization )
).





-define( send_warning_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		trace_disabled( State, Message, FormatValues,
						  MessageCategorization, Tick )
).


-define( warning_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		trace_disabled( State, Message, FormatValues, MessageCategorization,
					   Tick )
).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Info, without formatting.


% Most important trace categories cannot be disabled:

-define( send_info( State, Message ),
		trace_disabled( State, Message )
).


-define( info(Message),
		trace_disabled( State, Message )
).






-define( send_info_cat( State, Message, MessageCategorization ),
		trace_disabled( State, Message, MessageCategorization )
).


-define( info_cat( Message, MessageCategorization ),
		trace_disabled( State, Message, MessageCategorization )
).




-define( send_info_full( State, Message, MessageCategorization, Tick ),
		trace_disabled( State, Message, MessageCategorization, Tick )
).


-define( info_full( Message, MessageCategorization, Tick ),
		trace_disabled( State, Message, MessageCategorization, Tick )
).






% Subsection for Info, with formatting.


-define( send_info_fmt( State, Message, FormatValues ),
		trace_disabled( State, Message, FormatValues )
).


-define( info_fmt( Message, FormatValues ),
		trace_disabled( State, Message, FormatValues )
).





-define( send_info_fmt_cat( State, Message, FormatValues,
		 					 MessageCategorization ),
		trace_disabled( State, Message, FormatValues, MessageCategorization )
).


-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),
		trace_disabled( State, Message, FormatValues, MessageCategorization )

).






-define( send_info_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		trace_disabled( State, Message, FormatValues,
						  MessageCategorization, Tick )
).


-define( info_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization, Tick )
).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Trace, without formatting.


% Most important trace categories cannot be disabled:

-define( send_trace( State, Message ),
		 trace_disabled( State, Message )
).


-define( trace(Message),
		 trace_disabled( State, Message )
).





-define( send_trace_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( trace_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).





-define( send_trace_full( State, Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).


-define( trace_full( Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).






% Subsection for Trace, with formatting.


-define( send_trace_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).


-define( trace_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).



-define( send_trace_fmt_cat( State, Message, FormatValues,
		 					 MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
		 					 MessageCategorization )
).


-define( trace_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).



-define( send_trace_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues,
						  MessageCategorization, Tick )
).


-define( trace_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization, Tick )
).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Debug, without formatting.


% Most important debug categories cannot be disabled:

-define( send_debug( State, Message ),
		 trace_disabled( State, Message )
).


-define( debug(Message),
		 trace_disabled( State, Message )
).






-define( send_debug_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( debug_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).





-define( send_debug_full( State, Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).


-define( debug_full( Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).








% Subsection for Debug, with formatting.


-define( send_debug_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).


-define( debug_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).





-define( send_debug_fmt_cat( State, Message, FormatValues,
		 					 MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
		 					 MessageCategorization )
).


-define( debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).






-define( send_debug_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues,
						  MessageCategorization, Tick )
).


-define( debug_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization, Tick )
).



-endif.


% End of the TracingActivated branch.

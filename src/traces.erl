% Copyright (C) 2003-2019 Olivier Boudeville
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
% Creation date: July 1, 2007.


% This module gathers all code, common to tests and applications, that allows to
% lighten the trace macros or to share defines and types.
%
-module(traces).


-export([ get_trace_filename/1,
		  receive_applicative_message/0, receive_applicative_message/1,
		  check_pending_wooper_results/0, declare_beam_dirs_for_traces/0 ]).


-type emitter_name() :: text_utils:ustring().
-type emitter_categorization() :: text_utils:ustring().

-type emitter_info() :: { emitter_name(), emitter_categorization() }.


% A trace timestamp can be anything (ex: integer() | 'none'), no constraint
% applies on purpose, so that any kind of application-specific timestamps can be
% elected.
%
-type app_timestamp() :: any().

-type time() :: text_utils:ustring().

-type location() :: text_utils:ustring().


% A message may or may not (which is the default) by categorized:
-type message_categorization() :: text_utils:ustring() | 'uncategorized'.

-type priority() :: 1..6.

-type message() :: text_utils:ustring().


-type message_type() :: trace_utils:trace_severity().



% Text traces are either in pure, raw text, or in PDF:
-type trace_text_type() :: 'text_only' | 'pdf'.



% A trace type must be selected so that, when the traces are aggregated, the
% corresponding output is compliant with the tools to be used for supervision.
%
% So the trace type to select depends on whether a dedicated, advanced trace
% tool should be used to browse the execution traces, or just a text viewer
% (possibly with a PDF displaying thereof); indeed it is:
%
% - either 'advanced_traces', for traces typically expected to be read from the
% LogMX tool (relying then on our parser); see http://logmx.com/
%
% - or { 'text_traces', trace_text_type() }
%
-type trace_supervision_type() :: 'advanced_traces'
								| { 'text_traces', trace_text_type() }.


-export_type([ emitter_name/0, emitter_categorization/0, emitter_info/0,
			   app_timestamp/0, time/0, location/0, message_categorization/0,
			   priority/0, message/0, message_type/0,
			   trace_supervision_type/0 ]).


% For notify_warning_fmt:
-include("traces.hrl").


% Returns the name of the file in which traces will be written:
-spec get_trace_filename( basic_utils:module_name() ) ->
								file_utils:file_name().
get_trace_filename( ModuleName ) ->
	atom_to_list( ModuleName ) ++ ?TraceExtension.





% Receives an applicative, non-trace message, to protect user messages from the
% trace ones.
%
-spec receive_applicative_message() -> any().
receive_applicative_message() ->

	receive

		{ wooper_result, V } when V /= monitor_ok ->
			V

	end.



% Receives specified applicative, non-trace message, to protect user messages
% from the trace ones.
%
% Used for synchronization purpose.
%
-spec receive_applicative_message( any() ) -> void().
receive_applicative_message( Message=monitor_ok ) ->
	% Would interfere with the monitoring system:
	throw( { invalid_applicative_message, Message } );

receive_applicative_message( Message ) ->
	receive

		{ wooper_result, Message } ->
			message_received

	end.



% Displays and flushes all remaining WOOPER results.
%
% Defined here, since uses a trace.
%
-spec check_pending_wooper_results() -> void().
check_pending_wooper_results() ->

	receive

		{ wooper_result, AResult }  ->

			?notify_warning_fmt( "Following WOOPER result was unread: ~p.~n",
								 [ AResult ] ),

			check_pending_wooper_results()

	after

		0 ->
			ok

	end.



% Declares automatically the relevant BEAM directories in the code path, so that
% Ceylan-Traces can be fully usable from then on.
%
% Note:
%
% - the code_utils.beam module of Ceylan-Myriad and the wooper.beam module of
% Ceylan-WOOPER must be available from the current code path
%
% - the determined directories are not specifically checked for existence,
% and are added at the end of the code path.
%
-spec declare_beam_dirs_for_traces() -> void().
declare_beam_dirs_for_traces() ->
	wooper:declare_beam_dirs_for_wooper(),
	code_utils:declare_beam_dirs_for( "CEYLAN_TRACES" ).

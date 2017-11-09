% Copyright (C) 2003-2017 Olivier Boudeville
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


% This module gathers all code, common to tests and applications, that allows to
% lighten the trace macros or to share defines and types.
%
-module(traces).


-export([ get_trace_filename/1, get_attribute_pairs/1,
		  receive_applicative_message/0, receive_applicative_message/1,
		  check_pending_wooper_results/0 ]).


-type emitter_name() :: string().
-type emitter_categorization() :: string().

-type emitter_info() :: { emitter_name(), emitter_categorization() }.


% A trace timestamp can be anything (ex: integer() | 'none'), no constraint
% applies on purpose, so that any kind of application-specific timestamps can be
% elected.
%
-type app_timestamp() :: any().

-type time() :: string().

-type location() :: string().


% A message may or may not (which is the default) by categorized:
%
-type message_categorization() :: string() | 'uncategorized'.

-type priority() :: 1..6.

-type message() :: string().


-type message_type() :: trace_utils:trace_severity().


% Type of trace supervision:
-type trace_supervision_type() :: 'log_mx_traces'
								| { 'text_traces', 'text_only' | 'pdf' }.


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



% Returns the (user-level) attributes known of Traces for the specified state
% (i.e. all attributes except the ones used internally by Traces and WOOPER).
%
% (helper)
%
-spec get_attribute_pairs( wooper:state() ) -> [ wooper:attribute_entry() ].
get_attribute_pairs( State ) ->

	AllAttrs = wooper:get_all_attributes( State ),

	ReservedAttrs = get_traces_reserved_attribute_names(),

	% Remove Traces internals:
	filter_traces_attributes( AllAttrs, ReservedAttrs, _Acc=[] ).



% Removes from the specified atttributes the ones used internally by TRACES (so
% that only class-specific ones remain).
%
% (internal helper)
%
filter_traces_attributes( _AttrPairs=[], _ReservedAttrs, Acc ) ->
	Acc;

filter_traces_attributes( _AttrPairs=[ AttrEntry={ Name, _Value } | T ],
						  ReservedAttrs, Acc ) ->

	case lists:member( Name, ReservedAttrs ) of

		true ->
			filter_traces_attributes( T, ReservedAttrs, Acc );

		false ->
			filter_traces_attributes( T, ReservedAttrs, [ AttrEntry | Acc ] )

	end.



% Returns a list of the attribute names that are used internally by TRACES.
%
-spec get_traces_reserved_attribute_names() -> [ wooper:attribute_name() ].
get_traces_reserved_attribute_names() ->
	[ emitter_node, name, trace_aggregator_pid, trace_categorization,
	  trace_timestamp ].



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

-spec receive_applicative_message( any() ) -> basic_utils:void().
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
-spec check_pending_wooper_results() -> basic_utils:void().
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

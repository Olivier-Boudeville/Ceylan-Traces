% Copyright (C) 2003-2018 Olivier Boudeville
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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: July 1, 2007.


% Base class for all (WOOPER-based) emitters of traces.
%
% See class_TestTraceEmitter.erl and class_TraceEmitter_test.erl
%
-module(class_TraceEmitter).



% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).



% Parameters taken by the constructor ('construct').
%
% These are class-specific data needing to be set in the constructor: (the
% class-specific trace_emitter_categorization define will be set in the
% trace_categorization attribute of each child class when coming down the
% inheritance hierarchy, so that the latest child class sets its targeted
% trace_categorization value)
%
-define( wooper_construct_parameters, TraceEmitterInit ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
%
-export([ new/1, new_link/1, synchronous_new/1, synchronous_new_link/1,
		  synchronous_timed_new/1, synchronous_timed_new_link/1,
		  remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		  remote_synchronous_new_link/2, remote_synchronous_timed_new/2,
		  remote_synchronous_timed_new_link/2, remote_synchronisable_new_link/2,
		  construct/2 ]).



% Member method declarations:
%
-define( wooper_method_export, getName/1, setName/2, setCategorization/2,
		 display/1, toString/1 ).


% Static method declarations:
%
-define( wooper_static_method_export,
		 send_from_test/2, send_from_test/3,
		 send_from_case/2, send_from_case/3,
		 send_standalone/2, send_standalone/3, send_standalone/4,
		 send_standalone/5,
		 send_standalone_safe/2, send_standalone_safe/3,
		 send_standalone_safe/4, send_standalone_safe/5,
		 get_emitter_node_as_binary/0,
		 get_priority_for/1, get_channel_name_for_priority/1 ).



% Helper functions:
%
-export([ init/1, set_categorization/2,
		  send/3, send_safe/3, send/4, send_safe/4, send/5, send_safe/5,
		  send_synchronised/5,
		  get_trace_timestamp/1, get_trace_timestamp_as_binary/1,
		  get_plain_name/1, sync/1, await_output_completion/0 ]).



% The name of a trace emitter.
%
% It is a plain string or a binary one, containing the name of a trace emitter.
%
% Note: dots are not allowed in an emitter name (they are used as naming
% separator).
%
% Ex: "MyObject 16", or <<"First Talker">>.
%
-type emitter_name() :: string() | text_utils:bin_string().



% The categorization of a trace emitter.
%
% It is a plain string listing increasingly detailed trace sub-categories,
% separated by dots.
%
% Ex: "topics.sports.basketball"
%
-type emitter_categorization() :: string() | text_utils:bin_string().



% Initializing a trace emitter is specifying its name to the constructor of its
% actual class, which will augment that information with the correspond
% class-specific emitter categorization. Then, the pair resulting from this
% one-shot, initial operation will climb up the class hierarchy until reaching
% the class_TraceEmitter constructor.
%
% See also the trace_categorize/1 macro.
%
-type emitter_init() :: emitter_name()
						| { emitter_name(), emitter_categorization() }.


% PID of a trace emitter:
-type emitter_pid() :: wooper:instance_pid().


-export_type([ emitter_name/0, emitter_categorization/0, emitter_init/0,
			   emitter_pid/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For send_from_* and all:
-include("class_TraceAggregator.hrl").


% For trace_aggregator_name:
-include("class_TraceEmitter.hrl").


-define(LogPrefix,"[Trace Emitter]").




% Implementation notes:

% A trace emitter used to have a specific notion of time (execution tick) as it
% needs to timestamp its traces. Now it relies on the content of an opaque
% 'trace_timestamp' attribute, which is stringified and used as it is, allowing
% for mostly any kind of application-level timestamp.


% To reduce the memory footprint in the trace aggregator mailbox and the size of
% messages sent over the network, most of the time binaries are used instead of
% plain strings.
%
% Notably the 'name' attribute is stored as a binary.
%
% Use text_utils:binary_to_string/1 to get back a plain string or,
% preferably, the class_TraceEmitter:get_plain_name/1 static method.
%
% The same applies for the 'trace_categorization' attribute.

% The constructor of this class is idempotent, in the sense that it can be
% applied more than once with no undesirable consequence.


% The send_safe/{3,4,5} variations differ from their basic send/{3,4,5}
% counterparts on two aspects:
%
% - they are synchronous (blocking, hence safer)
% - they are echoed on the console as well



% Attributes of a trace emitter are:
%
% - name :: text_utils:bin_string() is the name of this trace emitter; not named
% trace_name is order to be more versatile
%
% - trace_categorization :: text_utils:bin_string() is the categorization of
% this trace emitter
%
% - trace_timestamp :: app_timestamp() is the current application-specific
% timestamp



% Constructs a new Trace emitter, from:
%
% - EmitterInit must be here a pair made of this name and another plain string
% listing increasingly detailed sub-categories about this trace emitter,
% separated by dots (ex: "topics.sports.basketball.coach")
%
% Note: this constructor should be idempotent, as a given instance might very
% well inherit (directly or not) from that class more than once.
%
-spec construct( wooper:state(), { emitter_name(), emitter_categorization() } )
			   -> wooper:state().
construct( State, _EmitterInit={ EmitterName, EmitterCategorization } ) ->

	%trace_utils:debug_fmt( "~s Creating a trace emitter whose name is '~s', "
	%						"whose PID is ~w and whose categorization is '~s'.",
	%			   [ ?LogPrefix, EmitterName, self(), EmitterCategorization ] ),

	InitState = init( State ),

	BinName = check_and_binarise_name( EmitterName ),

	BinCategorization = text_utils:ensure_binary( EmitterCategorization ),

	setAttributes( InitState, [
		{ name, BinName },
		{ trace_categorization, BinCategorization },
		{ trace_timestamp, undefined } ] );


% Should no mother class have set it:
construct( State, EmitterName ) ->
	construct( State, _EmitterInit={ EmitterName,
									 ?default_trace_emitter_categorization } ).



% Checks the emitter name, and, if needed, returns a binary version thereof.
%
% (helper)
%
-spec check_and_binarise_name( emitter_name() ) -> text_utils:bin_string().
check_and_binarise_name( StringName ) when is_list( StringName ) ->
	check_string_name( StringName ),
	text_utils:string_to_binary( StringName );

check_and_binarise_name( BinName ) when is_binary( BinName ) ->
	StringName = text_utils:binary_to_string( BinName ),
	check_string_name( StringName ),
	BinName.


% Helper:
check_string_name( Name ) ->

	% Can be an io_list():
	FlatName = text_utils:format( "~s", [ Name ] ),

	% No dot allowed, as is used as a naming separator:
	%
	case text_utils:split_at_first( _Marker=$., Name ) of

		none_found ->
			ok;

		_ ->
			throw( { no_dot_allowed_in_emitter_name, FlatName } )

	end.



% Useless:
%
% Overridden destructor.
%
%-spec destruct( wooper:state() ) -> wooper:state().
%destruct( State ) ->

	%trace_utils:debug_fmt( "~s Deleting Trace Emitter.", [ ?LogPrefix ] ),

	%trace_utils:debug_fmt( "~s Trace Emitter deleted.", [ ?LogPrefix ] ).

	%State.




% Methods section.



% Generic interface.


% Returns the name of this trace emitter, as a binary.
%
% Note: use text_utils:binary_to_string/1 to get back a plain string.
%
% (const request)
%
-spec getName( wooper:state() ) -> request_return( text_utils:bin_string() ).
getName( State ) ->
	?wooper_return_state_result( State, ?getAttr(name) ).



% Sets the name of this trace emitter from specified plain string.
%
% (oneway)
%
-spec setName( wooper:state(), emitter_name() ) -> oneway_return().
setName( State, NewName ) ->

	BinName = text_utils:string_to_binary( NewName ),

	?wooper_return_state_only( setAttribute( State, name, BinName ) ).



% Sets the trace categorization for this trace emitter to specified plain
% string.
%
% Setting the trace categorization early in the constructor, before sending any
% trace, allows to have all traces for a given emitter correctly gathered in the
% same trace category, which is a lot clearer when browsing afterwards.
%
% (oneway)
%
-spec setCategorization( wooper:state(), emitter_categorization() ) ->
							   oneway_return().
setCategorization( State, TraceCategorization ) ->

	NewState = set_categorization( TraceCategorization, State ),

	?wooper_return_state_only( NewState ).




% Displays the state in the console.
%
% (const oneway)
%
-spec display( wooper:state() ) -> oneway_return().
display( State ) ->
	wooper:display_instance( State ),
	?wooper_return_state_only( State ).


% Returns a textual description of this emitter.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, wooper:state_to_string( State ) ).




% 'Static' methods (module functions).




% Section for static methods.



% Sends all types of traces on behalf of a test, thus without requiring a
% class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_from_test( traces:message_type(), traces:message() ) -> void().
send_from_test( TraceType, Message ) ->
	send_from_test( TraceType, Message, ?default_test_emitter_categorization ).



% Sends all types of traces on behalf of a test, thus without requiring a
% class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_from_test( traces:message_type(), traces:message(),
					  traces:emitter_categorization() ) -> void().
send_from_test( TraceType, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->

			trace_utils:error( "class_TraceEmitter:send_from_test/3: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
							  time_utils:get_textual_timestamp() ),

			% Not State available here:
			EmitterNode = get_emitter_node_as_binary(),

			AggregatorPid ! { send,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=
					 text_utils:string_to_binary( "test" ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _AppTimestamp=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=
					 text_utils:string_to_binary( "Test" ),
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				] }

	end.





% Sends all types of traces on behalf of a case, thus without requiring a
% class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_from_case( traces:message_type(), traces:message() ) -> void().
send_from_case( TraceType, Message ) ->
	send_from_case( TraceType, Message, ?default_case_emitter_categorization ).



% Sends all types of traces on behalf of a case, thus without requiring a
% class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_from_case( traces:message_type(), traces:message(),
					  traces:emitter_categorization() ) -> void().
send_from_case( TraceType, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->

			trace_utils:error( "class_TraceEmitter:send_from_case: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
							  time_utils:get_textual_timestamp() ),

			% Not State available here:
			EmitterNode = get_emitter_node_as_binary(),

			AggregatorPid ! { send,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=
					 text_utils:string_to_binary( "case" ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _AppTimestamp=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=
					 text_utils:string_to_binary( "Case" ),
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				] }

	end.




% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone( traces:message_type(), traces:message() ) -> void().
send_standalone( TraceType, Message ) ->
	send_standalone( TraceType, Message,
					 ?default_standalone_emitter_categorization ).



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone( traces:message_type(), traces:message(),
					   traces:emitter_categorization() ) -> void().
send_standalone( TraceType, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->

			trace_utils:error( "class_TraceEmitter:send_standalone/3: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
				time_utils:get_textual_timestamp() ),

			% No State available here:
			EmitterNode = get_emitter_node_as_binary(),

			PidName = get_emitter_name_from_pid(),

			MessageCategorization =
				get_default_standalone_message_categorization(),

			AggregatorPid ! { send,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=text_utils:string_to_binary( PidName ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _AppTimestamp=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=
					 text_utils:string_to_binary( MessageCategorization ),
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				] }

	end.



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone( traces:message_type(), traces:message(),
		   traces:emitter_name(), traces:emitter_categorization() ) -> void().
send_standalone( TraceType, Message, EmitterName, EmitterCategorization ) ->
	send_standalone( TraceType, Message, EmitterName, EmitterCategorization,
					 _MessageCategorization=uncategorized ).



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone( traces:message_type(), traces:message(),
					   traces:emitter_name(), traces:emitter_categorization(),
					   traces:message_categorization() ) -> void().
send_standalone( TraceType, Message, EmitterName, EmitterCategorization,
				 MessageCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case global:whereis_name( ?trace_aggregator_name ) of


		undefined ->

			trace_utils:error( "class_TraceEmitter:send_standalone/5: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );


		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
				time_utils:get_textual_timestamp() ),

			% No State available here:
			EmitterNode = get_emitter_node_as_binary(),

			ActualMsgCateg = case MessageCategorization of

				uncategorized ->
					uncategorized;

				Categ ->
					text_utils:string_to_binary( Categ )

			end,

			AggregatorPid ! { send,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=text_utils:string_to_binary( EmitterName ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _AppTimestamp=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=ActualMsgCateg,
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				] }

	end.



% Sends all types of traces without requiring a class_TraceEmitter state, in a
% safe manner (synchronous and echoed on the console).
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone_safe( traces:message_type(), traces:message() ) -> void().
send_standalone_safe( TraceType, Message ) ->

	EmitterCategorization = ?trace_emitter_categorization,

	ApplicationTimestamp = time_utils:get_textual_timestamp(),

	send_standalone_safe( TraceType, Message, EmitterCategorization,
						  ApplicationTimestamp ).



% Sends all types of traces without requiring a class_TraceEmitter state, in a
% safe manner (synchronous and echoed on the console).
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone_safe( traces:message_type(), traces:message(),
							traces:emitter_categorization() ) -> void().
send_standalone_safe( TraceType, Message, EmitterCategorization ) ->

	ApplicationTimestamp = time_utils:get_textual_timestamp(),

	send_standalone_safe( TraceType, Message, EmitterCategorization,
						  ApplicationTimestamp ).



% Sends all types of traces without requiring a class_TraceEmitter state, in a
% safe manner (synchronous and echoed on the console).
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone_safe( traces:message_type(), traces:message(),
			traces:emitter_categorization(), traces:app_timestamp() ) -> void().
send_standalone_safe( TraceType, Message, EmitterCategorization,
					  ApplicationTimestamp ) ->

	EmitterName = get_emitter_name_from_pid(),

	MessageCategorization = get_default_standalone_message_categorization(),

	send_standalone_safe( TraceType, Message, EmitterName,
		  EmitterCategorization, MessageCategorization, ApplicationTimestamp ).



% Sends all types of traces without requiring a class_TraceEmitter state, in a
% safe manner (synchronous and echoed on the console).
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone_safe( traces:message_type(), traces:message(),
					   traces:emitter_name(), traces:emitter_categorization(),
					   traces:message_categorization() ) -> void().
send_standalone_safe( TraceType, Message, EmitterName, EmitterCategorization,
					  MessageCategorization ) ->

	ApplicationTimestamp = time_utils:get_textual_timestamp(),

	send_standalone_safe( TraceType, Message, EmitterName,
		 EmitterCategorization, MessageCategorization, ApplicationTimestamp ).




% Sends all types of traces without requiring a class_TraceEmitter state, in a
% safe manner (synchronous and echoed on the console).
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone_safe( traces:message_type(), traces:message(),
		   traces:emitter_name(), traces:emitter_categorization(),
		   traces:message_categorization(), traces:app_timestamp() ) -> void().
send_standalone_safe( TraceType, Message, EmitterName, EmitterCategorization,
					  MessageCategorization, ApplicationTimestamp ) ->

	% Follows the order of our trace format; request call:
	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->

			trace_utils:error( "class_TraceEmitter:send_standalone_safe/6: "
							   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
							  ApplicationTimestamp ),

			% No State available here:
			EmitterNode = get_emitter_node_as_binary(),

			ActualMsgCateg = case MessageCategorization of

				uncategorized ->
					get_default_standalone_message_categorization();

				% Must be a string then:
				_ ->
					MessageCategorization

			end,

			AggregatorPid ! { sendSync,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=text_utils:string_to_binary( EmitterName ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _AppTimestamp=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=ActualMsgCateg,
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				], self() },

			trace_utils:echo( Message, TraceType, MessageCategorization,
							  TimestampText ),

			wait_aggregator_sync()

	end.



% Returns the name of the node this emitter is on, as a binary string.
%
% (static)
%
-spec get_emitter_node_as_binary() -> text_utils:bin_string().
get_emitter_node_as_binary() ->
	erlang:atom_to_binary( net_utils:localnode(), _Encoding=latin1 ).



% Returns the priority of specified trace type (i.e. fatal, error, etc.).
%
% Note: now that LogMX v1.3.2 and later only support 5 levels of detail
% (stack/error, warning/warn, info, fine, finest/debug, i.e. no more trace),
% fatal and error messages have been put at the same priority level, and
% Ceylan trace level has been kept, whereas others have been offset.
%
% See also: get_channel_name_for_priority/1.
%
% (static)
%
-spec get_priority_for( traces:message_type() ) -> traces:priority().
% Corresponds to stack/error:
get_priority_for( fatal ) ->
	1 ;

% Corresponds to stack/error:
get_priority_for( error ) ->
	2 ;

% Corresponds to warning/warn:
get_priority_for( warning ) ->
	3 ;

% Corresponds to info:
get_priority_for( info ) ->
	4 ;

% Corresponds to fine:
get_priority_for( trace ) ->
	5 ;

% Corresponds to finest/debug:
get_priority_for( debug ) ->
	6.

% 'void' not expected here.



% Returns the name of the trace channel corresponding to the trace priority.
%
% See also: get_priority_for/1
%
% (static)
%
-spec get_channel_name_for_priority( traces:priority() ) ->
										traces:message_type().
get_channel_name_for_priority( 1 ) ->
	fatal;

get_channel_name_for_priority( 2 ) ->
	error;

get_channel_name_for_priority( 3 ) ->
	warning;

get_channel_name_for_priority( 4 ) ->
	info;

get_channel_name_for_priority( 5 ) ->
	trace;

get_channel_name_for_priority( 6 ) ->
	debug.

% 'void' not expected here.





% Section for helper functions.


% Returns a default emitter name, deduced from the PID of the corresponding
% process.
%
% (helper)
%
-spec get_emitter_name_from_pid() -> emitter_name().
get_emitter_name_from_pid() ->

	% Not wanting dots in PID here (otherwise this would be interpreted as
	% sub-categories in the traces):
	%
	text_utils:substitute( $., $-, pid_to_list( self() ) ).



% Returns the default message categorization.
%
% (helper)

-spec get_default_standalone_message_categorization() ->
								 emitter_categorization().
get_default_standalone_message_categorization() ->
	"Standalone".



% Initializes some context-specific information.
%
% (helper)
%
-spec init( wooper:state() ) -> wooper:state().
init( State ) ->

	% Context-specific, useful to re-use, for example for deserialisation:

	% Retrieves the trace aggregator (false: do not launch it if not available,
	% otherwise the creation of multiple emitters would result in a race
	% condition that would lead to the creation of multiple aggregators):
	%
	AggregatorPid = class_TraceAggregator:get_aggregator(
												  _LaunchAggregator=false ),

	setAttributes( State, [
		{ emitter_node, get_emitter_node_as_binary() },
		{ trace_aggregator_pid, AggregatorPid } ] ).



% Implementation of functions used by trace macros.


% Sets the trace categorization (part of the full emitter categorization) for
% this trace emitter to specified plain string.
%
% Setting the trace categorization early in the constructor, before sending any
% trace, allows to have all traces for a given emitter correctly gathered in the
% same trace category, which is a lot clearer when browsing afterwards.
%
% (helper)
%
-spec set_categorization( traces:emitter_categorization(), wooper:state() ) ->
								wooper:state().
set_categorization( TraceCategorization, State ) ->
	setAttribute( State, trace_categorization,
				  text_utils:string_to_binary( TraceCategorization ) ) .



% Sends a trace from that emitter.
% Message is a plain string.
%
% All information are available here, except the trace timestamp and the message
% categorization.
%
% (helper)
%
-spec send( traces:message_type(), wooper:state(), traces:message() ) -> void().
send( TraceType, State, Message ) ->
	send( TraceType, State, Message, _MessageCategorization=uncategorized ).



% Sends a trace from that emitter, echoing it through basic traces as well.
%
% Message is a plain string.
%
% All information are available here, except the trace timestamp and the message
% categorization.
%
% (helper)
%
-spec send_safe( traces:message_type(), wooper:state(), traces:message() ) ->
					   void().
send_safe( TraceType, State, Message ) ->
	send_safe( TraceType, State, Message,
			   _MessageCategorization=uncategorized ).




% Message is a plain string, MessageCategorization as well unless it is the
% 'uncategorized' atom.

% All informations available but the timestamp, determining its availability:
%
% (helper)
%
-spec send( traces:message_type(), wooper:state(), traces:message(),
			traces:message_categorization() ) -> void().
send( TraceType, State, Message, MessageCategorization ) ->
	send( TraceType, State, Message, MessageCategorization,
		  get_trace_timestamp( State ) ).


% All informations available but the timestamp, determining its availability:
%
% (helper)
%
-spec send_safe( traces:message_type(), wooper:state(), traces:message(),
				 traces:message_categorization() ) -> void().
send_safe( TraceType, State, Message, MessageCategorization ) ->

	send_synchronisable( TraceType, State, Message, MessageCategorization,
						 get_trace_timestamp( State ) ),

	trace_utils:echo( Message, TraceType, MessageCategorization ),

	wait_aggregator_sync().




% Sends all types of (unsynchronised) traces.
%
% (helper)
%
-spec send( traces:message_type(), wooper:state(), traces:message(),
			traces:message_categorization(), traces:app_timestamp() ) -> void().
send( TraceType, State, Message, MessageCategorization, AppTimestamp ) ->

	TimestampText = text_utils:string_to_binary(
					  time_utils:get_textual_timestamp() ),

	MsgCateg = case MessageCategorization of

		uncategorized ->
			uncategorized;

		_ ->
			text_utils:string_to_binary( MessageCategorization )

	end,

	AppTimestampString = list_to_binary(
						   io_lib:format( "~p", [ AppTimestamp ] ) ),

	% Follows the order of our trace format; oneway call:
	% (toggle the comment for the two blocks below to debug)

	?getAttr(trace_aggregator_pid) ! { send,

	%io:format( "Sending trace: PID=~w, emitter name='~p', "
	%		   "emitter categorization='~p', "
	%		   "app timestamp='~p', user time='~p', location='~p', "
	%		   "message categorization='~p', trace type='~w', message='~p'~n",

		[
		 _TraceEmitterPid=self(),
		 _TraceEmitterName=?getAttr(name),
		 _TraceEmitterCategorization=?getAttr(trace_categorization),
		 AppTimestampString,
		 _Time=TimestampText,
		 _Location=?getAttr(emitter_node),
		 _MessageCategorization=MsgCateg,
		 _Priority=get_priority_for( TraceType ),
		 _Message=text_utils:string_to_binary( Message )
		]
	% ).
	}.



% Sends all types of synchronisable traces (the synchronisation answer is
% requested yet not waited here, to allow for any interleaving).
%
% (helper)
%
-spec send_synchronisable( traces:message_type(), wooper:state(),
		  traces:message(), traces:message_categorization(),
		  traces:app_timestamp() ) -> void().
send_synchronisable( TraceType, State, Message, MessageCategorization,
					 AppTimestamp ) ->

	% Almost exactly the same as send/5, except that the sendSync/10 agggregator
	% request is called instead of the send/10 oneway, so that it sends an
	% acknowlegment when done.

	TimestampText = text_utils:string_to_binary(
	   time_utils:get_textual_timestamp() ),

	MsgCateg = case MessageCategorization of

		uncategorized ->
			uncategorized;

		_ ->
			text_utils:string_to_binary( MessageCategorization )

	end,

	AppTimestampString = list_to_binary(
						   io_lib:format( "~p", [ AppTimestamp ] ) ),

	% Follows the order of our trace format; request call:
	% (toggle the comment for the two blocks below to debug)

	?getAttr(trace_aggregator_pid) ! { sendSync,

	%io:format( "Sending trace: PID=~w, emitter name='~p', "
	%		   "emitter categorization='~p', "
	%		   "app timestamp='~p', user time='~p', location='~p', "
	%		   "message categorization='~p', trace type='~w', message='~p'~n",

		[
		 _TraceEmitterPid=self(),
		 _TraceEmitterName=?getAttr(name),
		 _TraceEmitterCategorization=?getAttr(trace_categorization),
		 AppTimestampString,
		 _Time=TimestampText,
		 _Location=?getAttr(emitter_node),
		 _MessageCategorization=MsgCateg,
		 _Priority=get_priority_for( TraceType ),
		 _Message=text_utils:string_to_binary( Message )
		],
		self()
	% ).
	}.




% Sends all types of synchronised traces (the synchronisation answer is
% requested and waited).
%
% (helper)
%
-spec send_synchronised( traces:message_type(), wooper:state(),
		  traces:message(), traces:message_categorization(),
		  traces:app_timestamp() ) -> void().
send_synchronised( TraceType, State, Message, MessageCategorization,
				   AppTimestamp ) ->
	send_synchronisable( TraceType, State, Message, MessageCategorization,
						 AppTimestamp ),
	wait_aggregator_sync().



% The function used to send all types of traces, with an echo.
%
% (helper)
%
-spec send_safe( traces:message_type(), wooper:state(), traces:message(),
				 traces:message_categorization(), traces:app_timestamp() ) ->
					   void().
send_safe( TraceType, State, Message, MessageCategorization, AppTimestamp ) ->

	send_synchronisable( TraceType, State, Message, MessageCategorization,
						 AppTimestamp ),

	trace_utils:echo( Message, TraceType, MessageCategorization,
					  text_utils:format( "~p", [ AppTimestamp ] ) ),

	wait_aggregator_sync().



% Waits for the aggregator to report that a trace synchronization has been
% completed.
%
% (helper)
%
-spec wait_aggregator_sync() -> void().
wait_aggregator_sync() ->
	receive

		{ wooper_result, trace_aggregator_synchronised } ->
			ok

	end.



% Returns the current trace-level timestamp (ex: possibly an execution tick
% offset), or the atom 'none' if the emitter time is not known.
%
% (helper)
%
-spec get_trace_timestamp( wooper:state() ) -> traces:app_timestamp().
get_trace_timestamp( State ) ->

	% Note: if an exception "No key 'trace_timestamp' found in following table:
	% empty hashtable" is triggered, probably that State is not (yet?) a
	% TraceEmitter one (ex: if using the blank state of a constructor in
	% ?debug(...) instead of using ?send_debug(ATraceState,...)).
	%
	?getAttr(trace_timestamp).



% Returns the current trace-level timestamp, as a binary string.
%
% (helper)
%
-spec get_trace_timestamp_as_binary( wooper:state() ) ->
										   text_utils:bin_string().
get_trace_timestamp_as_binary( State ) ->
	% Avoiding text_utils for a supposed speed-up:
	Stringified = io_lib:format( "~p", [ ?getAttr(trace_timestamp) ] ),
	list_to_binary( Stringified ).




% Returns the name of this trace emitter, as a plain string (not as a binary).
%
% (helper)
%
-spec get_plain_name( wooper:state() ) -> string().
get_plain_name( State ) ->
	text_utils:binary_to_string( ?getAttr(name) ).



% Synchronises the caller with the trace aggregator, ensuring that all
% (asynchronous) operations it triggered on this aggregator are over.
%
% Useful to ensure that traces have been fully received and stored before
% continuing (possibly with a VM crash).
%
-spec sync( wooper:state() ) -> void().
sync( State ) ->

	?getAttr(trace_aggregator_pid) ! { sync, [], self() },

	receive

		{ wooper_result, trace_aggregator_synchronised } ->
			ok

	end.



% Awaits for the completion of trace outputs.
%
% No firm guarantee, done of a best-effort basis.
%
-spec await_output_completion() -> void().
await_output_completion() ->
	system_utils:await_output_completion( _Milliseconds=200 ).

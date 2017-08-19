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


% Default, main trace aggregator.
%
% It just collects traces from emitters and stores them at once in a file, in a
% relevant user-specified format.
%
% Trace listeners can connect at any time to the aggregator. In this case it
% will stop and send first the full current trace file to them. From that
% moment, incoming traces will be both written in file and sent to each trace
% listener still connected, so that all of them have exactly all the traces
% (once and only once).
%
-module(class_TraceAggregator).



% Determines what are the mother classes of this class (if any):
% (the trace aggregator is not a trace emitter per se)
-define( wooper_superclasses, [] ).



% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, TraceFilename, TraceType, TraceTitle,
		 IsPrivate, IsBatch ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).



% Member method declarations.
-define( wooper_method_export, send/10, sendSync/10, renameTraceFile/2,
		 launchTraceSupervisor/3, addTraceListener/2, removeTraceListener/2,
		 requestReadyNotification/1, sync/1 ).


% Static method declarations (to be directly called from module):
-define( wooper_static_method_export, create/1, get_aggregator/1, remove/0 ).


% Helpers:
-export([ send_internal_immediate/3, send_internal_immediate/4, inspect_fields/1 ]).


-define( trace_emitter_categorization, "Traces" ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For TraceExtension:
-include("traces.hrl").


% For registration name:
-include("class_TraceAggregator.hrl").


-define(LogPrefix,"[Trace Aggregator]").


% Use global:registered_names() to check aggregator presence.


%-define( LogOutput( Message, Format ),
%  trace_utils:debug_fmt( Message, Format ) ).

-define( LogOutput( Message, Format ), void ).





% Implementation Notes.
%
% The aggregator could store per-emitter constant settings (ex: emitter name and
% categorization) instead of having them sent to it each time, but more look-ups
% would be involved.
%
% The aggregator is not a standard emitter.



% Constructs a new trace aggregator:
%
% - TraceFilename is the name of the file where traces should be written to
%
% - TraceType is either 'log_mx_traces', { 'text_traces', 'text_only' } or
% { 'text_traces', 'pdf' }, depending whether LogMX should be used to browse the
% execution traces, or just a text viewer (possibly with a PDF displaying
% thereof)
%
% - TraceTitle is the title that should be used for traces; mostly used for the
% PDF output
%
% - IsPrivate tells whether this trace aggregator will be privately held (hence
% should not be registered in naming service) or if it is a (registered)
% singleton
%
% - IsBatch tells whether the aggregator is run in a batch context; useful when
% trace type is {text_traces,pdf}, so that this aggregator does not display the
% produced PDF when in batch mode
%
-spec construct( wooper:state(), file_utils:file_name(),
				 traces:trace_supervision_type(), string(), boolean(),
				 boolean() ) -> wooper:state().
construct( State, TraceFilename, TraceType, TraceTitle, IsPrivate, IsBatch ) ->

	%trace_utils:debug_fmt( "Starting trace aggregator, with initial trace "
	%					   "filename '~s'.", [ TraceFilename ] ),

	% First the direct mother classes (none here), then this class-specific
	% actions:
	%
	AbsTraceFilename = file_utils:ensure_path_is_absolute( TraceFilename ),

	% Creates the trace file as soon as possible:
	File = open_trace_file( AbsTraceFilename ),

	% Increases the chances that the aggregator does not lag too much behind the
	% current simulation state:
	erlang:process_flag( priority, _Level=high ),

	SetState = setAttributes( State, [
		{ trace_filename, AbsTraceFilename },
		{ trace_file, File },
		{ trace_type, TraceType },
		{ trace_title, TraceTitle },
		{ trace_listeners, [] },
		{ is_batch, IsBatch } ] ),

	% We do not display these information of the console now, as the
	% appplication may have to change the trace filename (the best moment to
	% display that file information on the console is when the filename is
	% final, typically when the trace supervisor is started):
	%
	%trace_utils:info_fmt( "~n~s ~s", [ ?LogPrefix, Message ] ),


	PrivateState = case IsPrivate of

		true ->
			trace_utils:info( "~s Creating a private trace aggregator, "
							  "whose PID is ~w.", [ ?LogPrefix, self() ] ),
			setAttribute( SetState, is_private, true );

		false ->
			%trace_utils:info( "~n~s Creating the trace aggregator, "
			%   "whose PID is ~w.~n", [ ?LogPrefix, self() ] ),

			naming_utils:register_as( ?trace_aggregator_name, local_and_global ),

			setAttribute( SetState, is_private, false )

	end,

	% Closure used to avoid exporting the function (beware of self()):
	AggregatorPid = self(),

	OverLoadMonitorPid = spawn_link( fun() ->
					overload_monitor_main_loop( AggregatorPid ) end ),

	OverloadState = setAttribute( PrivateState, overload_monitor_pid,
								  OverLoadMonitorPid ),

	HeaderState = manage_trace_header( OverloadState ),

	% Writes the very first trace after this header, returns an updated state:
	TraceState = send_internal_immediate( info, "Trace aggregator created, "
								"trace filename is '~s', trace type is '~w', "
								"and trace title is '~s'.~n",
				   [ AbsTraceFilename, TraceType, TraceTitle ], HeaderState ),

	case is_tracing_activated() of

		true ->
			TraceState;

		false ->
			send_internal_immediate( warning, "Note that this trace aggregator has been "
						   "built with the trace system being deactivated, "
						   "hinting that the overall codebase is probably "
						   "in the same case (hence a lot less talkative).",
						   TraceState )

	end.




% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%trace_utils:debug_fmt( "~s Deleting trace aggregator.", [ ?LogPrefix ] ),

	?getAttr(overload_monitor_pid) ! delete,

	% Class-specific actions:
	case ?getAttr(is_private) of

		true ->
			ok;

		false ->
			naming_utils:unregister( ?trace_aggregator_name, local_and_global )

	end,

	FooterState = manage_trace_footer( State ),

	TraceFile = ?getAttr(trace_file),

	% We were performing immediate writes here (due to the delayed_write option,
	% close may return an old write error and not even try to close the file. In
	% that case we try to close it another time):
	%
	case file:close( TraceFile ) of

		{ error, _Reason } ->
			file:close( TraceFile ) ;

		ok  ->
			ok

	end,

	case ?getAttr(trace_type) of

		log_mx_traces ->
			ok;

		{ text_traces, text_only } ->
			ok;

		{ text_traces, pdf } ->

			trace_utils:info( "~s Generating PDF trace report.",
							  [ ?LogPrefix ] ),

			PdfTargetFilename = file_utils:replace_extension(
				?getAttr(trace_filename), ?TraceExtension, ".pdf" ),

			% Supposedly in the path:
			GenerationCommand = executable_utils:find_executable( "make" )
				++ " '" ++ PdfTargetFilename ++ "' VIEW_PDF=no",

			%io:format( "PDF generation command is '~s'.~n",
			% [ GenerationCommand ] ),

			case system_utils:run_executable( GenerationCommand ) of

				{ _ExitCode=0, _Output } ->

					case ?getAttr(is_batch) of

						true ->
							ok;

						false ->
							trace_utils:info_fmt(
							  "~s Displaying PDF trace report.",
							  [ ?LogPrefix ] ),

							executable_utils:display_pdf_file(
							  PdfTargetFilename )

						end;

				{ ExitCode, ErrorOutput } ->
					trace_utils:error_fmt(
					  "~s Generation of PDF from ~s failed (error ~B: '~s').",
					  [ ?LogPrefix, ?getAttr(trace_filename),
						ExitCode, ErrorOutput ] )

			end

	end,

	% Then call the direct mother class counterparts: (none)

	%trace_utils:info_fmt( "~s Aggregator deleted.", [ ?LogPrefix ] ),

	% Allow chaining:
	FooterState.





% Methods section.


% Sends a full trace to this aggregator to have it processed, i.e. stored or
% directly written.
%
% The nine fields correspond to the ones defined in our trace format.
%
% (oneway)
%
-spec send( wooper:state(), pid(), traces:emitter_name(),
			traces:emitter_categorization(), traces:app_timestamp(),
			traces:time(), traces:location(), traces:message_categorization(),
			traces:priority(), traces:message() ) -> oneway_return().
send( State, TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
	  AppTimestamp, Time, Location, MessageCategorization, Priority,
	  Message ) ->

	% Useful to check that all fields are of minimal sizes (ex: binaries):
	%inspect_fields( [ TraceEmitterPid, TraceEmitterName,
	%				  TraceEmitterCategorization,
	%				  AppTimestamp, Time, Location, MessageCategorization,
	%				  Priority, Message ] ),

	Trace = format_trace_for( ?getAttr(trace_type),
		{ TraceEmitterPid, TraceEmitterName,
		  TraceEmitterCategorization, AppTimestamp, Time, Location,
		  MessageCategorization, Priority, Message } ),

	% Was: io:format( ?getAttr(trace_file), "~s", [ Trace ] ),
	% but now we use faster raw writes:
	%ok = file:write( ?getAttr(trace_file), io_lib:format( "~s", [ Trace ] ) ),
	file_utils:write( ?getAttr(trace_file), Trace ),

	Listeners = ?getAttr(trace_listeners),

	case Listeners of

		[] ->
			ok;

		_AtLeastOne ->

			BinTrace = text_utils:string_to_binary( Trace ),

			lists:foreach(
				fun( Listener ) ->

					Listener ! { addTrace, BinTrace }

				end,
				Listeners )

	end,

	?wooper_return_state_only( State ).



% Same as the send/10 oneway, except a synchronisation message is sent to the
% emitter.
%
% (request)
%
-spec sendSync( wooper:state(), pid(), traces:emitter_name(),
				traces:emitter_categorization(), traces:app_timestamp(),
				traces:time(), traces:location(),
				traces:message_categorization(), traces:priority(),
				traces:message() ) ->
					  request_return( 'trace_aggregator_synchronised' ).
sendSync( State, TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		  AppTimestamp, Time, Location, MessageCategorization, Priority,
		  Message ) ->

	% Useful to check that all fields are of minimal sizes (ex: binaries):
	%inspect_fields( [ TraceEmitterPid, TraceEmitterName,
	%				  TraceEmitterCategorization,
	%				  AppTimestamp, Time, Location, MessageCategorization,
	%				  Priority, Message ] ),

	Trace = format_trace_for( ?getAttr(trace_type),
		{ TraceEmitterPid, TraceEmitterName,
		  TraceEmitterCategorization, AppTimestamp, Time, Location,
		  MessageCategorization, Priority, Message } ),

	% Was: io:format( ?getAttr(trace_file), "~s", [ Trace ] ),
	% but now we use faster raw writes:
	%ok = file:write( ?getAttr(trace_file), io_lib:format( "~s", [ Trace ] ) ),
	file_utils:write( ?getAttr(trace_file), Trace ),

	Listeners = ?getAttr(trace_listeners),

	case Listeners of

		[] ->
			ok;

		_AtLeastOne ->

			BinTrace = text_utils:string_to_binary( Trace ),

			lists:foreach(
				fun( Listener ) ->

					Listener ! { addTrace, BinTrace }

				end,
				Listeners )

	end,

	?wooper_return_state_result( State, trace_aggregator_synchronised ).




% Renames the trace file currently in use.
%
% Useful for example when the application requires an identifier to be included
% in the trace filename to discriminate among different runs.
%
% Note: if another process is reading that file (ex: a trace supervisor), an I/O
% will be triggered at its level (hence this is not a solution to transparently
% rename a file).
%
% (oneway)
%
-spec renameTraceFile( wooper:state(), file_utils:file_name() ) ->
							 oneway_return().
renameTraceFile( State, NewTraceFilename ) ->

	TraceFilename = ?getAttr(trace_filename),

	AbsNewTraceFilename = file_utils:ensure_path_is_absolute(
							NewTraceFilename ),

	SentState = send_internal_immediate( info,
							   "Trace aggregator renaming atomically trace "
							   "file from '~s' to '~s'.~n",
							   [ TraceFilename, AbsNewTraceFilename ],
							   State ),

	% Switching:
	%file_utils:close( ?getAttr(trace_file) ),
	%File = open_trace_file( AbsNewTraceFilename ),
	%
	% Better yet still not sufficient for a transparent renaming:
	file_utils:rename( TraceFilename, AbsNewTraceFilename ),

	NewState = setAttribute( SentState, trace_filename, AbsNewTraceFilename ),

	?wooper_return_state_only( NewState ).



% Launches the trace supervisor.
%
% Useful to do so once the final trace filename is known.
%
-spec launchTraceSupervisor( wooper:state(), traces:trace_supervision_type(),
							 pid() ) -> oneway_return().
launchTraceSupervisor( State, TraceType, TraceAggregatorPid ) ->

	class_TraceSupervisor:init( ?getAttr(trace_filename), TraceType,
								TraceAggregatorPid ),

	?wooper_return_state_only( State ).



% Adds specified trace listener to this aggregator.
%
% (oneway)
%
-spec addTraceListener( wooper:state(), pid() ) -> oneway_return().
addTraceListener( State, ListenerPid ) ->

	% Better log this events directly in the traces:
	NewState = case ?getAttr(trace_type) of

		log_mx_traces ->

			TraceFile = ?getAttr(trace_file),

			% Done ASAP, otherwise the listener could lose traces if buffers of
			% all levels were not flushed:

			% Not sufficient by itself...
			file:sync( TraceFile ),

			% ...so we have also to close and re-open:
			file_utils:close( TraceFile ),

			% Not a trace emitter but still able to send traces (to itself);
			% will be read from mailbox as first live-forwarded message:
			%
			send_internal_deferred( info,
									"Trace aggregator adding trace listener "
									"~w, and sending it previous traces.~n",
									[ ListenerPid ] ),

			% Transfers file:
			TraceFilename = ?getAttr(trace_filename),

			% We used to rely on basic ZIP over Erlang carrier:
			%Bin = file_utils:file_to_zipped_term( TraceFilename ),
			%ListenerPid ! { trace_zip, Bin, TraceFilename },

			% Now we prefer using xz over sendFile, which must be a lot more
			% efficient:
			%
			XZFilename = file_utils:compress( TraceFilename,
											  _CompressionFormat=xz ),

			% If compressing 'traceManagement_test.traces' for example, we do
			% not want to send 'traceManagement_test.traces.xz', as if the
			% listener (ex: traceListening_test) happens to decompress the
			% received file on the same directory, it will overwrite the first
			% file.

			% That way we would be producing
			% "Listener-traceManagement_test.traces" for example:
			%
			% The client will have nevertheless to receive this file to a
			% another (temporary) directory, as the same client-side overwriting
			% could happen directly with the compressed file (which have to
			% exist simultaneously on both ends).
			%
			% So finally the server-side renaming is useless, it will be done by
			% the listener:

			trace_utils:trace_fmt( "Sending '~s' to ~w.~n",
								   [ XZFilename, ListenerPid ] ),

			net_utils:send_file( XZFilename, ListenerPid ),

			file_utils:remove_file( XZFilename ),

			NewListeners = [ ListenerPid | ?getAttr(trace_listeners) ],

			NewFile = reopen_trace_file( TraceFilename ),

			setAttributes( State, [ { trace_listeners, NewListeners },
										{ trace_file, NewFile } ] );


		OtherTraceType ->

			Message = io_lib:format(
				"Trace aggregator not adding trace listener ~w, "
				"as it requires LogMX traces, whereas the current trace "
				"type is ~w.~n", [ ListenerPid, OtherTraceType ] ),

			?notify_warning( Message ),
			ListenerPid ! { trace_zip, incompatible_trace_type },
			State

	end,

	?wooper_return_state_only( NewState ).



% Removes specified trace listener from this aggregator.
%
% (oneway)
%
-spec removeTraceListener( wooper:state(), pid() ) -> oneway_return().
removeTraceListener( State, ListenerPid ) ->

	trace_utils:info_fmt( "~s Removing trace listener ~w.",
						  [ ?LogPrefix, ListenerPid ] ),

	SentState = send_internal_immediate( info, "Trace aggregator removing trace "
							   "listener ~w.~n",
							   [ ListenerPid ], State ),

	UnregisterState = deleteFromAttribute( SentState, trace_listeners,
										   ListenerPid ),

	?wooper_return_state_only( UnregisterState ).



% Requests this aggregator to send a notification as soon as it is ready,
% i.e. as soon as its trace file was created and its first trace was written.
%
% (const request)
%
-spec requestReadyNotification( wooper:state() ) ->
									  request_return( 'trace_file_ready' ).
requestReadyNotification( State ) ->

	% Being able to answer means ready, as a first synchronised message is sent
	% from the constructor:
	%
	?wooper_return_state_result( State, trace_file_ready ).



% Requests this aggregator to acknowledge this message (like a ping).
%
% The purpose is to ensure that all pending operations are performed (ex: so
% that any next crash will not result in the loss of traces).
%
-spec sync( wooper:state() ) ->
				  request_return( 'trace_aggregator_synchronised' ).
sync( State ) ->

	% Still to be done, otherwise might sit in cache and be wiped out before the
	% actual writing takes place:

	TraceFile = ?getAttr(trace_file),

	file:sync( TraceFile ),

	?wooper_return_state_result( State, trace_aggregator_synchronised ).




% 'Static' methods (module functions):


% Creates the trace aggregator asynchronously, with default settings.
%
% (static)
%
-spec create( boolean() ) -> pid().
create( UseSynchronousNew ) ->
	create( UseSynchronousNew, _TraceType=log_mx_traces ).



% Creates the trace aggregator asynchronously, using specified trace type.
%
% (static)
%
-spec create( boolean(), traces:trace_supervision_type() ) -> pid().
create( _UseSynchronousNew=false, TraceType ) ->
	new_link( ?trace_aggregator_filename, TraceType, ?TraceTitle,
			  _IsPrivate=false, _IsBatch=false  );

% Creates the trace aggregator synchronously, using specified trace type.
%
% (static)
%
create( _UseSynchronousNew = true, TraceType ) ->
	% Trace filename, isPrivate:
	synchronous_new_link( ?trace_aggregator_filename, TraceType,
						  ?TraceTitle, _IsPrivate=false, _IsBatch=false ).




% Returns the PID of the current trace aggregator.
%
% The parameter is a boolean telling whether the aggregator should be created if
% not available (if true), or if this method should just return a failure
% notification (if false).
%
% Note: to avoid race conditions between concurrent calls to this static method
% (ex: due to multiple trace emitter instances created in parallel), an
% execution might start with a call to this method with a blocking wait until
% the aggregator pops up in registry services.
%
% Waits a bit before giving up: useful when client and aggregator processes are
% launched almost simultaneously.
%
% (static)
%
-spec get_aggregator( boolean() ) ->
	   'trace_aggregator_launch_failed' | 'trace_aggregator_not_found' | pid().
get_aggregator( CreateIfNotAvailable ) ->

	% Only dealing with registered managers (instead of using directly their
	% PID) allows to be sure only one instance (singleton) is being used, to
	% avoid the case of two managers being launched at the same time (the second
	% will then terminate immediately).

	% If launching multiple trace emitters in a row, first emitter may trigger
	% the launch of trace aggregator, but second emitter might do the same if
	% the aggregator is still being initialized:
	%
	try

		naming_utils:wait_for_global_registration_of( ?trace_aggregator_name )

	catch { global_registration_waiting_timeout, _Name } ->

		case CreateIfNotAvailable of

			true ->

				% Not available, launch it synchronously (with default
				% settings):
				%
				create( true ),

				try

					naming_utils:wait_for_global_registration_of(
					  ?trace_aggregator_name )

				catch { global_registration_waiting_timeout, _Name } ->

						error_logger:error_msg(
						  "class_TraceAggregator:get_aggregatorunable to "
						  "launch successfully the aggregator.~n" ),
						trace_aggregator_launch_failed

				end;

			false ->
				% Not available and not to be started:
				trace_aggregator_not_found

		end

	end.



% Deletes synchronously the trace aggregator.
%
% (static)
%
-spec remove() -> 'delete' | 'trace_aggregator_not_found'.
remove() ->

	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->
			trace_aggregator_not_found;

		TraceAggregatorPid ->
			% WOOPER convenience:
			wooper:delete_synchronously_instance( TraceAggregatorPid )

	end.



% Code of the process that monitors the aggregator, overloading-wise.
%
overload_monitor_main_loop( AggregatorPid ) ->

	receive

		delete ->
			%io:format( "(overload monitor deleted)~n" ),
			deleted

	% Every 2s:
	after 2000 ->

			{ message_queue_len, QueueLen } = erlang:process_info(
				AggregatorPid, message_queue_len ),

			case QueueLen of

				TooMany when TooMany > 5000 ->
					io:format( "(warning: trace aggregator is overloaded, "
							   "too many traces are being sent, ~B of them "
							   "are still waiting to be processed)~n",
							   [ TooMany ] );

				_Other ->
					ok

			end,
			overload_monitor_main_loop( AggregatorPid )

	end.





% Some defines.


% Columns in text traces have fixed width, in characters (total: 110).
% In this mode, by default, emitter categorization, location and message
% categorization are not written.


% For Pid (ex: locally, <0.33.0>):
-define( PidWidth, 8 ).


% For EmitterName (ex: "First soda machine"):
-define( EmitterNameWidth, 12 ).


% For EmitterCategorization (ex: "TimeManagement"):
%-define( EmitterCategorizationWidth,12 ).
-define( EmitterCategorizationWidth, 0 ).


% For Tick (ex: unknown, 3169899360000) or any application-specific timestamp:
-define( AppTimestampWidth, 14 ).


% For Time (ex: "18/6/2009 16:32:14"):
-define( TimeWidth, 10 ).


% For Location (ex: "soda_deterministic_integration_run@a_example.org"):
%-define( LocationWidth,12 ).
-define( LocationWidth, 0 ).


% For MessageCategorization (ex: "Execution.Uncategorized"):
%-define( MessageCategorizationWidth, 4 ).
-define( MessageCategorizationWidth, 0 ).


% For Priority (ex: warning):
-define( PriorityWidth, 7 ).


% For Message:
-define( MessageWidth, 45 ).




% Helper functions.



% Sends traces immediately from the aggregator itself (hence to itself); this is
% done directly (ex: to write it, before any trace message that would be waiting
% in the mailbox).
%
% (helper)
%
-spec send_internal_immediate( traces:message_type(), string(),
					 wooper:state() ) -> wooper:state().
send_internal_immediate( MessageType, Message, State ) ->

	TimestampText = text_utils:string_to_binary(
					  time_utils:get_textual_timestamp() ),

	% Not State available here:
	EmitterNode = class_TraceEmitter:get_emitter_node_as_binary(),

	MessageCategorization = text_utils:string_to_binary( "Trace Management" ),

	SelfSentState = executeOneway( State, send, [
		_TraceEmitterPid=self(),
		_TraceEmitterName=text_utils:string_to_binary( "Trace Aggregator" ),
		_TraceEmitterCategorization=text_utils:string_to_binary(
									  ?trace_emitter_categorization ),
		_AppTimestamp=none,
		_Time=TimestampText,
		_Location=EmitterNode,
		MessageCategorization,
		_Priority=class_TraceEmitter:get_priority_for( MessageType ),
								 Message ] ),

	trace_utils:echo( Message, MessageType ),

	SelfSentState.



% Sends format-based traces from the aggregator itself (hence to itself).
%
% (helper)
%
-spec send_internal_immediate( traces:message_type(), string(), [ any() ],
					 wooper:state() ) -> wooper:state().
send_internal_immediate( MessageType, MessageFormat, MessageValues, State ) ->
	Message = text_utils:format( MessageFormat, MessageValues ),
	send_internal_immediate( MessageType, Message, State ).




% Sends traces immediately from the aggregator itself (hence to itself), through
% a message sending, so that a trace can be sent even if at that point no trace
% file is available for writing.
%
% (helper)
%
-spec send_internal_deferred( traces:message_type(), string() ) ->
									basic_utils:void().
send_internal_deferred( MessageType, Message ) ->

	TimestampText = text_utils:string_to_binary(
					  time_utils:get_textual_timestamp() ),

	% Not State available here:
	EmitterNode = class_TraceEmitter:get_emitter_node_as_binary(),

	MessageCategorization = text_utils:string_to_binary( "Trace Management" ),

	self() ! { send, [
		_TraceEmitterPid=self(),
		_TraceEmitterName=text_utils:string_to_binary( "Trace Aggregator" ),
		_TraceEmitterCategorization=text_utils:string_to_binary(
									  ?trace_emitter_categorization ),
		_AppTimestamp=none,
		_Time=TimestampText,
		_Location=EmitterNode,
		MessageCategorization,
		_Priority=class_TraceEmitter:get_priority_for( MessageType ),
					  Message ] },

	trace_utils:echo( Message, MessageType ).




% Sends format-based traces from the aggregator itself (hence to itself).
%
% (helper)
%
-spec send_internal_deferred( traces:message_type(), string(), [ any() ] ) ->
									basic_utils:void().
send_internal_deferred( MessageType, MessageFormat, MessageValues ) ->
	Message = text_utils:format( MessageFormat, MessageValues ),
	send_internal_deferred( MessageType, Message ).






% Takes care of any header in the trace header.
%
% Returns an updated state.
%
% (helper function)
%
-spec manage_trace_header( wooper:state() ) -> wooper:state().
manage_trace_header( State ) ->

	case ?getAttr(trace_type) of

		log_mx_traces ->
			State;

		{ text_traces, _TargetFormat } ->

			Title = ?getAttr(trace_title) ++ " Execution Trace Report",

			% Builds a proper RST-compatible title layout:
			TitleText = io_lib:format(
				   "~s~n.. _table:~n~n.. contents:: Table of Contents~n~n",
				   [ text_utils:generate_title(Title,1) ] )
				++ io_lib:format( "~s", [ text_utils:generate_title(
												 "Execution Context", 2 ) ] )
				++ io_lib:format( "Report generated on ~s, "
					"from trace file ``~s``, on host ``~s``.~n~n",
					[ time_utils:get_textual_timestamp(),
					  ?getAttr(trace_filename), net_adm:localhost() ] )
				++ io_lib:format( "~s",
					[ text_utils:generate_title( "Trace Begin", 2 ) ] ),

			PidLines = text_utils:format_text_for_width(
						 "Pid of Trace Emitter", ?PidWidth ),

			EmitterNameLines = text_utils:format_text_for_width(
								 "Emitter Name", ?EmitterNameWidth ),

			AppTimestampLines = text_utils:format_text_for_width(
						  "Application Timestamp", ?AppTimestampWidth ),

			TimeLines = text_utils:format_text_for_width( "User Time",
														  ?TimeWidth ),

			PriorityLines = text_utils:format_text_for_width( "Trace Type",
															  ?PriorityWidth ),

			MessageLines = text_utils:format_text_for_width(
				"Trace Message", ?MessageWidth ),

			HeaderLine = format_linesets( PidLines, EmitterNameLines,
				AppTimestampLines, TimeLines, PriorityLines, MessageLines ) ,

			file_utils:write( ?getAttr(trace_file), TitleText
							  ++ get_row_separator() ++ HeaderLine
							  ++ get_row_separator( $= ) ),

			State

	end.



% Takes care of any header in the trace header.
%
% Returns an updated state.
%
% (helper function)
%
-spec manage_trace_footer( wooper:state() ) -> wooper:state().
manage_trace_footer( State ) ->

	case ?getAttr(trace_type) of

		log_mx_traces ->
			State;

		{ text_traces, _TargetFormat } ->
			file_utils:write( ?getAttr(trace_file), io_lib:format(
					   "~s~n~nEnd of execution traces.~n"
					   "~nBack to the table_ of contents "
					   "and to the beginning of traces.",
				[ text_utils:generate_title( "Trace End", 2 ) ] ) ),
			State

	end.



% Returns the typical separator between array rows.
%
get_row_separator() ->
	get_row_separator( $- ).


% Returns the typical separator between array rows, with specified dash element
% to represent horizontal lines.
%
get_row_separator( DashType ) ->
	[ $+ ] ++ string:chars( DashType, ?PidWidth )
		++ [ $+ ] ++ string:chars( DashType, ?EmitterNameWidth )
		++ [ $+ ] ++ string:chars( DashType, ?AppTimestampWidth )
		++ [ $+ ] ++ string:chars( DashType, ?TimeWidth )
		++ [ $+ ] ++ string:chars( DashType, ?PriorityWidth )
		++ [ $+ ] ++ string:chars( DashType, ?MessageWidth )
		++ [ $+ ] ++ "\n".



% Formats specified trace according to specified trace type.
%
% Returns a plain string.
%
-spec format_trace_for( traces:trace_supervision_type(),
		 { pid(), traces:emitter_name(), traces:emitter_categorization(),
		  traces:app_timestamp(), traces:time(), traces:location(),
		  traces:message_categorization(), traces:priority(),
		  traces:message() } ) -> string().
format_trace_for( log_mx_traces, { TraceEmitterPid,
		TraceEmitterName, TraceEmitterCategorization, AppTimestamp, Time,
		Location, MessageCategorization, Priority, Message } ) ->
	lists:flatten( io_lib:format( "~w|~s|~s|~s|~s|~s|~s|~w|~s~n",
		[ TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		  AppTimestamp, Time, Location, MessageCategorization, Priority,
		  Message ] ) );

format_trace_for( { text_traces, _TargetFormat }, { TraceEmitterPid,
		TraceEmitterName, _TraceEmitterCategorization, AppTimestamp, Time,
		_Location, _MessageCategorization, Priority, Message } ) ->

	% Not output here:
	% - TraceEmitterCategorization
	% - Location
	% - MessageCategorization

	PidLines = text_utils:format_text_for_width(
		io_lib:format( "~w", [ TraceEmitterPid ] ), ?PidWidth ),

	EmitterNameLines = text_utils:format_text_for_width(
		io_lib:format( "~s", [ TraceEmitterName ] ), ?EmitterNameWidth ),

	% Can be a tick, an atom like 'unknown' or anything alike:
	AppTimestampLines = text_utils:format_text_for_width(
		io_lib:format( "~p", [ AppTimestamp ] ), ?AppTimestampWidth ),

	TimeLines = text_utils:format_text_for_width(
		io_lib:format( "~s", [ Time ] ), ?TimeWidth ),

	PriorityLines = text_utils:format_text_for_width(
		io_lib:format( "~w", [
			class_TraceEmitter:get_channel_name_for_priority( Priority ) ] ),
		?PriorityWidth ),

	MessageLines = text_utils:format_text_for_width(
		io_lib:format( "~s", [ Message ] ), ?MessageWidth ),

	format_linesets( PidLines, EmitterNameLines, AppTimestampLines, TimeLines,
		PriorityLines, MessageLines ) ++ get_row_separator().



% Formats specified list of linesets.
%
format_linesets( PidLines, EmitterNameLines, AppTimestampLines, TimeLines,
				 PriorityLines, MessageLines ) ->

	Columns = [ PidLines, EmitterNameLines, AppTimestampLines, TimeLines,
				PriorityLines, MessageLines ],

	TotalLineCount = lists:max( [ length( L ) || L <- Columns ] ),

	ColumnsPairs = [ { PidLines, ?PidWidth },
					 { EmitterNameLines, ?EmitterNameWidth },
					 { AppTimestampLines, ?AppTimestampWidth },
					 { TimeLines, ?TimeWidth },
					 { PriorityLines, ?PriorityWidth },
					 { MessageLines, ?MessageWidth } ],

	%io:format( "Column pairs:~n~p~n", [ColumnsPairs] ),

	FullLines = format_full_lines( ColumnsPairs, _Acc=[], TotalLineCount,
								   _Res=[], _CurrentLine="" ),

	string:join( FullLines, "\n" ).



% Returns a list of full lines, made from the lines of each column.
% Here we finished to handle all lines (none remaining):
%
format_full_lines( _Rows, [], 0, Res, CurrentLine ) ->
	lists:reverse( [ CurrentLine | Res ] ) ;

% Here we arrived at the end of a global line, preparing for next one:
%
format_full_lines( [], Acc, RemainingLineCount, Res, CurrentLine ) ->
	format_full_lines( lists:reverse(Acc), [], RemainingLineCount-1,
					   [ CurrentLine ++ "|" | Res ], "" );

% Here the corresponding column has no more content, just filling with spaces:
%
format_full_lines( [ { [], Width } | ColumnPairs ], Acc, RemainingLineCount,
				   Res, CurrentLine ) ->
	format_full_lines( ColumnPairs, [ { [], Width } | Acc ], RemainingLineCount,
		Res, CurrentLine ++ "|" ++ string:chars( $\ ,Width ) );

% Here the corresponding column has content, just adding it:
%
format_full_lines( [ { [ Line | OtherLines ], Width } | ColumnPairs ], Acc,
				   RemainingLineCount, Res, CurrentLine ) ->

	format_full_lines( ColumnPairs, [ { OtherLines, Width } | Acc ],
					   RemainingLineCount, Res, CurrentLine ++ "|" ++ Line ).



% Opens the specified trace file for writing from scratch.
%
% (helper)
%
open_trace_file( TraceFilename ) ->
	file_utils:open( TraceFilename,
					 [ write | get_trace_file_base_options() ] ).



% Reopens the specified trace file for writing from last position.
%
% (helper)
%
reopen_trace_file( TraceFilename ) ->

	file_utils:open( TraceFilename,
					 [ append | get_trace_file_base_options() ] ).



% Returns the base option for trace writing.
%
% (helper)
%
get_trace_file_base_options() ->

	% Writes to file, as soon as 32KB or 0.5s is reached:
	%
	[ raw, { delayed_write, _Size=32*1024, _Delay=500 } ].



% Allows inspecting the trace messages, which are often copied and/or sent over
% the network, hence must be of minimal size.
%
% Use with: make traceManagement_run $BATCH|grep '(list)' to ensure that
% binaries are used whenever possible instead of strings.
%
% (helper)
%
inspect_fields( FieldsReceived ) ->

	AllVars = lists:flatmap( fun( F ) ->
									 [ F, F, meta_utils:get_type_of( F ) ]
							 end,
							 FieldsReceived ),

	io:format( "~n"
			   "- TraceEmitterPid: '~w', i.e. '~p' (~s)~n"
			   "- TraceEmitterName: '~w', i.e. '~p' (~s)~n"
			   "- TraceEmitterCategorization: '~w', i.e. '~p' (~s)~n"
			   "- AppTimestamp: '~w', i.e. '~p' (~s)~n"
			   "- Time: '~w', i.e. '~p' (~s)~n"
			   "- Location: '~w', i.e. '~p' (~s)~n"
			   "- MessageCategorization: '~w', i.e. '~p' (~s)~n"
			   "- Priority: '~w', i.e. '~p' (~s)~n"
			   "- Message: '~w', i.e. '~p' (~s)~n~n",
			   AllVars ).



% Tells whether this aggregator was build with the traces being activated
% (presumably at least most BEAMs are expected to be build with that same
% setting).
%
% (helper)
%
-spec is_tracing_activated() -> boolean().


-ifdef(tracing_activated).

is_tracing_activated() ->
	true.

-else. % tracing_activated

is_tracing_activated() ->
	false.

-endif. % tracing_activated

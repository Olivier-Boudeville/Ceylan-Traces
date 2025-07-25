% Copyright (C) 2007-2025 Olivier Boudeville
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

-module(class_TraceAggregator).

-moduledoc """
The **trace aggregator** class, in charge of collecting and storing the traces
sent by emitters.
""".


% See documentation at http://traces.esperide.org.


-define( class_description,
		 "Default, main trace aggregator. "
		 "It just collects traces from emitters and stores them at once in a "
		 "file, in a relevant user-specified format. "
		 "Trace listeners can connect at any time to the aggregator. In this "
		 "case it will stop and send first the full current trace file to "
		 "them. From that moment, incoming traces will be both written in file "
		 "and sent to each trace listener still connected, so that all of them "
		 "have exactly all the traces % (once and only once)." ).



% Determines what are the direct mother classes of this class (if any):
% (the trace aggregator is not a trace emitter per se)
-define( superclasses, [] ).


% Implementation notes:
%
% Here two unrelated kinds of supervisors are mentioned: the main one, for trace
% supervision (i.e. browsing and monitoring the traces) and the OTP one, for the
% supervisor behaviour.
%
% Unless specified otherwise, 'supervisor' relates here primarily to the trace
% supervision.


% Describes the class-specific attributes:
-define( class_attributes, [

	{ trace_filename, bin_file_path(),
	  "the path of the file in which traces are to be stored, as a binary "
	  "(e.g. <<\"/tmp/foobar.traces\">>)" },

	{ trace_file, file_utils:file(),
	  "the actual file device in which traces are written" },

	{ trace_type, trace_type(),
	  "the type of traces to be written (e.g. advanced_traces)" },

	{ trace_title, text_utils:title(),
	  "the title assigned to the traces that will be stored" },

	{ trace_listeners, [ listener_pid() ],
	  "the known trace listeners, similar to remote supervisors" },

	{ registration_scope, option( registration_scope() ),
	  "tells whether this aggregator shall be registered in the "
	  "naming service and, if yes, for which scope(s)" },

	{ is_batch, boolean(),
	  "tells whether the aggregator runs on batch (non-interactive) mode" },

	{ init_supervision, initialise_supervision(),
	  "tells whether/when the trace supervisor shall be launched" },

	{ supervisor_pid, option( supervisor_pid() ),
	  "the PID of the associated trace supervisor (if any)" },

	{ rotation_min_size, byte_size(),
	  "the minimum size of a traces file before it can be rotated" },

	{ rotation_count, count(), "the number of the upcoming rotation to "
	  "take place (useful to keep track of a series of rotated files)" },

	{ overload_monitor_pid, pid(),
	  "the PID of the process (if any) in charge of tracking the length of "
	  "the message queue of this aggregator in order to detect whenever "
	  "it is unable to cope with the intensity of message receivings" },

	{ watchdog_pid, option( pid() ),
	  "the PID of the process (if any) in charge of ensuring that this "
	  "aggregator is still up and running, by looking it up and sending "
	  "a trace to it periodically" } ] ).



-doc "PID of a trace aggregator.".
-type aggregator_pid() :: instance_pid().


-export_type([ aggregator_pid/0 ]).


% Helpers:
-export([ send_internal_immediate/3, send_internal_immediate/4,
		  inspect_fields/1 ]).



-doc "Tells whether/when the trace supervision shall be initialised.".
-type initialise_supervision() :: boolean() | 'later'.

-export_type([ initialise_supervision/0 ]).



-define( trace_emitter_categorization, "Traces" ).


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% For TraceExtension:
-include("traces.hrl").


% For registration name:
-include("class_TraceAggregator.hrl").


-define( log_prefix, "[Trace Aggregator]" ).


% Use global:registered_names() to check aggregator presence.


%-define( LogOutput( Message, Format ),
%  trace_utils:debug_fmt( Message, Format ) ).

-define( LogOutput( Message, Format ), void ).




% The default period in seconds between two checks done by the watchdog (15
% minutes):
%
-define( default_watchdog_period, 15*60 ).

-define( watchdog_emitter_categ, <<"Traces.Trace Watchdog">> ).


-define( advanced_traces_format, "~w|~ts|~ts|~ts|~ts|~ts|~ts|~B|~ts~n" ).



% Type shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type title() :: text_utils:title().
-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().

-type seconds() :: unit_utils:seconds().
-type milliseconds() :: unit_utils:milliseconds().

-type file_name() :: file_utils:file_name().
-type bin_file_path() :: file_utils:bin_file_path().
-type bin_file_name() :: file_utils:bin_file_name().
-type any_file_name() :: file_utils:any_file_name().

-type byte_size() :: system_utils:byte_size().

-type registration_name() :: naming_utils:registration_name().
-type registration_scope() :: naming_utils:registration_scope().
-type lookup_scope() :: naming_utils:lookup_scope().


-type trace_type() :: traces:trace_supervision_type().
-type message() :: traces:message().
-type bin_message() :: traces:bin_message().
-type trace_severity() :: traces:trace_severity().
-type bin_emitter_name() :: traces:bin_emitter_name().
-type trace_supervision_type() :: traces:trace_supervision_type().
-type bin_emitter_categorization() :: traces:bin_emitter_categorization().
-type bin_message_categorization() :: traces:bin_message_categorization().
-type priority() :: traces:priority().
-type app_timestamp() :: traces:app_timestamp().
-type bin_time() :: traces:bin_time().
-type bin_location() :: traces:bin_location().

-type listener_pid() :: class_TraceListener:listener_pid().

-type emitter_pid() :: class_TraceEmitter:emitter_pid().
-type preformatted_trace() :: class_TraceEmitter:preformatted_trace().

-type supervisor_pid() :: class_TraceSupervisor:supervisor_pid().



% Implementation notes:
%
% The aggregator could store per-emitter constant settings (e.g. emitter name
% and categorization) instead of having them sent to it each time, but more
% look-ups would be involved.
%
% The aggregator is not a standard trace emitter; do not use for example
% ?debug-like macros.
%
% The aggregator can be (optionally) plugged to an OTP supervision tree, thanks
% to the Traces supervisor bridge (see the traces_sup module).
%
% The most usual registration scope for a trace aggregator used to be
% global_only, as such this is the convention that was first prioritised and
% tuned for, yet it is now increasingly local, to uncouple applications.



-doc """
Constructs a trace aggregator.

Construction parameters are:

- TraceFilename is the path of the file in which traces should be written to

- TraceSupervisionType is either 'advanced_traces', {'text_traces', 'text_only'}
or {'text_traces', 'pdf'}, depending whether LogMX should be used to browse the
execution traces, or just a text viewer (possibly with a PDF displaying thereof)

- TraceTitle is the title that should be used for traces; mostly used for the
PDF output

- MaybeRegistrationScope tells whether this trace aggregator will be privately
held (hence should not be registered in naming service) - if set to 'undefined',
or if it is a registered (locally and/or globally) singleton

- IsBatch tells whether the aggregator is run in a batch context; useful when
trace type is {text_traces, pdf}, so that this aggregator does not display the
produced PDF when in batch mode
""".
-spec construct( wooper:state(), file_name(), trace_supervision_type(), title(),
				 option( registration_scope() ), boolean() ) -> wooper:state().
construct( State, TraceFilename, TraceSupervisionType, TraceTitle,
		   MaybeRegistrationScope, IsBatch ) ->
	construct( State, TraceFilename, TraceSupervisionType, TraceTitle,
			   MaybeRegistrationScope, IsBatch, _InitTraceSupervisor=false ).



-doc """
Constructs a trace aggregator, telling whether a trace supervisor shall be
created.

Construction parameters are:

- TraceFilename is the path of the file in which traces should be written to

- TraceSupervisionType is either 'advanced_traces', {'text_traces',
'text_only'} or {'text_traces', 'pdf'}, depending whether LogMX should be used
to browse the execution traces, or just a text viewer (possibly with a PDF
displaying thereof)

- TraceTitle is the title that should be used for traces; mostly used for the
PDF output

- MaybeRegistrationScope tells whether this trace aggregator will be privately
held (hence should not be registered in naming service) - if set to
'undefined', or if it is a registered (locally and/or globally) singleton

- IsBatch tells whether the aggregator is run in a batch context; useful when
trace type is {text_traces,pdf}, so that this aggregator does not display the
produced PDF when in batch mode

- InitTraceSupervisor tells whether the trace supervisor shall be created (now
or later, i.e. at the first renaming of the trace file) or not
""".
% Main constructor:
-spec construct( wooper:state(), file_name(), trace_supervision_type(), title(),
		option( registration_scope() ), boolean(), initialise_supervision() ) ->
						wooper:state().
construct( State, TraceFilename, TraceSupervisionType, TraceTitle,
		   MaybeRegistrationScope, IsBatch, InitTraceSupervisor ) ->

	%trace_utils:debug_fmt( "Starting trace aggregator, with initial trace "
	%   "filename '~ts' (init supervisor: ~w).",
	%   [ TraceFilename, InitTraceSupervisor ] ),

	% Wanting a better control by resisting to exit messages being received (see
	% the onWOOPERExitReceived/3 callback):
	%
	erlang:process_flag( trap_exit, true ),

	% Surely that these aggregators can be overwhelmed by (trace) messages:
	erlang:process_flag( message_queue_data, off_heap ),


	% First the direct mother classes (none here), then this class-specific
	% actions:

	AbsTraceFilename = file_utils:ensure_path_is_absolute( TraceFilename ),

	AbsBinTraceFilename = text_utils:string_to_binary( AbsTraceFilename ),

	% Creates the trace file as soon as possible:
	File = open_trace_file( AbsBinTraceFilename ),

	% Apparently not needed:
	%system_utils:force_unicode_support(),

	% Increases the chances that this aggregator does not lag too much behind
	% the current application state:
	%
	erlang:process_flag( priority, _Level=high ),

	ShouldInitTraceSupervisor = case InitTraceSupervisor of

		false ->
			false;

		IS when IS =:= true orelse IS =:= later ->
			case IsBatch of

				% Batch mode silences supervision in all cases:
				true ->
					false;

				false ->
					IS

			end

	end,

	%trace_utils:debug_fmt( "InitTraceSupervisor=~ts, IsBatch=~ts, "
	%   "ShouldInitTraceSupervisor=~ts.",
	%   [ InitTraceSupervisor, IsBatch, ShouldInitTraceSupervisor ] ),

	SetState = setAttributes( State, [
		{ trace_filename, AbsBinTraceFilename },
		{ trace_file, File },
		{ trace_type, TraceSupervisionType },
		{ trace_title, TraceTitle },
		{ trace_listeners, [] },

		% Checked just below:
		{ registration_scope, MaybeRegistrationScope },

		{ is_batch, IsBatch },
		{ init_supervision, ShouldInitTraceSupervisor },
		{ supervisor_pid, undefined },

		% 2 MB is a reasonable default:
		{ rotation_min_size, 2000000 },

		{ rotation_count, 1 },
		% overload_monitor_pid set later
		{ watchdog_pid, undefined } ] ),

	Localhost = net_utils:localhost(),

	% We do not display these information on the console now, as the application
	% may have to change the trace filename (the best moment to display that
	% file information on the console is when the filename is final, typically
	% when the trace supervisor is started):
	%
	%trace_utils:debug_fmt( "~ts ~ts", [ ?log_prefix, Message ] ),

	case MaybeRegistrationScope of

		undefined ->
			trace_utils:info_fmt( "~ts Creating a private trace aggregator "
				"whose PID is ~w, on host '~ts', whose (initial) trace "
				"file is '~ts'.",
				[ ?log_prefix, self(), Localhost, AbsBinTraceFilename ] );

		RegScope ->

			RegName = ?trace_aggregator_name,

			% Implicit check of scope:
			naming_utils:register_as( RegName, RegScope ),

			trace_utils:info_fmt( "~ts Creating a trace aggregator, "
				"whose PID is ~w, with name '~ts' and registration scope ~ts, "
				"on host '~ts', whose (initial) trace file is '~ts'.",
				[ ?log_prefix, self(), RegName, RegScope, Localhost,
				  AbsBinTraceFilename ] ),

			% As soon as possible (i.e. now that it is registered, provided that
			% get_aggregator/1 can find it, i.e. that it is registered
			% globally), we plug ourselves as the (single) logger handler from
			% now:
			%
			override_standard_logger_handler( RegScope ) andalso
				begin

					send_internal_deferred( info, "Self-registering as "
						"the default standard logger handler." ),

					% Note that a side-effect may be to enable a lower log
					% level, resulting in extra (logger) logs to be notified
					% when Traces is used:
					%
					traces:set_handler( self() )

				end

	end,

	% Closure used to avoid exporting the function (beware of self()):
	AggregatorPid = self(),

	OverloadMonitorPid = ?myriad_spawn_link(
		fun() -> overload_monitor_main_loop( AggregatorPid ) end ),

	OverloadState = setAttribute( SetState, overload_monitor_pid,
								  OverloadMonitorPid ),

	HeaderState = manage_trace_header( OverloadState ),

	% Writes the very first trace (hence a key one, relevant to share details)
	% after this header, returns an updated state:
    %
	TraceState = send_internal_immediate( notice, "Trace aggregator created "
        "on ~ts (node name: '~ts', naming mode: ~ts), trace filename is '~ts', "
        "trace type is '~w', and trace title is '~ts'.",
		[ net_utils:localhost(), node(), net_utils:get_node_naming_mode(),
          AbsBinTraceFilename, TraceSupervisionType, TraceTitle ],
		HeaderState ),

	case ShouldInitTraceSupervisor of

		true ->
			initialise_supervision( TraceState );

		_FalseOrLater ->
			TraceState

	end,

	case is_tracing_activated() of

		true ->
			%trace_utils:info( "Aggregator ready." ),
			TraceState;

		false ->
			send_internal_immediate( notice, "Note that this trace aggregator "
				"has been built with the trace system being deactivated, "
				"hinting that the overall codebase is probably in the same "
				"case (hence a lot less talkative).", TraceState )

	end.



-doc "Overridden destructor.".
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%trace_utils:debug_fmt( "~ts Deleting trace aggregator.", [ ?log_prefix ] ),

	% Otherwise would block the proper termination of the OTP Traces application
	% (see traces_otp_application_test.erl):
	%
	% (reset now rather than later, as no trace will be written anymore; we want
	% to minimise the risk that any standard log is lost because of this
	% termination)

	RegScope = ?getAttr(registration_scope),

	override_standard_logger_handler( RegScope ) andalso
		traces:reset_handler(),

	?getAttr(overload_monitor_pid) ! terminate,

	case ?getAttr(watchdog_pid) of

		undefined ->
			ok;

		WatchdogPid ->
			WatchdogPid ! terminate

	end,

	RegScope =:= undefined orelse
		naming_utils:unregister( ?trace_aggregator_name, RegScope ),

	FooterState = manage_trace_footer( State ),

	TraceFile = ?getAttr(trace_file),

	% We were performing immediate writes here (due to the delayed_write option,
	% close may return an old write error and not even try to close the file. In
	% that case we try to close it another time):
	%
	% (using here raw closes for more control)
	%
	case file:close( TraceFile ) of

		{ error, _Reason } ->
			file:close( TraceFile ) ;

		ok ->
			ok

	end,

	case ?getAttr(trace_type) of

		advanced_traces ->
			ok;

		{ text_traces, text_only } ->
			ok;

		{ text_traces, pdf } ->

			trace_utils:info_fmt( "~ts Generating PDF trace report.",
								  [ ?log_prefix ] ),

			PdfTargetFilename = file_utils:replace_extension(
				?getAttr(trace_filename), ?TraceExtension, ".pdf" ),

			% Supposedly in the path:
			GenerationCommand = executable_utils:find_executable( "make" )
				++ " '" ++ PdfTargetFilename ++ "' VIEW_PDF=no",

			%trace_utils:info_fmt( "PDF generation command is '~ts'.",
			%                      [ GenerationCommand ] ),

			case system_utils:run_command( GenerationCommand ) of

				{ _ExitCode=0, _Output } ->

					?getAttr(is_batch) orelse
						begin
							trace_utils:info_fmt( "~ts Displaying PDF trace "
								"report.", [ ?log_prefix ] ),

							executable_utils:display_pdf_file(
								PdfTargetFilename )

						end;

				{ ExitCode, ErrorOutput } ->
					trace_utils:error_fmt( "~ts Generation of PDF from ~ts "
						"failed (error ~B: '~ts').",
						[ ?log_prefix, ?getAttr(trace_filename),
						  ExitCode, ErrorOutput ] )

			end

	end,

	%trace_utils:info_fmt( "~ts Aggregator deleted.", [ ?log_prefix ] ),

	% Allow chaining:
	FooterState.




% Methods section.


-doc "Enables the aggregator watchdog, using the defaults.".
-spec enableWatchdog( wooper:state() ) -> oneway_return().
enableWatchdog( State ) ->

	WatchState = enable_watchdog( ?trace_aggregator_name,
		?default_trace_aggregator_lookup_scope,
		?default_watchdog_period, State ),

	wooper:return_state( WatchState ).



-doc """
Enables the aggregator watchdog, using the specified check period and the
defaults.
""".
-spec enableWatchdog( wooper:state(), seconds() ) -> oneway_return().
enableWatchdog( State, Period ) ->

	WatchState = enable_watchdog( ?trace_aggregator_name,
		?default_trace_aggregator_lookup_scope, Period, State ),

	wooper:return_state( WatchState ).



-doc """
Enables the aggregator watchdog, using the specified check period and naming
information.
""".
-spec enableWatchdog( wooper:state(), registration_name(), lookup_scope(),
					  seconds() ) -> oneway_return().
enableWatchdog( State, RegName, LookupScope, Period ) ->

	WatchState = enable_watchdog( RegName, LookupScope, Period, State ),

	wooper:return_state( WatchState ).



-doc """
Sends a full trace to this aggregator to have it processed, that is stored or
directly written.

The nine fields correspond to the ones defined in our trace format.
""".
-spec send( wooper:state(), pid(), bin_emitter_name(),
		bin_emitter_categorization(), app_timestamp(), bin_time(),
		bin_location(), bin_message_categorization(), priority(),
		bin_message() ) -> const_oneway_return().
send( State, TraceEmitterPid, BinTraceEmitterName,
	  BinTraceEmitterCategorization, AppTimestamp, BinTime, BinLocation,
	  BinMessageCategorization, Priority, BinMessage ) ->

	%trace_utils:debug_fmt( "Received trace message '~ts'.", [ Message ] ),

	% Useful to check that all fields are of minimal sizes (e.g. binaries):
	%inspect_fields( [ TraceEmitterPid, BinTraceEmitterName,
	%   BinTraceEmitterCategorization, AppTimestamp, BinTime, BinLocation,
	%   BinMessageCategorization, Priority, BinMessage ] ),
	% Wanting notably to avoid that plain strings get used silently:
	cond_utils:if_defined( traces_check_types,
		begin

			% As AppTimestamp :: any():
			type_utils:check_pid( TraceEmitterPid ),
			type_utils:check_integer( Priority ),
			type_utils:check_binaries( [ BinTraceEmitterName,
				BinTraceEmitterCategorization, BinTime, BinLocation,
				BinMessage ] ),

			( is_atom( BinMessageCategorization )
				orelse is_binary( BinMessageCategorization ) ) orelse
					throw( { non_binary_message_categorization,
							 BinMessageCategorization } )

		end ),

	Trace = format_trace_for( ?getAttr(trace_type),
		{ TraceEmitterPid, BinTraceEmitterName,
		  BinTraceEmitterCategorization, AppTimestamp, BinTime, BinLocation,
		  BinMessageCategorization, Priority, BinMessage } ),

	% Was: io:format( ?getAttr(trace_file), "~ts", [ Trace ] ),
	% but now we use faster raw writes:
	%ok = file:write( ?getAttr(trace_file),
	%                 text_utils:format( "~ts", [ Trace ] ) ),

	%trace_utils:debug_fmt( "Writing to ~p: ~p.",
	%                       [ ?getAttr(trace_file), Trace ] ),

	file_utils:write_ustring( ?getAttr(trace_file), Trace ),

	Listeners = ?getAttr(trace_listeners),

	Listeners =:= [] orelse
		begin
			BinTrace = text_utils:string_to_binary( Trace ),
			[ L ! { addTrace, BinTrace } || L <- Listeners ]
		end,

	wooper:const_return().



-doc """
Sends a preformatted trace to this aggregator to have it processed, that is
stored or directly written.
""".
-spec sendPreformatted( wooper:state(), preformatted_trace() ) ->
								const_oneway_return().
sendPreformatted( State, PreformattedTrace ) ->

	% Mostly like send/10 above:

	%trace_utils:debug_fmt( "Received preformatted trace '~ts'.",
	%                       [ PreformattedTrace ] ),

	cond_utils:if_defined( traces_check_types,
		type_utils:check_binary( PreformattedTrace ) ),

	file_utils:write( ?getAttr(trace_file), PreformattedTrace ),

	Listeners = ?getAttr(trace_listeners),

	Listeners =:= [] orelse
		begin
			[ L ! { addTrace, PreformattedTrace } || L <- Listeners ]
		end,

	wooper:const_return().



-doc """
Sends a full synchronised trace to this aggregator to have it processed, that is
stored or directly written.

Same as the send/10 oneway, except that a synchronisation message is sent back
to the caller.
""".
-spec sendSync( wooper:state(), pid(), bin_emitter_name(),
		bin_emitter_categorization(), app_timestamp(), bin_time(),
		bin_location(), bin_message_categorization(), priority(),
		bin_message() ) ->
			const_request_return( 'trace_aggregator_synchronised' ).
sendSync( State, TraceEmitterPid, BinTraceEmitterName,
		  BinTraceEmitterCategorization, AppTimestamp, BinTime, BinLocation,
		  BinMessageCategorization, Priority, BinMessage ) ->

	% Useful to check that all fields are of minimal sizes (e.g. binaries):
	%inspect_fields( [ TraceEmitterPid, BinTraceEmitterName,
	%   BinTraceEmitterCategorization, AppTimestamp, BinTime, BinLocation,
	%   BinMessageCategorization, Priority, BinMessage ] ),

	% Wanting notably to avoid that plain strings get used silently:
	cond_utils:if_defined( traces_check_types,
		begin

			% As AppTimestamp :: any():
			type_utils:check_pid( TraceEmitterPid ),
			type_utils:check_integer( Priority ),
			type_utils:check_binaries( [ BinTraceEmitterName,
				BinTraceEmitterCategorization, BinTime, BinLocation,
				BinMessage ] ),

			( is_atom( BinMessageCategorization )
				orelse is_binary( BinMessageCategorization ) ) orelse
					throw( { non_binary_message_categorization,
							 BinMessageCategorization } )

		end ),

	Trace = format_trace_for( ?getAttr(trace_type),
		{ TraceEmitterPid, BinTraceEmitterName,
		  BinTraceEmitterCategorization, AppTimestamp, BinTime, BinLocation,
		  BinMessageCategorization, Priority, BinMessage } ),

	%trace_utils:debug_fmt( "Writing sync trace '~ts'.", [ Trace ] ),

	TraceFile = ?getAttr(trace_file),

	% Was: io:format( TraceFile, "~ts", [ Trace ] ),
	% but now we use faster raw writes:
	%
	%ok = file:write( TraceFile, text_utils:format( "~ts", [ Trace ] ) ),

	file_utils:write_ustring( TraceFile, Trace ),

	Listeners = ?getAttr(trace_listeners),

	Listeners =:= [] orelse
		begin
			BinTrace = text_utils:string_to_binary( Trace ),
			[ L ! { addTrace, BinTrace } || L <- Listeners ]
		end,

	%trace_utils:debug_fmt( "Sync trace '~ts' written.", [ Trace ] ),

	% Done as late as possible, and strictly necessary (and apparently
	% sufficient), otherwise at least some crashes could prevent the trace to be
	% actually written on disk (actually seen in the wild):
	%
	file:sync( TraceFile ),

	wooper:const_return_result( trace_aggregator_synchronised ).



-doc """
Sends a preformatted, synchronised trace to this aggregator to have it
processed, that is stored or directly written.

Same as the sendPreformatted/10 oneway, except that a synchronisation message is
sent back to the caller.
""".
-spec sendPreformattedSync( wooper:state(), preformatted_trace() ) ->
			const_request_return( 'trace_aggregator_synchronised' ).
sendPreformattedSync( State, PreformattedTrace ) ->

	% Mostly like sendSync/10 above:

	cond_utils:if_defined( traces_check_types,
		type_utils:check_binary( PreformattedTrace ) ),

	%trace_utils:debug_fmt( "Writing sync preformatted trace '~ts'.",
	%   [ Trace ] ),

	TraceFile = ?getAttr(trace_file),

	file_utils:write( TraceFile, PreformattedTrace ),

	Listeners = ?getAttr(trace_listeners),

	Listeners =:= [] orelse
		begin
			[ L ! { addTrace, PreformattedTrace } || L <- Listeners ]
		end,

	%trace_utils:debug_fmt( "Sync preformatted trace '~ts' written.",
	%    [ PreformattedTrace ] ),

	% Done as late as possible, and strictly necessary (and apparently
	% sufficient), otherwise at least some crashes could prevent the trace to be
	% actually written on disk (actually seen in the wild):
	%
	file:sync( TraceFile ),

	wooper:const_return_result( trace_aggregator_synchronised ).



-doc """
Renames the trace file currently in use.

Useful for example when the application requires an identifier to be included in
the trace filename in order to discriminate among different runs, or if started
with OTP (no parameter given programatically at application start, hence the
creation of a supervisor is deferred until a later renaming is done; see the
init_supervision attribute).

Note: if another process is reading that file (e.g. a trace supervisor), an I/O
error will be triggered at its level (hence this is not a solution to
transparently rename a file).
""".
-spec renameTraceFile( wooper:state(), any_file_name() ) -> oneway_return().
renameTraceFile( State, NewTraceFilename ) when is_binary( NewTraceFilename ) ->

	RenamedState = renameTraceFile( State,
		text_utils:binary_to_string( NewTraceFilename ) ),

	wooper:return_state( RenamedState );


renameTraceFile( State, NewTraceFilename ) ->

	AbsNewTraceFilename =
		file_utils:ensure_path_is_absolute( NewTraceFilename ),

	BinTraceFilename = ?getAttr(trace_filename),

	InitSupervision = ?getAttr(init_supervision),

	Msg = text_utils:format( "Trace aggregator renaming atomically trace file "
		"from '~ts' to '~ts' (init supervision: ~ts).",
		[ BinTraceFilename, AbsNewTraceFilename, InitSupervision ] ),

	% Otherwise tracking down renamings is really inconvenient:
	trace_utils:notice( Msg ),

	SentState = send_internal_immediate( info, Msg, State ),

	% Switching gracefully:
	file_utils:close( ?getAttr(trace_file) ),
	file_utils:rename_preserving( BinTraceFilename, AbsNewTraceFilename ),
	NewFile = reopen_trace_file( AbsNewTraceFilename ),

	RenState = setAttributes( SentState, [
		{ trace_filename, text_utils:string_to_binary( AbsNewTraceFilename ) },
		{ trace_file, NewFile } ] ),

	SupState = case InitSupervision of

		later ->
			initialise_supervision( RenState );

		IS when is_boolean( IS ) ->
			%trace_utils:debug_fmt( "Not initialising trace supervision (~ts).",
			%                       [ IS ] ),
			RenState

	end,

	wooper:return_state( SupState ).



-doc """
Returns to the caller the current trace type in use.

Useful for example to launch a relevant trace supervision.
""".
-spec getTraceType( wooper:state() ) ->
				const_request_return( { 'notify_trace_type', trace_type() } ).
getTraceType( State ) ->

	Res = { notify_trace_type, ?getAttr(trace_type) },

	wooper:const_return_result( Res ).



-doc """
Returns to the caller the current trace settings in use.

Useful for example to launch a relevant trace supervision.
""".
-spec getTraceSettings( wooper:state() ) -> const_request_return(
			{ 'notify_trace_settings', bin_file_name(), trace_type() } ).
getTraceSettings( State ) ->

	Res = { notify_trace_settings, ?getAttr(trace_filename),
			?getAttr(trace_type) },

	wooper:const_return_result( Res ).



-doc """
Launches the trace supervisor, with settings that are by design relevant.

If possible, it is useful to do so only once the final trace filename is known.
""".
-spec launchTraceSupervisor( wooper:state() ) ->
		const_request_return( supervisor_pid() ).
launchTraceSupervisor( State ) ->

	SupervisorPid = class_TraceSupervisor:create( _MaybeWaitingPid=?getSender(),
		?getAttr(trace_filename), ?getAttr(trace_type),
		_TraceAggregatorPid=self() ),

	wooper:const_return_result( SupervisorPid ).



-doc "Registers the specified trace listener to this aggregator.".
-spec addTraceListener( wooper:state(), listener_pid() ) -> oneway_return().
addTraceListener( State, ListenerPid ) ->

	% Better log this events directly in the traces:
	NewState = case ?getAttr(trace_type) of

		advanced_traces ->

			TraceFile = ?getAttr(trace_file),

			% Done ASAP, otherwise the listener could lose traces if buffers of
			% all levels were not flushed:

			% Not sufficient by itself (so file:datasync/1 no sufficient
			% either)...
			%
			file:sync( TraceFile ),

			% ...so we have also to close and re-open later:
			file_utils:close( TraceFile ),

			% Transfers file:
			BinTraceFilename = ?getAttr(trace_filename),

			% Not a trace emitter, but still able to send traces (to itself);
			% will be read from mailbox as first live-forwarded message:
			%
			send_internal_deferred( info, "Trace aggregator adding trace "
				"listener ~w, and sending it previous traces (from '~ts').~n",
				[ ListenerPid, BinTraceFilename ] ),

			TraceFilename = text_utils:binary_to_string( BinTraceFilename ),

			% We used to rely on basic ZIP over Erlang carrier:
			%Bin = file_utils:file_to_zipped_term( TraceFilename ),
			%ListenerPid ! { trace_sending, Bin, TraceFilename },

			% Now we prefer using xz over sendFile, which must be a lot more
			% efficient:
			%
			XZFilename =
				file_utils:compress( TraceFilename, _CompressionFormat=xz ),

			% If compressing 'traceManagement_test.traces' for example, we do
			% not want to specify as a filename, literally,
			% 'traceManagement_test.traces.xz', as if the listener (e.g.
			% traceListening_test) happens to decompress the received file on
			% the same directory, it will overwrite the first file.
			%
			% So we relied on a specifically named file (e.g.
			% "Listener-traceManagement_test.traces" for example).
			%
			% Yet, the client will have nevertheless to receive this file to a
			% another (temporary) directory, as the same client-side overwriting
			% could happen directly with the compressed file (which has to exist
			% simultaneously on both ends).
			%
			% So finally the server-side renaming is useless, and it will be
			% done by the listener:

			trace_utils:info_fmt( "Sending '~ts' to listener ~w.",
								  [ XZFilename, ListenerPid ] ),

			SentState = try

				net_utils:send_file( XZFilename, ListenerPid ),

				NewListeners = [ ListenerPid | ?getAttr(trace_listeners) ],

				setAttribute( State, trace_listeners, NewListeners )

			catch

				throw:Exception:Stacktrace ->
					Message = text_utils:format( "Adding trace listener ~w "
						"failed, hence has been ignored: exception '~w' was "
						"raised.~nStacktrace was: ~ts",
						[ ListenerPid, Exception,
						  code_utils:interpret_stacktrace( Stacktrace ) ] ),
					% Will be duplicated on the console anyway:
					%trace_utils:error( Message ),
					send_internal_deferred( error, Message ),
					State

			end,

			% Not in an 'after' clause, as its result is ignored:
			file_utils:remove_file_if_existing( XZFilename ),
			NewFile = reopen_trace_file( TraceFilename ),
			setAttribute( SentState, trace_file, NewFile );

		OtherTraceSeverity ->

			Message = text_utils:format(
				"Trace aggregator not adding trace listener ~w, "
				"as it requires advanced (LogMX) traces, whereas the current "
				"trace type is ~w.~n", [ ListenerPid, OtherTraceSeverity ] ),

			?notify_warning( Message ),
			ListenerPid ! { trace_sending, incompatible_trace_type },
			State

	end,

	wooper:return_state( NewState ).



-doc "Removes specified trace listener from this aggregator.".
-spec removeTraceListener( wooper:state(), listener_pid() ) -> oneway_return().
removeTraceListener( State, ListenerPid ) ->

	trace_utils:info_fmt( "~ts Removing trace listener ~w.",
						  [ ?log_prefix, ListenerPid ] ),

	SentState = send_internal_immediate( info,
		"Trace aggregator removing trace listener ~w.~n",
		[ ListenerPid ], State ),

	UnregisterState =
		deleteFromAttribute( SentState, trace_listeners, ListenerPid ),

	wooper:return_state( UnregisterState ).



-doc """
Requests this aggregator to send a notification as soon as it is ready, that is
as soon as its trace file has been created and its first trace been written.
""".
-spec requestReadyNotification( wooper:state() ) ->
									const_request_return( 'trace_file_ready' ).
requestReadyNotification( State ) ->

	%trace_utils:info( "Requested for a ready notification." ),

	% Being able to answer means being ready, as a first synchronised message is
	% sent from the constructor:
	%
	wooper:const_return_result( trace_file_ready ).



-doc """
Requests this aggregator to flush any trace not already written in file and to
acknowledge this message (like a ping).

The purpose is to ensure that all pending operations are performed (e.g. so that
any next crash will not result in the loss of traces).
""".
-spec sync( wooper:state() ) ->
				const_request_return( 'trace_aggregator_synchronised' ).
sync( State ) ->

	% Still to be done, otherwise might sit in cache and be wiped out before the
	% actual writing takes place:

	TraceFile = ?getAttr(trace_file),

	file:sync( TraceFile ),

	wooper:const_return_result( trace_aggregator_synchronised ).



-doc """
Sets the specified minimum size for the trace file before it can be rotated.

A size of zero leads to unconditinal rotation.
""".
-spec setMinimumTraceFileSizeForRotation( wooper:state(), byte_size() ) ->
												oneway_return().
setMinimumTraceFileSizeForRotation( State, MinFileSize )
										when is_integer( MinFileSize ) ->

	send_internal_deferred( info, "Setting minimum size for trace file to ~ts.",
		[ system_utils:interpret_byte_size_with_unit( MinFileSize ) ] ),

	SetState = setAttribute( State, rotation_min_size, MinFileSize ),

	wooper:return_state( SetState ).



-doc """
Rotates the current trace file (asynchronous version): provided that its size is
above the current threshold, closes the current file, renames it, compresses it
and creates a file from scratch to avoid it becomes too large. No trace can be
lost in the process.

If the current trace file is named 'my_file.traces', its rotated version could
be an XZ archive named for example
'my_file.traces.7.2021-1-17-at-22h-14m-00s.xz' (a rotation count then a textual
timestamp), located in the same directory.
""".
-spec rotateTraceFile( wooper:state() ) -> oneway_return().
rotateTraceFile( State ) ->

	{ _Res, RotateState } = rotate_trace_file( State ),

	wooper:return_state( RotateState ).



-doc """
Rotates the current trace file (synchronous version): provided that its size is
above the current threshold, closes the current file, renames it, compresses it
and creates a file from scratch to avoid it becomes too large. No trace can be
lost in the process.

If the current trace file is named 'my_file.traces', its rotated version could
be an XZ archive named for example
'my_file.traces.7.2021-1-17-at-22h-14m-00s.xz' (a rotation count then a textual
timestamp), located in the same directory.
""".
-spec rotateTraceFileSync( wooper:state() ) ->
		request_return( fallible( { 'trace_file_rotated', bin_file_path() }
								| 'trace_file_below_min_size' ) ).
rotateTraceFileSync( State ) ->

	{ Res, RotState } = case rotate_trace_file( State ) of

		undefined ->
			{ trace_file_below_min_size, State };

		{ RotatedFilePath, RotateState } ->
			BinRotatedFilePath = text_utils:string_to_binary( RotatedFilePath ),
			{ { trace_file_rotated, BinRotatedFilePath }, RotateState }

	end,

	wooper:return_state_result( RotState, Res ).



-doc """
Callback triggered, as we trap exits, notably to reduce the risk of being
crashed in turn.
""".
-spec onWOOPERExitReceived( wooper:state(), pid(),
							basic_utils:exit_reason() ) -> oneway_return().
onWOOPERExitReceived( State, StopPid, _ExitType=normal ) ->

	Msg = text_utils:format( "Ignoring normal exit from process ~w.",
							 [ StopPid ] ),

	% Was 'notice', yet such a trace was often sent (once), most probably due to
	% the automatic renaming of the trace file or its sending to a trace
	% listener; so now only:
	%
	send_internal_deferred( debug, Msg ),

	wooper:const_return();


% Typically received from the Traces supervisor bridge when Traces is stopped:
onWOOPERExitReceived( State, Pid, ExitType=shutdown ) ->

	Msg = text_utils:format( "Trace aggregator received a shutdown message "
		"from ~w, shutting down immediately.", [ Pid ] ),

	SentState = send_internal_immediate( info, Msg, State ),

	trace_utils:debug( Msg ),

	DestructedState = destruct( SentState ),

	exit( ExitType ),

	% Never executed:
	wooper:return_state( DestructedState );


onWOOPERExitReceived( State, CrashPid, ExitType ) ->

	Msg = text_utils:format( "Trace aggregator received and ignored "
		"following exit message from ~w:~n  ~p", [ CrashPid, ExitType ] ),

	send_internal_deferred( error, Msg ),

	wooper:const_return().





% Static section.


-doc """
Creates the trace aggregator asynchronously, with default settings (advanced
traces, global registration and not in batch mode).
""".
-spec create( boolean() ) -> static_return( aggregator_pid() ).
create( UseSynchronousNew ) ->

	AggregatorPid = create( UseSynchronousNew, _TraceSeverity=advanced_traces ),

	wooper:return_static( AggregatorPid ).



-doc """
Creates the trace aggregator asynchronously, using specified trace type, and
registered globally.
""".
-spec create( boolean(), trace_supervision_type() ) ->
											static_return( aggregator_pid() ).
create( _UseSynchronousNew=false, TraceSeverity ) ->

	% For registration scope, see also get_aggregator/{0,1}:
	AggregatorPid = new_link( ?trace_aggregator_filename, TraceSeverity,
		?TraceTitle, ?default_trace_aggregator_registration_scope,
		_IsBatch=false ),

	wooper:return_static( AggregatorPid );

% Creates the trace aggregator synchronously, using specified trace type, and
% registered globally.
%
create( _UseSynchronousNew=true, TraceSeverity ) ->

	% For registration scope, see also get_aggregator/{0,1}:
	AggregatorPid = synchronous_new_link( ?trace_aggregator_filename,
		TraceSeverity, ?TraceTitle,
		?default_trace_aggregator_registration_scope, _IsBatch=false ),

	wooper:return_static( AggregatorPid ).



-doc """
Returns the PID of the current trace aggregator, supposed to be already
available, or an error atom.

Waits a bit before giving up: useful when client and aggregator processes are
launched almost simultaneously.
""".
-spec try_get_aggregator() ->
		static_return( 'trace_aggregator_not_found' | aggregator_pid() ).
try_get_aggregator() ->
	AggRes = get_aggregator( _CreateIfNotAvailable=false ),
	wooper:return_static( AggRes ).



-doc """
Returns the PID of the current trace aggregator, supposed to be already
available, or throws an exception.

Waits a bit before giving up: useful when client and aggregator processes are
launched almost simultaneously.
""".
-spec get_aggregator() -> static_return(  aggregator_pid() ).
get_aggregator() ->

	case try_get_aggregator() of

		trace_aggregator_not_found ->
			throw( trace_aggregator_not_found );

		AggPid ->
			wooper:return_static( AggPid )

	end.



-doc """
Returns the PID of the current trace aggregator, using the default look-up
scope, or an error atom.

The parameter is a boolean telling whether the aggregator should be created if
not available (if true), or if this method should just return a failure
notification (if false).

Note: to avoid race conditions between concurrent calls to this static method
(e.g. due to multiple trace emitter instances created in parallel), an execution
might start with a call to this method with a blocking wait until the aggregator
pops up in registry services.

Waits a bit before giving up: useful when client and aggregator processes are
launched almost simultaneously.
""".
-spec try_get_aggregator( boolean() ) ->
			static_return( 'trace_aggregator_launch_failed'
						 | 'trace_aggregator_not_found' | aggregator_pid() ).
try_get_aggregator( CreateIfNotAvailable ) ->
	AggRes = get_aggregator( CreateIfNotAvailable,
                             ?default_trace_aggregator_lookup_scope ),
	wooper:return_static( AggRes ).



-doc """
Returns the PID of the current trace aggregator, using the default look-up
scope, or throws an exception.

The parameter is a boolean telling whether the aggregator should be created if
not available (if true), or if this method should just return a failure
notification (if false).

Note: to avoid race conditions between concurrent calls to this static method
(e.g. due to multiple trace emitter instances created in parallel), an execution
might start with a call to this method with a blocking wait until the aggregator
pops up in registry services.

Waits a bit before giving up: useful when client and aggregator processes are
launched almost simultaneously.
""".
-spec get_aggregator( boolean() ) -> static_return( aggregator_pid() ).
get_aggregator( CreateIfNotAvailable ) ->
	case try_get_aggregator( CreateIfNotAvailable ) of

		AggPid when is_pid( AggPid ) ->
			wooper:return_static( AggPid );

		ErrorAtom ->
			throw( ErrorAtom )

	end.



-doc """
Returns the PID of the current trace aggregator, using specified look-up scope,
or an error atom.

The parameter is a boolean telling whether the aggregator should be created if
not available (if true), or if this method should just return a failure
notification (if false).

Note: to avoid race conditions between concurrent calls to this static method
(e.g. due to multiple trace emitter instances created in parallel), an execution
might start with a call to this method with a blocking wait until the aggregator
pops up in registry services.

Waits a bit before giving up: useful when client and aggregator processes are
launched almost simultaneously.
""".


-spec try_get_aggregator( boolean(), lookup_scope() ) ->
			static_return( 'trace_aggregator_launch_failed'
						 | 'trace_aggregator_not_found' | aggregator_pid() ).
try_get_aggregator( CreateIfNotAvailable, LookupScope ) ->

	AggRegName = ?trace_aggregator_name,

	% Only dealing with registered managers (instead of using directly their
	% PID) allows to be sure only one instance (singleton) is being used, to
	% avoid the case of two managers being launched at the same time (the second
	% will then terminate immediately).

	% If launching multiple trace emitters in a row, first emitter may trigger
	% the launch of trace aggregator, but second emitter might do the same if
	% the aggregator is still being initialised:
	%
	AggRes = try

		naming_utils:wait_for_registration_of( AggRegName, LookupScope )

	catch { registration_waiting_timeout, _Name, _Scope } ->

		case CreateIfNotAvailable of

			true ->

				% Not available, launch it synchronously (with default
				% settings):
				%
				create( true ),

				try

					naming_utils:wait_for_registration_of( AggRegName,
														   LookupScope )

				catch { registration_waiting_timeout, _AName, _AScope } ->

					% Hopefully it is not a scope mismatch:
					trace_utils:error_fmt(
						"class_TraceAggregator:get_aggregator/2 seems unable "
						"to launch successfully the aggregator "
						"(lookup name: '~ts'; scope: ~ts).",
						[ AggRegName, LookupScope ] ),
					trace_aggregator_launch_failed

				end;

			false ->
				% Not available and not to be started:
				trace_aggregator_not_found

		end

	end,

	wooper:return_static( AggRes ).



-doc """
Returns the PID of the current trace aggregator, using specified look-up scope,
or throws an exception.

The parameter is a boolean telling whether the aggregator should be created if
not available (if true), or if this method should just return a failure
notification (if false).

Note: to avoid race conditions between concurrent calls to this static method
(e.g. due to multiple trace emitter instances created in parallel), an execution
might start with a call to this method with a blocking wait until the aggregator
pops up in registry services.

Waits a bit before giving up: useful when client and aggregator processes are
launched almost simultaneously.
""".
-spec get_aggregator( boolean(), lookup_scope() ) ->
								static_return( aggregator_pid() ).
get_aggregator( CreateIfNotAvailable, LookupScope ) ->
	case try_get_aggregator( CreateIfNotAvailable, LookupScope ) of

		AggPid when is_pid( AggPid ) ->
			wooper:return_static( AggPid );

		ErrorAtom ->
			throw( ErrorAtom )

	end.



-doc """
Deletes synchronously the trace aggregator, expected to be registered globally.
""".
-spec remove() -> static_return( 'deleted' | 'trace_aggregator_not_found' ).
remove() ->

	case naming_utils:is_registered( ?trace_aggregator_name,
			?default_trace_aggregator_lookup_scope ) of

		not_registered ->
			wooper:return_static( trace_aggregator_not_found );

		TraceAggregatorPid ->

			% WOOPER convenience:
			wooper:delete_synchronously_instance( TraceAggregatorPid ),

			wooper:return_static( deleted )

	end.



-doc """
Returns the base option for trace writing.

(static method, for sharing with for example the trace listener class)
""".
-spec get_trace_file_base_options() ->
							static_return( [ file_utils:file_open_mode() ] ).
get_trace_file_base_options() ->

	% Writes to file, as soon as 32KB or 0.5s is reached, with Unicode support
	% (apparently not specifically needed, however):
	%
	% Note: we thought that the 'raw' option shall not be added here, otherwise,
	% at least in some cases, strings such as "àéèïîôùû." would not be written
	% correctly, in the sense that the overall encoding of the file will not be
	% UTF-8 as expected, but ISO-8859... However we have not be able to
	% reproduce the problem recently anymore, so 'raw' seems perfectly fine (at
	% least now).
	%
	% No file_utils:get_default_encoding_option/0 here, otherwise a
	% double-Unicode encoding will take place:
	%
	wooper:return_static(
		[ raw, { delayed_write, _Size=32*1024, _Delay=500 } ] ).



% Other functions.


-doc "Code run by the process that monitors the aggregator, overloading-wise.".
-spec overload_monitor_main_loop( aggregator_pid() ) -> no_return().
overload_monitor_main_loop( AggregatorPid ) ->

	% Just an ad-hoc variation of
	% process_utils:message_queue_monitor_main_loop/4.

	receive

		terminate ->
			%trace_utils:info( "(overload monitor terminated)" ),
			terminated

	% Every 2s:
	after 2000 ->

			{ message_queue_len, QueueLen } =
				erlang:process_info( AggregatorPid, message_queue_len ),

			QueueLen > 5000 andalso
				trace_utils:warning_fmt( "The trace aggregator ~w is "
					"overloaded, too many traces are being sent, "
					"~B of them are still waiting to be processed.",
					[ AggregatorPid, QueueLen ] ),

			overload_monitor_main_loop( AggregatorPid )

	end.



-doc """
Code run by the process that monitors the aggregator, watchdog-wise: like a
client, tries to look-up that aggregator and checks that its PID is still alive
and the same.
""".
-spec watchdog_main_loop( registration_name(), lookup_scope(),
						  aggregator_pid(), milliseconds() ) -> no_return().
watchdog_main_loop( RegName, RegScope, AggregatorPid, MsPeriod ) ->

	receive

		terminate ->
			%trace_utils:info( "(aggregator watchdog terminated)" ),
			terminated

	after MsPeriod ->

			% First check whether the registered process still matches:
			case naming_utils:is_registered( RegName, RegScope ) of

				not_registered ->

					ErrorMsg = text_utils:format( "Watchdog did not find trace "
						"aggregator '~ts' in naming service (scope: ~ts; "
						"expected PID: ~w).",
						[ RegName, RegScope, AggregatorPid ] ),

					trace_utils:error( ErrorMsg ),

					% Trying nevertheless:
					class_TraceEmitter:send_direct( error, ErrorMsg,
						_BinEmitterCategorization=?watchdog_emitter_categ,
						AggregatorPid ),

					% And still trying afterwards:
					watchdog_main_loop( RegName, RegScope, AggregatorPid,
										MsPeriod );


				% Matching PID:
				AggregatorPid ->

					% Mimicking the Erlang VM standard messages:
					Msg = text_utils:format( "ALIVE: watchdog found trace "
						"aggregator ~w (registration name: '~ts'; "
						"scope: ~ts) available.",
						[ AggregatorPid, RegName, RegScope ] ),

					%trace_utils:debug( Msg ),
					class_TraceEmitter:send_direct( info, Msg,
						_BinEmitterCategorization=?watchdog_emitter_categ,
						AggregatorPid ),

					watchdog_main_loop( RegName, RegScope, AggregatorPid,
										MsPeriod );


				OtherAggregatorPid ->

					Msg = text_utils:format( "Watchdog found trace aggregator "
						"(registration name: '~ts'; scope: ~ts) available, but "
						"as ~w, instead of the expected ~w.",
						[ RegName, RegScope, OtherAggregatorPid,
						  AggregatorPid ] ),

					class_TraceEmitter:send_direct( warning, Msg,
						_BinEmitterCategorization=?watchdog_emitter_categ,
						OtherAggregatorPid ),

					% Adopt newer aggregator then:
					watchdog_main_loop( RegName, RegScope, OtherAggregatorPid,
										MsPeriod )

	 end

end.



% Some defines.


% Columns in text traces have fixed width, in characters (total: 110).
% In this mode, by default, emitter categorization, location and message
% categorization are not written.


% For Pid (e.g. locally, <0.33.0>):
-define( pid_width, 8 ).


% For EmitterName (e.g. "First soda machine"):
-define( emitter_name_width, 12 ).


% For EmitterCategorization (e.g. "TimeManagement"):
%-define( emitter_categorization_width,12 ).
-define( emitter_categorization_width, 0 ).


% For Tick (e.g. unknown, 3169899360000) or any application-specific timestamp:
-define( app_timestamp_width, 14 ).


% For Time (e.g. "18/6/2009 16:32:14"):
-define( time_width, 10 ).


% For Location (e.g. "soda_deterministic_integration_run@a_example.org"):
%-define( location_width,12 ).
-define( location_width, 0 ).


% For MessageCategorization (e.g. "Execution.Uncategorized"):
%-define( message_categorization_width, 4 ).
-define( message_categorization_width, 0 ).


% For Priority (e.g. warning):
-define( priority_width, 7 ).


% For Message:
-define( message_width, 45 ).




% Helper functions.


-spec initialise_supervision( wooper:state() ) -> wooper:state().
initialise_supervision( State ) ->

	% Check:
	undefined = ?getAttr(supervisor_pid),

	TraceFilename = ?getAttr(trace_filename),

	SentState = send_internal_immediate( info,
		text_utils:format( "Initialising now trace supervision for '~ts'.",
						   [ TraceFilename ] ), State ),

	MaybeSupervPid = class_TraceSupervisor:create( _MaybeWaitingPid=self(),
		TraceFilename, ?getAttr(trace_type), _TraceAggregatorPid=self() ),

	%trace_utils:debug_fmt( "Created supervisor: ~w.", [ MaybeSupervPid ] ),

	setAttribute( SentState, supervisor_pid, MaybeSupervPid ).



-doc """
Tells whether this aggregator shall be used as the default standard logger
handler.

Centralised to avoid unmatched setting/unsetting of the standard handler.
""".
-spec override_standard_logger_handler( registration_scope() ) -> boolean().

% We relaxed the "global" constraint, as we do not see anymore its necessity;
% moreover we tend to increasingly use local scopes in order to have multiple
% instances coexist independently, and we surely do not want to miss any
% (standard) log message that would be only in erlang.log.* files; so:

%override_standard_logger_handler( _RegScope=global_only ) ->
%   true;

%override_standard_logger_handler( _RegScope=local_and_global ) ->
%   true;

%override_standard_logger_handler( _RegScope ) ->
%   false;
override_standard_logger_handler( _RegScope ) ->
	true.



-doc "Enables the aggregator watchdog.".
-spec enable_watchdog( registration_name(), lookup_scope(), seconds(),
					   wooper:state() ) -> wooper:state().
enable_watchdog( RegName, LookupScope, Period, State ) ->

	case ?getAttr(watchdog_pid) of

		undefined ->
			MsPeriod = 1000 * Period,

			% Closure used to avoid exporting the function (beware of self()):
			AggregatorPid = self(),

			send_internal_deferred( info, "Aggregator watchdog enabled "
				"for '~ts' (scope: ~ts; PID: ~w), based on a period of ~ts.",
				[ RegName, LookupScope, AggregatorPid,
				  time_utils:duration_to_string( MsPeriod ) ] ),

			WatchdogPid = ?myriad_spawn_link( fun() ->
				watchdog_main_loop( RegName, LookupScope,
									AggregatorPid, MsPeriod )
											  end ),

			setAttribute( State, watchdog_pid, WatchdogPid );

		WatchdogPid ->
			send_internal_deferred( error, "An aggregator watchdog was "
				"already enabled (as PID: ~w), newer enabling request ignored.",
				[ WatchdogPid ] )

	end.



-doc """
Sends the specified trace immediately from the aggregator itself (hence to
itself); this is done directly, in order to write it before any trace message
that would be waiting in the mailbox.

(helper)
""".
-spec send_internal_immediate( trace_severity(), message(),
							   wooper:state() ) -> wooper:state().
send_internal_immediate( TraceSeverity, Message, State ) ->

	MessageCategorization = text_utils:string_to_binary( "Trace Management" ),

	SelfSentState = executeOneway( State, send, [
		_TraceEmitterPid=self(),
		_TraceEmitterName= <<"Trace Aggregator">>,
		_TraceEmitterCategorization=text_utils:string_to_binary(
			?trace_emitter_categorization ),
		_AppTimestamp=none,
		_Time=time_utils:get_bin_textual_timestamp(),
		_Location=net_utils:localnode_as_binary(),
		MessageCategorization,
		_Priority=trace_utils:get_priority_for( TraceSeverity ),
		text_utils:string_to_binary( Message ) ] ),

	trace_utils:is_error_like( TraceSeverity ) andalso
		trace_utils:echo( Message, TraceSeverity ),

	SelfSentState.



-doc """
Sends format-based traces from the aggregator itself (hence to itself).

(helper)
""".
-spec send_internal_immediate( trace_severity(), format_string(),
						format_values(), wooper:state() ) -> wooper:state().
send_internal_immediate( TraceSeverity, MessageFormat, MessageValues, State ) ->
	Message = text_utils:format( MessageFormat, MessageValues ),
	send_internal_immediate( TraceSeverity, Message, State ).



-doc """
Sends traces immediately from the aggregator itself (hence to itself), through a
message sending, so that a trace can be sent even if at that point no trace file
is available for writing.

(helper)
""".
-spec send_internal_deferred( trace_severity(), message() ) -> void().
send_internal_deferred( TraceSeverity, Message ) ->

	MessageCategorization = <<"Trace Management">>,

	self() ! { send, [
		_TraceEmitterPid=self(),
		_TraceEmitterName= <<"Trace Aggregator">>,
		_TraceEmitterCategorization=
			text_utils:string_to_binary( ?trace_emitter_categorization ),
		_AppTimestamp=none,
		_Time=time_utils:get_bin_textual_timestamp(),
		_Location=net_utils:localnode_as_binary(),
		MessageCategorization,
		_Priority=trace_utils:get_priority_for( TraceSeverity ),
		text_utils:string_to_binary( Message ) ] },

	trace_utils:is_error_like( TraceSeverity ) andalso
		trace_utils:echo( Message, TraceSeverity ).



-doc """
Sends format-based traces from the aggregator itself (hence to itself).

(helper)
""".
-spec send_internal_deferred( trace_severity(), format_string(),
							  format_values() ) -> void().
send_internal_deferred( TraceSeverity, MessageFormat, MessageValues ) ->
	Message = text_utils:format( MessageFormat, MessageValues ),
	send_internal_deferred( TraceSeverity, Message ).



-doc """
Takes care of any header in the trace header.

(helper)
""".
-spec manage_trace_header( wooper:state() ) -> wooper:state().
manage_trace_header( State ) ->

	case ?getAttr(trace_type) of

		advanced_traces ->
			State;

		{ text_traces, _TargetFormat } ->

			Title = ?getAttr(trace_title) ++ " Execution Trace Report",

			% Builds a proper RST-compatible title layout:
			TitleText = text_utils:format(
					"~ts~n.. _table:~n~n.. contents:: Table of Contents~n~n",
					[ text_utils:generate_title(Title,1) ] )
				++ text_utils:format( "~ts", [ text_utils:generate_title(
													"Execution Context", 2 ) ] )
				++ text_utils:format( "Report generated on ~ts, "
					"from trace file ``~ts``, on host ``~ts``.~n~n",
					[ time_utils:get_textual_timestamp(),
					  ?getAttr(trace_filename), net_adm:localhost() ] )
				++ text_utils:format( "~ts",
					[ text_utils:generate_title( "Trace Begin", 2 ) ] ),

			PidLines = text_utils:format_text_for_width(
				"Pid of Trace Emitter", ?pid_width ),

			EmitterNameLines = text_utils:format_text_for_width(
				"Emitter Name", ?emitter_name_width ),

			AppTimestampLines = text_utils:format_text_for_width(
				"Application Timestamp", ?app_timestamp_width ),

			TimeLines = text_utils:format_text_for_width( "User Time",
				?time_width ),

			PriorityLines = text_utils:format_text_for_width( "Trace Type",
				?priority_width ),

			MessageLines = text_utils:format_text_for_width( "Trace Message",
				?message_width ),

			HeaderLine = format_linesets( PidLines, EmitterNameLines,
				AppTimestampLines, TimeLines, PriorityLines, MessageLines ) ,

			file_utils:write_ustring( ?getAttr(trace_file), TitleText
				++ get_row_separator() ++ HeaderLine
				++ get_row_separator( $= ) ),

			State

	end.



-doc """
Takes care of any header in the trace header.

(helper)
""".
-spec manage_trace_footer( wooper:state() ) -> wooper:state().
manage_trace_footer( State ) ->

	case ?getAttr(trace_type) of

		advanced_traces ->
			State;

		{ text_traces, _TargetFormat } ->
			file_utils:write_ustring( ?getAttr(trace_file),
				"~ts~n~nEnd of execution traces.~n"
				"~nBack to the table_ of contents "
				"and to the beginning of traces.",
				[ text_utils:generate_title( "Trace End", 2 ) ] ),
			State

	end.



-doc "Returns the typical separator between array rows.".
get_row_separator() ->
	get_row_separator( $- ).



-doc """
Returns the typical separator between array rows, with specified dash element to
represent horizontal lines.
""".
get_row_separator( DashType ) ->
	[ $+ ] ++ string:chars( DashType, ?pid_width )
		++ [ $+ ] ++ string:chars( DashType, ?emitter_name_width )
		++ [ $+ ] ++ string:chars( DashType, ?app_timestamp_width )
		++ [ $+ ] ++ string:chars( DashType, ?time_width )
		++ [ $+ ] ++ string:chars( DashType, ?priority_width )
		++ [ $+ ] ++ string:chars( DashType, ?message_width )
		++ [ $+ ] ++ "\n".



-doc "Formats the specified trace according to the specified trace type.".
-spec format_trace_for( trace_supervision_type(),
		{ emitter_pid(), bin_emitter_name(), bin_emitter_categorization(),
		  app_timestamp(), bin_time(), bin_location(),
		  bin_message_categorization(), priority(), bin_message() } ) ->
				ustring().
format_trace_for( _TraceType=advanced_traces, { TraceEmitterPid,
		TraceEmitterName, BinTraceEmitterCategorization, AppTimestamp, BinTime,
		BinLocation, BinMessageCategorization, Priority, BinMessage } ) ->
	lists:flatten(

		% For debugging, use io_lib:format/2 if wanting to crash on abnormal
		% input:
		%
		%io_lib:format(
		text_utils:format( ?advanced_traces_format,
			[ TraceEmitterPid, TraceEmitterName, BinTraceEmitterCategorization,
			  AppTimestamp, BinTime, BinLocation, BinMessageCategorization,
			  Priority, BinMessage ] ) );

format_trace_for( { _TraceType=text_traces, _TargetFormat }, { TraceEmitterPid,
		BinTraceEmitterName, _BinTraceEmitterCategorization, AppTimestamp,
		BinTime, _BinLocation, _BinMessageCategorization, Priority,
		BinMessage } ) ->

	% Not output here:
	% - TraceEmitterCategorization
	% - Location
	% - MessageCategorization

	PidLines = text_utils:format_text_for_width(
		text_utils:format( "~w", [ TraceEmitterPid ] ), ?pid_width ),

	EmitterNameLines = text_utils:format_text_for_width(
		text_utils:format( "~ts", [ BinTraceEmitterName ] ),
		?emitter_name_width ),

	% Can be a tick, an atom like 'unknown' or anything alike:
	AppTimestampLines = text_utils:format_text_for_width(
		text_utils:format( "~ts", [ AppTimestamp ] ), ?app_timestamp_width ),

	TimeLines = text_utils:format_text_for_width(
		text_utils:format( "~ts", [ BinTime ] ), ?time_width ),

	PriorityLines = text_utils:format_text_for_width(
		text_utils:format( "~w", [
			class_TraceEmitter:get_channel_name_for_priority( Priority ) ] ),
		?priority_width ),

	MessageLines = text_utils:format_text_for_width(
		text_utils:format( "~ts", [ BinMessage ] ), ?message_width ),

	format_linesets( PidLines, EmitterNameLines, AppTimestampLines, TimeLines,
					 PriorityLines, MessageLines ) ++ get_row_separator().



-doc "Formats the specified list of linesets.".
format_linesets( PidLines, EmitterNameLines, AppTimestampLines, TimeLines,
				 PriorityLines, MessageLines ) ->

	Columns = [ PidLines, EmitterNameLines, AppTimestampLines, TimeLines,
				PriorityLines, MessageLines ],

	TotalLineCount = lists:max( [ length( L ) || L <- Columns ] ),

	ColumnsPairs = [ { PidLines, ?pid_width },
					 { EmitterNameLines, ?emitter_name_width },
					 { AppTimestampLines, ?app_timestamp_width },
					 { TimeLines, ?time_width },
					 { PriorityLines, ?priority_width },
					 { MessageLines, ?message_width } ],

	%trace_utils:debug_fmt( "Column pairs:~n~p", [ ColumnsPairs ] ),

	FullLines = format_full_lines( ColumnsPairs, _Acc=[], TotalLineCount,
								   _Res=[], _CurrentLine="" ),

	string:join( FullLines, "\n" ).



-doc "Returns a list of full lines, made from the lines of each column.".
% Here we finished to handle all lines (none remaining):
format_full_lines( _Rows, _Acc=[], _RemainingLineCount=0, Res, CurrentLine ) ->
	lists:reverse( [ CurrentLine | Res ] ) ;

% Here we arrived at the end of a global line, preparing for next one:
format_full_lines( _Rows=[], Acc, RemainingLineCount, Res, CurrentLine ) ->
	format_full_lines( lists:reverse( Acc ), [], RemainingLineCount-1,
					   [ CurrentLine ++ "|" | Res ], "" );

% Here the corresponding column has no more content, just filling with spaces:
format_full_lines( _Rows=[ { [], Width } | ColumnPairs ], Acc,
				   RemainingLineCount, Res, CurrentLine ) ->
	format_full_lines( ColumnPairs, [ { [], Width } | Acc ], RemainingLineCount,
		Res, CurrentLine ++ "|" ++ string:chars( $\ ,Width ) );

% Here the corresponding column has content, just adding it:
format_full_lines( _Rows=[ { [ Line | OtherLines ], Width } | ColumnPairs ],
				   Acc, RemainingLineCount, Res, CurrentLine ) ->
	format_full_lines( ColumnPairs, [ { OtherLines, Width } | Acc ],
					   RemainingLineCount, Res, CurrentLine ++ "|" ++ Line ).



-doc """
Formats directly, typically client-side, a message from the specified
information, assuming a trace supervision type of type 'advanced_traces'.

Doing so should be more efficient that sending as messages separate parameters:
the resulting binary should be smaller than the sum of these terms, and the
processing is shifted from a singleton (the trace aggregator) to the many
emitters involved.

The target trace supervision type is the only degree of freedom that intervenes
(the only attribute from the aggregator that is to be taken into account).
""".
-spec format_as_advanced_trace( emitter_pid(), bin_emitter_name(),
	bin_emitter_categorization(), app_timestamp(), bin_time(), bin_location(),
	bin_message_categorization(), priority(), bin_message() ) ->
		static_return( bin_string() ).
format_as_advanced_trace( TraceEmitterPid, BinTraceEmitterName,
		BinTraceEmitterCategorization, AppTimestamp, BinTime, BinLocation,
		BinMessageCategorization, Priority, BinMessage ) ->

	% Like done by receiving primitives:
	cond_utils:if_defined( traces_check_types,
		begin

			% As AppTimestamp :: any():
			type_utils:check_pid( TraceEmitterPid ),
			type_utils:check_integer( Priority ),
			type_utils:check_binaries( [ BinTraceEmitterName,
				BinTraceEmitterCategorization, BinTime, BinLocation,
				BinMessage ] ),

			( is_atom( BinMessageCategorization )
				orelse is_binary( BinMessageCategorization ) ) orelse
					throw( { non_binary_message_categorization,
							 BinMessageCategorization } )

		end ),

	wooper:return_static( text_utils:bin_format( ?advanced_traces_format,
		[ TraceEmitterPid, BinTraceEmitterName, BinTraceEmitterCategorization,
		  AppTimestamp, BinTime, BinLocation, BinMessageCategorization,
		  Priority, BinMessage ] ) ).



-doc """
Opens the specified trace file for writing from scratch.

(helper)
""".
open_trace_file( TraceFilename ) ->

	%trace_utils:debug_fmt( "Creating trace file '~ts'.", [ TraceFilename ] ),

	% 'exclusive' not needed:
	file_utils:create_preserving( TraceFilename,
								  get_trace_file_base_options() ).



-doc """
Reopens the specified trace file for writing from last position.

(helper)
""".
reopen_trace_file( TraceFilename ) ->

	%trace_utils:debug_fmt( "Reopening trace file '~ts'.", [ TraceFilename ] ),

	% 'exclusive' not wanted:
	file_utils:open( TraceFilename,
					 [ append | get_trace_file_base_options() ] ).



% (helper)
-spec rotate_trace_file( wooper:state() ) ->
								option( { file_name(), wooper:state() } ).
rotate_trace_file( State ) ->

	BinTracePath = ?getAttr(trace_filename),

	% Equality allows to support unconditionality (with a null size):
	case file_utils:get_size( BinTracePath ) >= ?getAttr(rotation_min_size) of

		true ->
			send_internal_immediate( _TraceSeverity=info,  "Rotating '~ts'.",
									 [ BinTracePath ], State ),

			RotCount = ?getAttr(rotation_count),

			ArchiveFilePath = text_utils:format( "~ts.~B.~ts", [ BinTracePath,
				RotCount, time_utils:get_textual_timestamp_for_path() ] ),

			TraceFile = ?getAttr(trace_file),

			% Close is hopefully a strictly synchronous operation, yet we
			% prefer to be very defensive:
			%
			file:sync( TraceFile ),

			file_utils:close( TraceFile ),

			file_utils:move_file( BinTracePath, ArchiveFilePath ),

			CompressedFilePath = file_utils:compress( ArchiveFilePath ),

			file_utils:remove_file( ArchiveFilePath ),

			send_internal_deferred( debug,
				"Trace rotation done: archive '~ts' generated "
				"(original '~ts' removed).",
				[ CompressedFilePath, BinTracePath ] ),

			NewTraceFile = open_trace_file( BinTracePath ),

			RotatedState = setAttributes( State, [
				{ trace_file, NewTraceFile },
				{ rotation_count, RotCount+1 } ] ),

			{ CompressedFilePath, RotatedState };

		false ->
			undefined

	end.



-doc """
Allows inspecting the trace messages, which are often copied and/or sent over
the network, hence must be of minimal size.

Use with: make traceManagement_run $BATCH|grep '(list)' to ensure that binaries
are used whenever possible, instead of strings.

(helper)
""".
inspect_fields( FieldsReceived ) ->

	AllVars = lists:flatmap( fun( F ) ->
								[ F, F, type_utils:get_type_of( F ) ]
							 end,
							 FieldsReceived ),

	trace_utils:info_fmt( "~n"
		"- TraceEmitterPid: '~w', i.e. '~p' (~ts)~n"
		"- TraceEmitterName: '~w', i.e. '~p' (~ts)~n"
		"- TraceEmitterCategorization: '~w', i.e. '~p' (~ts)~n"
		"- AppTimestamp: '~w', i.e. '~p' (~ts)~n"
		"- Time: '~w', i.e. '~p' (~ts)~n"
		"- Location: '~w', i.e. '~p' (~ts)~n"
		"- MessageCategorization: '~w', i.e. '~p' (~ts)~n"
		"- Priority: '~w', i.e. '~p' (~ts)~n"
		"- Message: '~w', i.e. '~p' (~ts)~n",
		AllVars ).



-doc """
Tells whether this aggregator was built with the traces being activated
(presumably at least most BEAMs are expected to be built with that same
setting).

(helper)
""".
-spec is_tracing_activated() -> boolean().


-ifdef(tracing_activated).

is_tracing_activated() ->
	true.

-else. % tracing_activated

is_tracing_activated() ->
	false.

-endif. % tracing_activated

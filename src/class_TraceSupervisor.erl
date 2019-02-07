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
%
-module(class_TraceSupervisor).


-define( class_description, "Trace supervisor; this version relies on the "
		 "advanced traces, often monitored thanks to LogMX (http://logmx.com) "
		 "to track the default execution trace file, expected to be locally "
		 "available on disk." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


% Describes the class-specific attributes:
-define( class_attributes, [

	{ trace_filename, file_utils:file_path(),
	  "the name of the file where traces are to be stored (ex: *.traces)" },

	{ trace_type, trace_type(),
	  "the type of traces to be written (ex: advanced_traces)" },

	{ trace_aggregator_pid, aggregator_pid(),
	  "the PID of the supervised trace aggregator" } ] ).


-type supervisor_pid() :: pid().

-export_type([ supervisor_pid/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For TraceExtension:
-include("traces.hrl").


% For the default trace filename:
-include("class_TraceAggregator.hrl").


-define( LogPrefix, "[Trace Supervisor]" ).


% Use global:registered_names() to check supervisor presence.


%-define( LogOutput( Message, Format ),
%  trace_utils:debug_fmt( Message, Format ) ).

-define( LogOutput(Message,Format), void ).



% Total width (expressed as a number of characters) of a line of log, in text
% mode (text_traces).
%
-define( TextWidth, 110 ).


% Shorthand:

-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().



% Constructs a new trace supervisor:
%
% - {TraceFilename,TraceType,TraceAggregatorPid}:
%
%   - TraceFilename is the name of the file where traces should be read from
%
%   - TraceType the type of traces to expect (ex: advanced_traces, text_traces)
%
%   - TraceAggregatorPid is the PID of the trace aggregator, or undefined
%
% - MonitorNow tells whether the supervision should begin immediately (if true)
% or only when the monitor method is called (if false)
%
% - Blocking tells whether the monitoring should be non-blocking (if equal to
% 'none'); otherwise the monitoring should be blocking, and this Blocking
% parameter should be the PID of the caller to be notified. This parameter has a
% meaning iff MonitorNow is true
%
-spec construct( wooper:state(), { file_utils:file_name(),
		 traces:trace_supervision_type(), aggregator_pid() },
			 boolean(), 'none' | pid() ) -> wooper:state().
construct( State, { TraceFilename, TraceType, TraceAggregatorPid }, MonitorNow,
		   Blocking ) ->

	%trace_utils:debug_fmt( "~s Creating a trace supervisor, whose PID is ~w.",
	%						[ ?LogPrefix, self() ] ),

	NewState = setAttributes( State, [
		{ trace_filename, TraceFilename },
		{ trace_type, TraceType },
		{ trace_aggregator_pid, TraceAggregatorPid } ] ),

	case TraceAggregatorPid of

		AggPid when is_pid( AggPid ) ->
			% We have a PID, avoid the race condition that could happen if LogMX
			% was launched before a first trace was written by the aggregator in
			% the trace file:

			%trace_utils:trace(
			%	   "(trace supervisor waiting for the trace aggregator)" ),

			AggPid ! { requestReadyNotification, [], self() },

			receive

				{ wooper_result, trace_file_ready } ->
					%trace_utils:trace( "Trace aggregator answered." ),
					ok

			end;

		undefined ->

			%trace_utils:trace(
			%	   "(trace supervisor not waiting any trace aggregator)" ),

			% Supposedly no race condition is to be feared here:
			ok

	end,


	EndState = case MonitorNow of

		true ->

			case Blocking of

				CallerPid when is_pid( CallerPid ) ->
					% Pattern-match the result of in-place invocation:
					% ('monitor_ok' used to be temporarily replaced by '_' due
					% to the LogMX issue with java_security_PrivilegedAction)
					%
					case executeRequest( NewState, blocking_monitor ) of

						{ RequestState, monitor_ok } ->
							% Sends back to the caller:
							CallerPid ! { wooper_result, monitor_ok },
							self() ! delete,
							RequestState;

						{ AnyState, monitor_failed } ->

							% If needing to ignore a non-significant error from
							% the supervision tool:
							CallerPid ! { wooper_result, monitor_ok },
							self() ! delete,
							AnyState

							%throw( blocking_monitoring_failed )

					end;

				none ->
					% Non-blocking, handled after the constructor:
					self() ! monitor,
					NewState

			end;

		false ->
			NewState

	end,

	%trace_utils:debug_fmt( "~s Supervisor created.", [ ?LogPrefix ] ),

	EndState.




% Methods section.


% Triggers a non-blocking supervision (trace monitoring).
% Will return immediately.
%
-spec monitor( wooper:state() ) -> const_oneway_return().
monitor( State ) ->

	case ?getAttr(trace_type) of

		{ text_traces, pdf } ->
			trace_utils:info_fmt( "~s Supervisor has nothing to monitor, "
					   "as the PDF trace report will be generated only on "
					   "execution termination.", [ ?LogPrefix ] ),
			wooper:const_return();


		_Other ->

			{ Command, ActualFilename } = get_viewer_settings( State ),

			case file_utils:is_existing_file( ActualFilename ) of

				true ->
					ok;

				false ->
					trace_utils:error_fmt( "class_TraceSupervisor:monitor "
										   "unable to find trace file '~s'.",
											[ ActualFilename ] ),
					throw( { trace_file_not_found, ActualFilename } )

			end,

			trace_utils:info_fmt( "~s Supervisor will monitor file '~s' now, "
				"with '~s'.", [ ?LogPrefix, ActualFilename, Command ] ),

			Cmd = Command ++ " '" ++ ActualFilename ++ "'",

			% Non-blocking (command must be found in the PATH):
			system_utils:run_background_executable( Cmd ),

			wooper:const_return()

	end.



% Triggers a blocking supervision (trace monitoring).
%
% Will block until the viewer window is closed by the user.
%
-spec blocking_monitor( wooper:state() ) ->
							  const_request_return( 'monitor_ok' ).
blocking_monitor( State ) ->

	case ?getAttr(trace_type) of

		{ text_traces, pdf } ->
			trace_utils:info_fmt( "~s Supervisor has nothing to monitor, "
					   "as the PDF trace report will be generated only on "
					   "execution termination.", [ ?LogPrefix ] ),
			wooper:const_return_result( monitor_ok );

		_Other ->

			{ Command, ActualFilename } = get_viewer_settings( State ),

			case file_utils:is_existing_file( ActualFilename ) of

				true ->
					ok;

				false ->
					trace_utils:error_fmt(
					  "class_TraceSupervisor:blocking_monitor "
					  "unable to find trace file '~s'.", [ ActualFilename ] ),
					throw( { trace_file_not_found, ActualFilename } )

			end,

			trace_utils:info_fmt( "~s Supervisor will monitor file '~s' now "
				"with '~s', blocking until the user closes the viewer window.",
				[ ?LogPrefix, ActualFilename, Command ] ),

			% Blocking:
			case system_utils:run_executable(
				   Command ++ " '" ++ ActualFilename ++ "'" ) of

				{ _ExitStatus=0, _Output } ->
					trace_utils:info_fmt(
					  "~s Supervisor ended monitoring of '~s'.",
					  [ ?LogPrefix, ActualFilename ] ),
					wooper:const_return_result( monitor_ok );

				{ ExitStatus, ErrorOutput } ->
					trace_utils:error_fmt(
						"The monitoring of trace supervisor failed "
						"(error ~B): '~s'.", [ ExitStatus, ErrorOutput ] ),

					% Must not be a blocking error:
					%wooper:const_return_result( monitor_failed )
					%throw( trace_supervision_failed )
					wooper:const_return_result( monitor_ok )

			end

	end.




% Static section.


% Creates the trace supervisor with default settings regarding trace filename,
% start mode (immediate here, not deferred) and trace type (advanced ones here,
% not text based), with no PID specified for the trace aggregator, and blocks
% until closed.
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create() -> static_return( supervisor_pid() ).
create() ->

	SupervisorPid = create( _Blocking=true ),

	wooper:return_static( SupervisorPid ).



% Creates the trace supervisor, then blocks iff Blocking is true, with default
% settings regarding trace filename, start mode (immediate here, not deferred)
% and trace type (advanced ones here, not text based), with no PID specified for
% the trace aggregator.
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create( boolean() ) -> static_return( supervisor_pid() ).
create( Blocking ) ->

	SupervisorPid = create( Blocking, ?trace_aggregator_filename ),

	wooper:return_static( SupervisorPid ).



% Creates the trace supervisor, then blocks iff Blocking is true, with default
% settings regarding start mode (immediate here, not deferred) and trace type
% (advanced ones here, not text based), with no PID specified for the trace
% aggregator.
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create( boolean(), file_utils:file_name() ) ->
					static_return( supervisor_pid() ).
create( Blocking, TraceFilename ) ->

	SupervisorPid = create( Blocking, TraceFilename, _TraceType=advanced_traces,
							_TraceAggregatorPid=undefined ),

	wooper:return_static( SupervisorPid ).




% Creates the trace supervisor, then blocks iff Blocking is true, with default
% settings regarding start mode (immediate here, not deferred).
%
% See create/5 for a more in-depth explanation of the parameters.
%
-spec create( boolean(), file_utils:file_name(),
			  traces:trace_supervision_type(), maybe( aggregator_pid() ) ) ->
					static_return( supervisor_pid() ).
create( Blocking, TraceFilename, TraceType, TraceAggregatorPid ) ->

	SupervisorPid = create( Blocking, _MonitorNow=true, TraceFilename, TraceType,
							TraceAggregatorPid ),

	wooper:return_static( SupervisorPid ).




% Creates a trace supervisor:
%
% - Blocking tells whether the monitoring should be blocking (if true) or not
% (the supervisor tool is then launched in the background)
%
% - MonitorNow tells whether the monitoring should start immediately or only
% when a monitor/blocking_monitor method is called
%
% - TraceFilename the trace file to monitor
%
% - TraceType the expected type of the traces (ex: advanced_traces, text_traces)
%
% - TraceAggregatorPid is either the PID of the trace aggregator, or the
% 'undefined' atom
%
-spec create( boolean(), boolean(), file_utils:file_name(),
			 traces:trace_supervision_type(), maybe( supervisor_pid() ) ) ->
					static_return( supervisor_pid() ).
create( Blocking, MonitorNow, TraceFilename, TraceType, TraceAggregatorPid ) ->

	BlockingParam = case Blocking of

		true ->
			self() ;

		false ->
			none

	end,

	SupervisorPid = new_link( { TraceFilename, TraceType, TraceAggregatorPid },
							  MonitorNow, BlockingParam ),

	wooper:return_static( SupervisorPid ).




% Use the --batch option (ex: erl --batch, or with the make system 'make
% MY_TARGET CMD_LINE_OPT="--batch") to disable the use of the trace supervisor.
%
-spec init( file_utils:file_name(), traces:trace_supervision_type(),
		aggregator_pid() ) ->
			static_return( 'no_trace_supervisor_wanted' | supervisor_pid() ).
init( TraceFilename, TraceType, TraceAggregatorPid ) ->

	% By default (with no specific option) a synchronous supervisor is wanted
	% (wait for its launch to complete):

	% One '-' already eaten:
	case executable_utils:is_batch() of

		true ->
			% Option specified to disable the supervisor:
			trace_utils:info_fmt( "Application trace file is '~s'; no "
								  "interactive supervision requested.",
								  [ TraceFilename ] ),
			wooper:return_static( no_trace_supervisor_wanted );

		false ->
			% Default: a trace supervisor is used.
			%trace_utils:info( "Supervisor enabled." ),
			SupervisorPid = create( _BlockingSupervisor=true, TraceFilename,
									TraceType, TraceAggregatorPid ),
			%trace_utils:debug( "Waiting for trace supervisor to be closed." )

			wooper:return_static( SupervisorPid )

	end.



% Waits, usually at the end of a test, for any trace supervisor to be closed by
% the user.
%
-spec wait_for() -> static_return( void() ).
wait_for() ->

	case executable_utils:is_batch() of

		true ->
			% No supervisor was launched.
			% Let live the system for some time instead:
			system_utils:await_output_completion();

		false ->
			% A supervisor must be waited for:
			trace_utils:info(
			  "(waiting for the user to stop the trace supervision)" ),

			receive

				{ wooper_result, monitor_ok } ->
					%trace_utils:info(
					%    "Notification received from supervisor." ),
					% Not {test,app}_info, as used in both contexts:
					class_TraceEmitter:send_standalone( info,
						"Traces successfully monitored." )

			end

	end,

	wooper:return_static( void ).



% Helper section.


% Returns the path of the tool and the corresponding file that should be used to
% monitor traces.
%
% (helper)
%
-spec get_viewer_settings( wooper:state() ) ->
					{ file_utils:path(), file_utils:file_name() }.
get_viewer_settings( State ) ->

	Filename = ?getAttr(trace_filename),

	case ?getAttr(trace_type) of

		advanced_traces ->
			{ executable_utils:get_default_trace_viewer_path(), Filename };

		{ text_traces, text_only } ->
			{ executable_utils:get_default_wide_text_viewer_path( ?TextWidth ),
			  Filename };

		{ text_traces, pdf } ->

			PdfTargetFilename = file_utils:replace_extension( Filename,
													?TraceExtension, ".pdf" ),

			{ executable_utils:get_default_pdf_viewer_path(),
			  PdfTargetFilename }

	end.

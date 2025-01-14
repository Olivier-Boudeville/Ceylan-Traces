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

-module(traces_for_apps).

-moduledoc """
This module gathers all the code that allows to **lighten the trace macros for
(Myriad) applications**.
""".



-export([ app_start/2, app_start/3, app_start/4,
		  app_stop/3, app_immediate_stop/2, app_stop_on_shell/2 ]).


-define( trace_emitter_categorization, "application.life-cycle" ).


% For app_info_fmt and al:
-include("traces_app_header.hrl").


% For TraceType:
-include("traces.hrl").


-include("class_TraceSupervisor.hrl").


-include("traces_app_footer.hrl").



% Type shorthands:

-type module_name() :: basic_utils:module_name().

-type aggregator_pid() :: class_TraceAggregator:aggregator_pid().

-type initialise_supervision() ::
		class_TraceAggregator:initialise_supervision().



-doc """
Starts the specified application.

To be called notably from the counterpart macro.

The trace supervisor can be requested to be initialized now or not at all, or
later (typically only once the desired filename for the traces file will be
known for good, i.e. at its first renaming).

Here we disable explicitly the trapping of EXIT signals, as a function run
through "erl -eval" or through "erl -run" (like our apps) will be executed in a
process that will silently trap EXIT signals, which would mean that the crash of
any process created from the app, even thanks to spawn_link, would most probably
remain unnoticed (just leading to an EXIT message happily sitting in the mailbox
of the app process).

The resulting trace aggregator will be registered globally (only).
""".
-spec app_start( module_name(), initialise_supervision() ) -> aggregator_pid().
app_start( ModuleName, InitTraceSupervisor ) ->
	app_start( ModuleName, InitTraceSupervisor, _DisableExitTrapping=true ).



-doc """
Starts specified (Myriad - not specifically related to OTP) application,
deciding whether EXIT messages shall be trapped.

The trace supervisor can be requested to be initialized now or not at all, or
later (typically only once the desired filename for the traces file will be
known for good, i.e. at its first renaming).

The trapping of EXIT messages may be disabled (by setting DisableExitTrapping to
true), typically in most tests / cases (see comments in app_start/2). However it
may also be left as it is, notably when this function is executed from a
supervisor (see traces_bridge_sup:init/1), whose trapping of EXITs shall not be
altered (otherwise, for example, shutdowns may freeze).

The resulting trace aggregator will be registered globally (only).
""".
-spec app_start( module_name(), initialise_supervision(), boolean() ) ->
						aggregator_pid().
% All values possible for InitTraceSupervisor here:
app_start( ModuleName, InitTraceSupervisor, DisableExitTrapping ) ->
	app_start( ModuleName, InitTraceSupervisor, DisableExitTrapping,
			   _AggRegScope=global_only ).



-doc """
Starts specified (Myriad - not specifically related to OTP) application,
deciding whether EXIT messages shall be trapped and the registration scope of
the resulting trace aggregator.

The trace supervisor can be requested to be initialized now or not at all, or
later (typically only once the desired filename for the traces file will be
known for good, i.e. at its first renaming).

The trapping of EXIT messages may be disabled (by setting DisableExitTrapping to
true), typically in most tests / cases (see comments in app_start/2). However it
may also be left as it is, notably when this function is executed from a
supervisor (see traces_bridge_sup:init/1), whose trapping of EXITs shall not be
altered (otherwise, for example, shutdowns may freeze).
""".
-spec app_start( module_name(), initialise_supervision(), boolean(),
				 naming_utils:registration_scope() ) -> aggregator_pid().
% All values possible for InitTraceSupervisor here:
app_start( ModuleName, InitTraceSupervisor, DisableExitTrapping,
		   AggRegScope ) ->

	% See also the comments of app_start/2:
	case DisableExitTrapping of

		true ->
			erlang:process_flag( trap_exit, false );

		false ->
			% No changing the status regarding the trapping of EXITs, whatever
			% it is currently.
			ok

	end,

	% Create first, synchronously (to avoid race conditions), a trace
	% aggregator.
	%
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be already created at start-up).

	AppIsBatch = executable_utils:is_batch(),

	%trace_utils:debug_fmt( "At app_start/2: AppIsBatch=~ts, "
	%   "InitTraceSupervisor=~ts.", [ AppIsBatch, InitTraceSupervisor ] ),

	TraceFilename = traces:get_trace_filename( ModuleName ),

	% Not wanting the trace aggregator to initialize the trace supervisor, as
	% otherwise the latter would notify that its monitoring is over to the
	% former, whereas we want instead the calling process (i.e. the application)
	% to be notified of it (see app_stop/2):
	%
	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		TraceFilename, ?TraceType, ?TraceTitle, AggRegScope, AppIsBatch,
		_AggInitTraceSupervisor=false ),

	case ModuleName of

		traces_via_otp ->
			?app_info( "Starting the Ceylan-Traces application from an "
					   "OTP context." );

		_ ->
			?app_info_fmt( "Starting application ~ts.", [ ModuleName ] )

	end,

	% So we trigger the supervisor launch by ourselves:
	%
	% (e.g. InitTraceSupervisor could have been set to 'later')
	( not AppIsBatch ) andalso ( InitTraceSupervisor =:= true ) andalso
		begin
			TraceAggregatorPid ! { launchTraceSupervisor, [], self() },
			receive

				{ wooper_result, _SupervisorPid } ->
					ok

			end

		end,

	%trace_utils:debug( "(Ceylan-Traces started)" ),

	TraceAggregatorPid.



-doc """
Stops the specified (Myriad - not specifically related to OTP) application,
waiting for the trace supervisor if requested.

To be called from the counterpart macro.
""".
-spec app_stop( module_name(), aggregator_pid(), boolean() ) -> no_return().
app_stop( ModuleName, TraceAggregatorPid, WaitForTraceSupervisor ) ->

	% As app_start might have been called with InitTraceSupervisor=false.

	%trace_utils:info_fmt( "Application stopping (aggregator: ~w, wait "
	%    "supervisor: ~ts).", [ TraceAggregatorPid, WaitForTraceSupervisor] ),

	WaitForTraceSupervisor andalso class_TraceSupervisor:wait_for(),

	%trace_utils:info( "Going for immediate stop of Traces." ),

	% Stop trace sent there:
	app_immediate_stop( ModuleName, TraceAggregatorPid ).



-doc """
Stops specified (Myriad - not specifically related to OTP) application
immediately, not waiting for any trace supervisor, stopping the trace
aggregator, and finishing on the shell.

To be called from the counterpart macro.
""".
-spec app_immediate_stop( module_name(), aggregator_pid() ) -> no_return().
app_immediate_stop( ModuleName, TraceAggregatorPid ) ->

	%trace_utils:info( "Immediate stop of Traces." ),

	% Stop trace sent there:
	app_stop_on_shell( ModuleName, TraceAggregatorPid ),

	%trace_utils:info( "Finishing." ),

	app_facilities:finished().



-doc """
Stops specified (Myriad - not specifically related to OTP) application, stopping
the trace aggregator and finishing on the shell.

To be called from the counterpart macro, directly or not.
""".
-spec app_stop_on_shell( module_name(), aggregator_pid() ) -> no_return().
app_stop_on_shell( ModuleName, TraceAggregatorPid ) ->

	?app_info_fmt( "Stopping application ~ts.", [ ModuleName ] ),

	% Also possible: class_TraceAggregator:remove(),

	% Variable shared through macro use:
	TraceAggregatorPid ! { synchronous_delete, self() },

	receive

		{ deleted, TraceAggregatorPid } ->
			ok

	end,

	traces:check_pending_wooper_results(),

	app_facilities:display( "End of application ~ts.", [ ModuleName ] ).

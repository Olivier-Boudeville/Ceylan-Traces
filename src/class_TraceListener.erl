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


% Trace listener, similar to a remote supervisor.
%
% This version just uses LogMX (http://logmx.com) to track the default execution
% trace file, which will be synchronized automatically: history will be
% retrieved under a zipped form from the aggregator, and next traces will be
% sent directly to this listener as well as to the aggregator.
%
% So the corresponding trace aggregator must have been run with the LogMX trace
% type beforehand (log_mx_traces).
%
-module(class_TraceListener).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, TraceAggregatorPid, CloseListenerPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).



% Method declarations.
-define( wooper_method_export, monitor/1, addTrace/2, onMonitoringOver/2 ).


% Static method declarations (to be directly called from module):
-export([ create/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-define(LogPrefix,"[Trace Listener]").


%-define( LogOutput( Message, Format ), io:format( Message, Format ) ).
-define( LogOutput( Message, Format ), void ).



% Implementation notes:
%
% In some (presumably rare) cases, LogMX may display a message twice. To
% compare traces for equality, one should preferably compare directly *.traces
% files.

% We cannot block the listener process with a synchronous execution of LogMX, as
% this would prevent the listener to receive new trace messages. So we shall
% launch LogMX in the background. However then the closing of LogMX cannot be
% detected anymore, and the listener will not shutdown automatically once local
% supervision is over. To solve this, LogMX is now launched on a separate
% process, waiting (synchronously) for LogMX, sending a message back to the
% listener when closed. Then the listener can unregister from the aggregator
% (important) and terminate.


% We want the trace listeners to have the *exact* same traces as the aggregator.



% Class-specific attributes:
%
% - supervision_waiter_pid :: pid() allows to keep track of the process in
% charge of waiting for the trace interface to be closed
%
% - close_listener_pid :: pid() | 'undefined' corresponds to the process (if
% any) to notify the listener is terminating (typically so that the calling
% application can itself terminate afterwards)



% Constructs a new trace listener.
%
% TraceAggregatorPid is the PID of the trace aggregator to which this listener
% will be synchronized.
%
-spec construct( wooper:state(), pid(), pid() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	io:format( "~s Creating a trace listener whose PID is ~w, "
			   "synchronized on trace aggregator ~w.~n",
			   [ ?LogPrefix, self(), TraceAggregatorPid ] ),

	% First the direct mother classes (none), then this class-specific actions:

	io:format( "~s Requesting from aggregator a trace synchronization.~n",
			   [ ?LogPrefix ] ),

	TraceAggregatorPid ! { addTraceListener, self() },

	% We used to rely on basic ZIP sent over Erlang messages:
	%receive
	%
	%	 { trace_zip, Bin, TraceFilename } ->
	%
	%			% Allows to run for the same directory as aggregator:
	%			ListenerTraceFilename = "Listener-" ++ TraceFilename,
	%
	%           file_utils:zipped_term_to_unzipped_file( Bin,
	%										 ListenerTraceFilename ),
	%	{ trace_zip, ErrorReason } ->
	%
	%		io:format( "~s Trace listener cannot listen to current trace "
	%				   "aggregator, as this aggregator does not use "
	%				   "LogMX-based traces.~n", [ ?LogPrefix ] ),
	%
	%		throw( { cannot_listen_aggregator, TraceAggregatorPid,
	%				 ErrorReason } )

	% Now we prefer XZ + sendFile:

	% Currently we prefer using a temporary directory (it also allows to avoid
	% stepping on the source compressed file if running the listener from the
	% same directory as the aggregator - as it is the case for tests):
	%
	TempDir = file_utils:create_temporary_directory(),

	CompressedFilename = net_utils:receive_file( TraceAggregatorPid, TempDir ),

	TraceFilename = file_utils:decompress( CompressedFilename,
										   _CompressionFormat=xz ),

	file_utils:remove_file( CompressedFilename ),

	io:format( "~s Received from aggregator a trace synchronization "
			   "for file '~s', reused for later traces.~n",
			   [ ?LogPrefix, TraceFilename ] ),

	% Will write in it newly received traces (sent through messages):
	%File = file_utils:open( TraceFilename,
	%			[ append, raw, { delayed_write, _Size=1024, _Delay=200 } ] ),
	File = file_utils:open( TraceFilename, [ append ] ),

	NewState = setAttributes( State, [
				{ trace_aggregator_pid, TraceAggregatorPid },
				{ trace_filename, TraceFilename },
				{ trace_file, File },
				{ temp_dir, TempDir },
				{ supervision_waiter_pid, undefined },
				{ close_listener_pid, CloseListenerPid }
									 ] ),

	EndState = executeOneway( NewState, monitor ),

	io:format( "~s Trace listener created.~n", [ ?LogPrefix ] ),

	EndState.




% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	io:format( "~s Deleting trace listener.~n", [ ?LogPrefix ] ),

	% Class-specific actions:

	% Important message, to avoid loading the aggregator with sendings to
	% defunct listeners:
	%
	?getAttr(trace_aggregator_pid) ! { removeTraceListener, self() },

	file_utils:close( ?getAttr(trace_file) ),

	file_utils:remove_file( ?getAttr(trace_filename) ),

	file_utils:remove_directory( ?getAttr(temp_dir) ),


	case ?getAttr(close_listener_pid) of

		P when is_pid( P ) ->
			%io:format( "Notifying close listener of deletion.~n" ),
			P ! { trace_listening_finished, self() };

		_ ->
			ok

	end,

	io:format( "~s Trace listener deleted.~n", [ ?LogPrefix ] ),

	% Allow chaining:
	State.




% Methods section.


% Triggers an asynchronous supervision (trace monitoring).
%
% Will return immediately.
%
% Note: directly inspired from class_TraceSupervisor.erl: monitor/1 and
% blocking_monitor/1.
%
% (oneway)
%
-spec monitor( wooper:state() ) -> oneway_return().
monitor( State ) ->

	Filename = ?getAttr(trace_filename),

	case file_utils:is_existing_file( Filename ) of

		true ->
			ok;

		false ->
			error_logger:error_msg( "class_TraceListener:monitor "
									"unable to find trace file '~s'.~n",
									[ Filename ] ),
			trace_file_not_found

	end,

	io:format( "~s Trace listener will monitor file '~s' with LogMX now.~n",
			   [ ?LogPrefix, Filename ] ),

	Self = self(),

	WaiterPid = spawn_link( fun() ->

		% Blocking this waiter process (logmx.sh must be found in the PATH):
		case system_utils:run_executable(
			   executable_utils:get_default_trace_viewer_path() ++ " '"
			   ++ Filename ++ "'" ) of

				{ _ExitCode=0, _Output } ->
					io:format( "~s Trace listener ended the monitoring "
							   "of '~s'.~n", [ ?LogPrefix, Filename ] );

				{ ExitCode, ErrorOutput } ->
					error_logger:error_msg( "The trace listening failed "
											"(error code: ~B): ~s.~n",
											[ ExitCode, ErrorOutput ] )

		end,

		% Unblock the listener:
		Self ! { onMonitoringOver, self() }

							end ),


	SupState = setAttribute( State, supervision_waiter_pid, WaiterPid ),

	?wooper_return_state_only( SupState ).



% Registers a new pre-formatted trace in trace file.
% To be called by the trace aggregator.
%
% (oneway)
%
-spec addTrace( wooper:state(), binary() ) -> oneway_return().
addTrace( State, NewTrace ) ->

	io:format( ?getAttr(trace_file), "~s", [ binary_to_list( NewTrace ) ] ),

	?wooper_return_state_only( State ).



% Callback triggered when the waiter process detected that the supervision tool
% has been closed.
%
% (oneway)
%
-spec onMonitoringOver( wooper:state(), pid() ) -> oneway_return().
onMonitoringOver( State, WaiterPid ) ->

	% Check:
	WaiterPid = ?getAttr(supervision_waiter_pid),

	self() ! delete,

	?wooper_return_state_only( State ).



% 'Static' methods (module functions):


% Creates the trace listener that will synchronize itself to the specified
% aggregator.
%
% (static)
%
-spec create( pid() ) -> pid().
create( AggregatorPid ) ->

	% No link here, not wanting to take down the whole system because of a
	% listener:
	%
	new( AggregatorPid, _CloseListenerPid=undefined ).

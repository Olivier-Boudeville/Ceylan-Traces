% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Saturday, July 20, 2019.


% Module implementing the root supervisor of Traces.
%
% In practice, it will supervise a single process, the one of the (singleton)
% trace aggregator.
%
-module(traces_sup).


% The trace aggregator is not a gen_server but a WOOPER instance, therefore a
% supervisor bridge (provided by this module) is needed in order to connect this
% aggregator to an OTP supervision tree.
%
% As a result, the process whose code is defined in the current module behaves
% like a real supervisor to its own supervisor, but has a different interface
% than a real supervisor to the Traces subsystem.
%
% Hence used for (optional) OTP compliance (see
% http://erlang.org/doc/man/supervisor_bridge.html).
%
-behaviour(supervisor_bridge).



% User API:
-export([ start_link/1 ]).


% Callback of the supervisor_bridge behaviour:
-export([ init/1, terminate/2 ]).



-define( supervisor_name, ?MODULE ).


-define( application_module_name, traces_via_otp ).



% Starts and links the Traces root supervisor bridge.
-spec start_link( boolean() ) -> term().
start_link( TraceSupervisorWanted ) ->

	trace_utils:debug( "Starting the Traces root supervisor." ),

	supervisor_bridge:start_link( { local, ?supervisor_name },
						  _Module=?MODULE, _Args=[ TraceSupervisorWanted ] ).


% Callback to initialise this supervisor bridge.
init( [ TraceSupervisorWanted ] ) ->

	trace_utils:trace_fmt( "Initializing the Traces supervisor bridge "
		"(trace supervisor wanted: ~s).", [ TraceSupervisorWanted ] ),

	% Not trapping EXITs explicitly:
	TraceAggregatorPid = traces_for_apps:app_start(
		_ModuleName=?application_module_name, TraceSupervisorWanted ),

	{ ok, TraceAggregatorPid, _State=TraceAggregatorPid }.



% Callback to terminate this supervisor bridge.
terminate( Reason, _State=TraceAggregatorPid )
  when is_pid( TraceAggregatorPid ) ->

	trace_utils:trace_fmt(
	  "Terminating the Traces supervisor bridge (reason: ~w, "
	  "trace aggregator: ~w).", [ Reason, TraceAggregatorPid ] ),

	% Works whether or not a trace supervisor is used:
	traces_for_apps:app_stop( _ModuleName=?application_module_name,
							  TraceAggregatorPid ).

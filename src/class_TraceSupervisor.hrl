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


% Note: the test_info macro is expected to be defined prior to the
% class_TraceSupervisor.hrl inclusion (ex: included from test_constructs.hrl).
	

% Allows to use an interactive trace supervisor (LogMX) if true, otherwise a
% silent execution.


% Expected to be already done:
%-include("test_constructs.hrl").



% Defines trace filename, if not already specified:
-ifndef(TraceFilename).
	-define(TraceFilename,"Ceylan-trace-test" ++ ?TraceExtension ).
-endif.




% Use the --batch option (ex: erl --batch) to disable the use of the trace
% supervisor:
-define( init_trace_supervisor,
	% By default (with no specific option) a synchronous supervisor is wanted
	% (wait for its launch to complete):
	
	% One '-' already eaten:
	case init:get_argument('-batch') of
		{ok,_} ->
			% Option specified to disable the supervisor:
			%io:format( "Supervisor disabled.~n" ),
			no_trace_supervisor_wanted;
			
		_ ->
			% Default: a trace supervisor is used.
			%io:format( "Supervisor enabled.~n" ),			
			class_TraceSupervisor:create( _BlockingSupervisor=true, 
										 ?TraceFilename, ?TraceType )
			%io:format( "Waiting for trace supervisor to be closed.~n" )		
	end			
).



-define( wait_for_any_trace_supervisor,
	case init:get_argument('-batch') of
	
		{ok,_} ->
			% No supervisor was launched.
			% Let live the system for some time instead:
			timer:sleep(1000);
		
		_ ->
			% A supervisor must be waited for:
			io:format( "(waiting for the user to stop the trace supervision)~n"
					  ),
			receive

				{wooper_result,monitor_ok} ->
					%io:format( "Notification received from supervisor.~n" ),
					?test_info( "Traces successfully monitored." )

			end
	end
).


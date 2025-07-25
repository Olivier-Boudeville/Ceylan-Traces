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


% The name under which the trace aggregator process is to be registered:
% (use ?trace_aggregator_name to have it)
%
-define( trace_aggregator_name, ceylan_trace_aggregator ).


% The default registration scope (must correspond to default_trace):
-define( default_trace_aggregator_registration_scope, global_only ).

% The default look-up scope (must correspond to
% default_trace_aggregator_registration_scope):
%
-define( default_trace_aggregator_lookup_scope, global_otherwise_local ).


% For TraceExtension:
% Should be already done: -include("traces.hrl").


% The name of the file to which the aggregator will write the traces that it
% receives:
%
-define( trace_aggregator_filename, "Ceylan-traces" ++ ?TraceExtension ).


% The name of the (pseudo) module to rely on for the naming of the trace file
% produced by the trace aggregator, in an OTP-related application context.
%
-define( otp_application_module_name, traces_via_otp ).

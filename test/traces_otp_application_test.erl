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
% Creation date: Tuesday, August 6, 2019.


% Testing of Traces as an OTP active application, directly from within its code
% base (hence without needing to create a separate, mock-up test release for
% that).
%
-module(traces_otp_application_test).


% For run/0 export and al:
-include_lib("myriad/include/test_facilities.hrl").


% For TraceType:
-include("traces.hrl").


% Actual test:
test_traces_application( OrderedAppNames ) ->

	test_facilities:display( "Starting the Traces OTP active application." ),
	otp_utils:start_applications( OrderedAppNames ),


	test_facilities:display( "Traces version: ~p.",
				 [ system_utils:get_application_version( traces ) ] ),

	% To test also a Traces module:

	class_TraceEmitter:send_from_test( info, "Sent from this OTP test!" ),

	%traces:manage_supervision(),


	test_facilities:display( "Stopping the Traces application." ),
	otp_utils:stop_applications( OrderedAppNames ),

	test_facilities:display(
	  "Successful end of test of the Traces OTP application." ).



% Note that the traces.app, wooper.app and myriad.app files will have to be
% found and used for this test to succeed: Traces, WOOPER and Myriad must be
% already available as prerequisite, fully-built OTP applications.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Build root directory from which prerequisite applications may be found:
	BuildRootDir = "..",

	OrderedAppNames = [ myriad, wooper, traces ],

	case otp_utils:prepare_for_test( OrderedAppNames, BuildRootDir ) of

		ready ->
			test_traces_application( OrderedAppNames ) ;

		{ lacking_app, _App } ->
			% (a detailed warning message has been issued by
			% otp_utils:prepare_for_test/2)
			%
			ok

	end,

	test_facilities:stop().

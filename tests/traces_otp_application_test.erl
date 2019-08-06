% Copyright (C) 2019-2019 Olivier Boudeville
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


% Testing of Traces as an OTP active application.
-module(traces_otp_application_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For TraceType:
-include("traces.hrl").


% Actual test:
test_traces_application( TracesEBinPath, WOOPEREBinPath, MyriadEBinPath ) ->

	code_utils:declare_beam_directories( [ TracesEBinPath, WOOPEREBinPath,
										   MyriadEBinPath ] ),

	test_facilities:display( "Starting the Traces application." ),

	% Was expecting that starting the dependencies would be automatic,
	% apparently it is not the case (as without an explicit starting, we have:
	% '{error,{not_started,wooper}}'); moreover it visibly should be done before
	% entering traces_app:start/2, so:
	%
	ok = application:start( myriad ),
	ok = application:start( wooper ),
	ok = application:start( traces ),

	test_facilities:display( "Traces version: ~p.",
				 [ system_utils:get_application_version( traces ) ] ),

	% To test also a Traces module:

	class_TraceEmitter:send_from_test( info, "Sent from this OTP test!" ),

	%traces:manage_supervision(),

	test_facilities:display( "Stopping the Traces application." ),

	ok = application:stop( traces ),
	ok = application:stop( wooper ),
	ok = application:stop( myriad ),

	test_facilities:display(
	  "Successful end of test of the Traces application." ).



% Note that the ebin application directory must be in the code path for the
% traces.app file to be found and used, and for this test to succeed.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Supposing here that all Ceylan applications are built, in the usual _build
	% directory of Traces (not in their respective usual directories such as
	% ../../Ceylan-{Myriad,WOOPER}), with the default rebar3 profile (hence
	% typically after a 'make rebar3-application'):
	%
	BaseEBinPath = [ "..", "_build", "default", "lib" ],

	TracesEBinPath = file_utils:join( BaseEBinPath ++ [ "traces", "ebin" ] ),

	case file_utils:is_existing_directory_or_link( TracesEBinPath ) of

		true ->

			MyriadEBinPath = file_utils:join( BaseEBinPath ++ [ "myriad", "ebin" ] ),

			case file_utils:is_existing_directory_or_link( MyriadEBinPath ) of

				true ->

					WOOPEREBinPath = file_utils:join( BaseEBinPath
													  ++ [ "wooper", "ebin" ] ),

					case file_utils:is_existing_directory_or_link(
						   WOOPEREBinPath ) of

						true ->
							test_traces_application( TracesEBinPath,
											WOOPEREBinPath, MyriadEBinPath ) ;

						false ->
							trace_utils:warning_fmt(
							  "No build directory found for the WOOPER parent "
							  "application (searched for '~s'), "
							  "stopping this test (run beforehand "
							  "'make rebar3-compile' at the root of the "
							  "WOOPER source tree for a more relevant testing).",
							  [ WOOPEREBinPath ] )

					end;

				false ->
					trace_utils:warning_fmt(
					  "No build directory found for the Myriad parent "
					  "application (searched for '~s'), stopping this test "
					  "(run beforehand 'make rebar3-compile' at the root "
					  "of the Myriad source tree for a more relevant testing).",
					  [ MyriadEBinPath ] )

			end;


		false ->
			trace_utils:warning_fmt( "No build directory found for the Traces "
				"application (searched for '~s'), stopping this test "
				"(run beforehand 'make rebar3-compile' at the root of the "
				"source tree for a more relevant testing).", [ TracesEBinPath ] )

	end,

	test_facilities:stop().

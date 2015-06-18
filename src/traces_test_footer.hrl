% Copyright (C) 2003-2015 Olivier Boudeville
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
% Creation date: Tuesday, January 11, 2011



% Defines all the macros and exports useful for trace-using tests.
%
% Allows to define exports before functions: macros and functions have been
% split so that overall header files can be defined which start with all macros
% and finish with all function definitions.




% Define functions as late as possible:

-ifndef(TracingActivated).



% Allows to avoid warnings about variables not be used when traces are disabled:
%
-spec test_trace_disabled( any() ) -> 'test_trace_disabled'.
test_trace_disabled( _ ) ->
	test_trace_disabled.



-spec test_trace_disabled( any(), any() ) -> 'test_trace_disabled'.
test_trace_disabled( _, _ ) ->
	test_trace_disabled.



-endif.

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2010-2018, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(conf_winpath, []).

/** <module> Configure location of external programs on Windows

ClioPatria relies on two external  executables   that  are  not standard
available on Windows: *|git.exe|* for  providing version information and
*|dot.exe|*, part of GraphViz for rendering graphs.

These programs must either  be  in   %PATH%  or  made accessible through
user:file_search_path/2. The latter can be by  means of hard-coded paths
or using the dynamic approach implemented   below.  This hopefully works
out-of-the-box in most installations. If it   fails for you, please help
improving this config and/or add a  hard-coded location. See the example
below.

@see http://git-scm.com/ for installing git
@see http://www.graphviz.org/ for installing the graphviz programs
*/

:- multifile
    user:file_search_path/2.
:- dynamic
    user:file_search_path/2.

% Add hardcoded locations if these programs   are  in weird places. Note
% that currently (May 2011), you cannot   specify UNC (//share/..) paths
% for GIT. Although Prolog can  find   git.exe,  git.exe cannot find its
% components on UNC paths.

% user:file_search_path(path, 'E:/Git').
user:file_search_path(path, Dir) :-
    prog_in_dir('dot.exe', Dir).
user:file_search_path(path, Dir) :-
    prog_in_dir('git.exe', Dir).

%!  candidate_prog_dir(-Dir) is nondet.
%
%   Propose candidate locations for installing   programs. The first
%   two are described by Windows.  The   next  takes the location of
%   SWI-Prolog, assuming that  other  programs   might  be  near and
%   finally we deal with old Windows versions.

candidate_prog_dir(Dir) :-
    getenv('PROGRAMFILES', Dir).
candidate_prog_dir(Dir) :-
    getenv('ProgramFiles(x86)', Dir).
candidate_prog_dir(Dir) :-
    current_prolog_flag(executable, Exe),
    file_directory_name(Exe, PlBinDir),
    file_directory_name(PlBinDir, PlHomeDir),
    file_directory_name(PlHomeDir, Dir).
candidate_prog_dir('C:/Program Files').

%!  prog_dir_pattern(+Prog, -Pattern) is nondet.
%
%   Pattern is a partial pattern for   expand_file_name/2 to find an
%   executable below a place where programs are normally installed.

prog_dir_pattern('git.exe', 'Git/bin').
prog_dir_pattern('dot.exe', 'Graphviz*/bin').


%!  prog_in_dir(+Prog, -Directory) is semidet.
%
%   True if Prog  resides  in  Directory.   This  is  used  to  find
%   candidate directories for  the  =path=   alias  in  Windows. The
%   search   is   further   guided   by   candidate_prog_dir/1   and
%   prog_dir_pattern/2.

:- dynamic
    dir_cache/2,
    resolving/1.

prog_in_dir(Prog, _) :-                 % Break loop
    resolving(Prog), !, fail.
prog_in_dir(Prog, Dir) :-
    (   dir_cache(Prog, Cached)
    ->  Cached = dir(Dir)
    ;   setup_call_cleanup(asserta(resolving(Prog), Ref),
                           absolute_file_name(path(Prog), _,
                                              [ access(read),
                                                file_errors(fail)
                                              ]),
                           erase(Ref))
    ->  asserta(dir_cache(Prog, nodir)),
        fail
    ;   prog_in_dir_no_cache(Prog, Computed)
    ->  asserta(dir_cache(Prog, dir(Computed))),
        Dir = Computed
    ;   asserta(dir_cache(Prog, nodir)),
        fail
    ).


prog_in_dir_no_cache(Prog, Dir) :-
    candidate_prog_dir(Dir0),
    prog_dir_pattern(Prog, SubDirPattern),
    atomic_list_concat([Dir0, /, SubDirPattern, /, Prog], Pattern),
    expand_file_name(Pattern, Files0),
    reverse(Files0, Files),         % Last one might be last version
    member(File, Files),
    access_file(File, read),
    file_directory_name(File, Dir).


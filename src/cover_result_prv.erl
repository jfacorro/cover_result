-module(cover_result_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, cover_result).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 cover_result"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Display cover result in the command-line"},
            {desc, "Display cover result in the command-line"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    report(),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

report() ->
  CoverSpecPath = case os:getenv("COVER_SPEC") of
                    false ->
                      throw("COVER_SPEC environment variable missing.");
                    Value ->
                      Value
                  end,
  {ok, CoverSpec} = file:consult(CoverSpecPath),
  CoverDataPath = case proplists:get_value(export, CoverSpec) of
                    undefined ->
                      throw("No 'export' config in Cover specifications.");
                    X -> X
                    end,

  file:set_cwd(filename:dirname(CoverSpecPath)),
  cover:import(CoverDataPath),

  {result, Ok, Fail} = cover:analyze(module),

  io:format("~n~s==== Code Coverage ====~s~n~n",
            [color("white-bold"), color("reset")]),
  [report_result(Result) || Result <- lists:sort(Ok)],

  case Fail of
    [] -> ok;
    _ ->
      io:format("~nCoverage analysis failed in the following modules: "),
      [io:format("~p, ", [Module]) || {_, Module} <- lists:sort(Fail)]
  end,

  io:format("~n~s=======================~s~n~n",
            [color("white-bold"), color("reset")]).

report_result({Module, {0, 0}}) ->
  report_result({Module, {1, 0}});
report_result({Module, {Cov, NotCov}}) ->
  Coverage = erlang:round(100 * Cov / (Cov + NotCov)),
  Color = get_color(Coverage),
  Reset = color("reset"),
  io:format("[~s% ~3.. B~s]\t~p~n", [Color, Coverage, Reset, Module]).

get_color(Coverage) when Coverage == 100 ->
  color("green-bold");
get_color(Coverage) when Coverage > 90 ->
  color("green");
get_color(Coverage) when Coverage > 70 ->
    color("yellow");
get_color(Coverage) when Coverage > 50 ->
    color("red-bold");
get_color(_Coverage) ->
    color("red").

color(Name) ->
  Colors = #{"red" => "\e[0;31m",
             "red-bold" => "\e[1;31m",
             "green" => "\e[0;32m",
             "green-bold" => "\e[1;32m",
             "yellow" => "\e[1;33m",
             "white" => "\e[0;37m",
             "white-bold" => "\e[1;37m",
             "reset" => "\e[0m"},
  maps:get(Name, Colors).

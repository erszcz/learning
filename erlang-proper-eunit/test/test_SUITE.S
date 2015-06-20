{module, test_SUITE}.  %% version = 0

{exports, [{module_info,0},
           {module_info,1},
           {prop_1,0},
           {prop_1_test_,0},
           {test,0}]}.

{attributes, []}.

{labels, 16}.


{function, prop_1, 0, 2}.
  {label,1}.
    {line,[{location,"test_SUITE.erl",7}]}.
    {func_info,{atom,test_SUITE},{atom,prop_1},0}.
  {label,2}.
    {allocate_zero,1,0}.
    {move,{integer,10},{x,1}}.
    {move,{integer,0},{x,0}}.
    {line,[{location,"test_SUITE.erl",8}]}.
    {call_ext,2,{extfunc,proper_types,integer,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,15},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"test_SUITE.erl",8}]}.
    {call_ext_last,2,{extfunc,proper,forall,2},1}.


{function, prop_1_test_, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,test_SUITE},{atom,prop_1_test_},0}.
  {label,4}.
    {make_fun2,{f,12},0,0,0}.
    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{integer,7}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    return.


{function, test, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,test_SUITE},{atom,test},0}.
  {label,6}.
    {move,{atom,test_SUITE},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,eunit,test,1}}.


{function, module_info, 0, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,test_SUITE},{atom,module_info},0}.
  {label,8}.
    {move,{atom,test_SUITE},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,test_SUITE},{atom,module_info},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{atom,test_SUITE},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-prop_1_test_/0-fun-0-', 0, 12}.
  {label,11}.
    {line,[]}.
    {func_info,{atom,test_SUITE},{atom,'-prop_1_test_/0-fun-0-'},0}.
  {label,12}.
    {allocate,0,0}.
    {line,[]}.
    {call,0,{f,2}}.
    {move,nil,{x,1}}.
    {line,[]}.
    {call_ext,2,{extfunc,proper,quickcheck,2}}.
    {test,is_eq_exact,{f,13},[{x,0},{atom,true}]}.
    {deallocate,0}.
    return.
  {label,13}.
    {line,[]}.
    {badmatch,{x,0}}.


{function, '-prop_1/0-fun-0-', 1, 15}.
  {label,14}.
    {line,[{location,"test_SUITE.erl",8}]}.
    {func_info,{atom,test_SUITE},{atom,'-prop_1/0-fun-0-'},1}.
  {label,15}.
    {move,{atom,true},{x,0}}.
    return.

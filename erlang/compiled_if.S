{module, compiled_if}.  %% version = 0

{exports, [{condition,0},
           {f,0},
           {f2,0},
           {module_info,0},
           {module_info,1},
           {some_val,0}]}.

{attributes, []}.

{labels, 18}.


{function, condition, 0, 2}.
  {label,1}.
    {line,[{location,"compiled_if.erl",5}]}.
    {func_info,{atom,compiled_if},{atom,condition},0}.
  {label,2}.
    {allocate,0,0}.
    {move,{integer,2},{x,0}}.
    {line,[{location,"compiled_if.erl",6}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {bif,'==',{f,0},[{integer,1},{x,0}],{x,0}}.
    {deallocate,0}.
    return.


{function, f, 0, 4}.
  {label,3}.
    {line,[{location,"compiled_if.erl",8}]}.
    {func_info,{atom,compiled_if},{atom,f},0}.
  {label,4}.
    {allocate,0,0}.
    {line,[{location,"compiled_if.erl",9}]}.
    {call,0,{f,2}}.
    {select_val,{x,0},{f,7},{list,[{atom,true},{f,5},{atom,false},{f,6}]}}.
  {label,5}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,6}.
    {test,is_boolean,{f,7},[{x,0}]}.
    {move,{atom,not_ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,7}.
    {line,[{location,"compiled_if.erl",10}]}.
    if_end.


{function, some_val, 0, 9}.
  {label,8}.
    {line,[{location,"compiled_if.erl",15}]}.
    {func_info,{atom,compiled_if},{atom,some_val},0}.
  {label,9}.
    {move,{integer,123},{x,0}}.
    return.


{function, f2, 0, 11}.
  {label,10}.
    {line,[{location,"compiled_if.erl",18}]}.
    {func_info,{atom,compiled_if},{atom,f2},0}.
  {label,11}.
    {allocate,0,0}.
    {line,[{location,"compiled_if.erl",19}]}.
    {call,0,{f,9}}.
    {gc_bif,'rem',{f,12},1,[{integer,3},{x,0}],{x,1}}.
    {test,is_eq,{f,12},[{x,1},{integer,0}]}.
    {move,{atom,equals_zero},{x,0}}.
    {deallocate,0}.
    return.
  {label,12}.
    {gc_bif,'rem',{f,13},1,[{integer,3},{x,0}],{x,1}}.
    {test,is_ne,{f,13},[{x,1},{integer,0}]}.
    {move,{atom,does_not_equal_zero},{x,0}}.
    {deallocate,0}.
    return.
  {label,13}.
    {line,[{location,"compiled_if.erl",20}]}.
    if_end.


{function, module_info, 0, 15}.
  {label,14}.
    {line,[]}.
    {func_info,{atom,compiled_if},{atom,module_info},0}.
  {label,15}.
    {move,{atom,compiled_if},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 17}.
  {label,16}.
    {line,[]}.
    {func_info,{atom,compiled_if},{atom,module_info},1}.
  {label,17}.
    {move,{x,0},{x,1}}.
    {move,{atom,compiled_if},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.

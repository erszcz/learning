```
1> tryof:error_from_condition().
{caught,{error,error_from_condition}}
2> tryof:error_from_clause().
** exception error: error_from_clause
     in function  tryof:error_from_clause/0 (tryof.erl, line 11)
3> tryof:error_case_clause()/
3> .
* 2: syntax error before: '.'
3> tryof:error_case_clause().
** exception error: no try clause matching z
     in function  tryof:error_case_clause/0 (tryof.erl, line 16)
```

# Eureka!

There's more to opaque types than it seems at the first sight.
Consider the following:

-   when `#xmlel{}` is defined as `-type` we get this dialyzer error:

    ```
    The call usage_example:xmlel(#{#<105>(8, 1, 'integer', ['unsigned', 'big']), #<110>(8, 1, 'integer', ['unsigned', 'big']), #<118>(8, 1, 'integer', ['unsigned', 'big']), #<97>(8, 1, 'integer', ['unsigned', 'big']), #<108>(8, 1, 'integer', ['unsigned', 'big']), #<105>(8, 1, 'integer', ['unsigned', 'big']), #<100>(8, 1, 'integer', ['unsigned', 'big'])}#, [], [<<_:144>>,...]) does not have a term of type [#xmlel{name::binary(),attrs::[any()],children::[#xmlel{name::binary(),attrs::[any()],children::[any()]} | eet_cdata:t()]} | eet_cdata:t()] (with opaque subterms) as 3rd argument
    ```

-   when `#xmlel{}` is defined as `-opaque` we get this instead:

    ```
    The call eet_el:new(#{#<105>(8, 1, 'integer', ['unsigned', 'big']), #<110>(8, 1, 'integer', ['unsigned', 'big']), #<118>(8, 1, 'integer', ['unsigned', 'big']), #<97>(8, 1, 'integer', ['unsigned', 'big']), #<108>(8, 1, 'integer', ['unsigned', 'big']), #<105>(8, 1, 'integer', ['unsigned', 'big']), #<100>(8, 1, 'integer', ['unsigned', 'big'])}#, [], [<<_:144>>,...]) does not have a term of type [eet_cdata:t() | eet_el:t()] (with opaque subterms) as 3rd argument
    ```

Specifically, the gain is:

```erlang
[eet_cdata:t() | eet_el:t()]
```

versus:

```erlang
[#xmlel{name::binary(),attrs::[any()],children::[#xmlel{name::binary(),attrs::[any()],children::[any()]} | eet_cdata:t()]} | eet_cdata:t()]
```

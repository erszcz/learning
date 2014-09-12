-module(circle_area).

-export([from_diameter/1,
         from_radius/1]).

-export_type([circle_area/0]).

-record(circle_area_v1, {area}).
-opaque circle_area() :: #circle_area_v1{}.

-spec from_diameter(number()) -> circle_area().
from_diameter(D) ->
    from_radius(D / 2).

-spec from_radius(number()) -> circle_area().
from_radius(R) ->
    #circle_area_v1{area = math:pi() * math:pow(R, 2)}.

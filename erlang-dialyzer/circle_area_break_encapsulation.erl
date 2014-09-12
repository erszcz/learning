-module(circle_area_break_encapsulation).

-compile([export_all]).

main() ->
    Area = circle_area:from_radius(3.5),
    NumArea = element(2, Area).

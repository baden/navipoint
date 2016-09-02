-module(navipoint_sub_protocol).

-callback init(any(), any()) -> {atom(), any(), any()}.
-callback get(map:map()) -> map:map().
-callback post(binary(), map:map()) -> map:map().

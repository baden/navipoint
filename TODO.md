## Диализ

Проблема во внешней библиотеке, спек для navidb:get описан неверно:

```
navipoint_params.erl:18: The call navidb:get('params',{<<_:16>>,_}) breaks the contract (Collection::atom(),Keys::[binary()]) -> [document()]
navipoint_params.erl:42: The call navidb:get('params',{'id',_}) breaks the contract (Collection::atom(),Keys::[binary()]) -> [document()]
```

Пока просто ссылки насыпом:

## XREF

- https://github.com/inaka/cowboy-swagger/blob/master/Makefile
- https://github.com/inaka/cowboy-swagger/search?utf8=%E2%9C%93&q=xref
- http://erlang.org/doc/apps/tools/xref_chapter.html
- https://github.com/sparrell/sFractals/blob/80f8ac5a933e0239187a7c546b450e1a2b4bc04d/xref.config
- https://github.com/inaka/xref_runner


## Elixir

- https://github.com/botsunit/elixir.mk
- https://github.com/botsunit/elixir.mk/tree/master/sample

## Стоит посмотреть на другие плагины erlang.mk

- https://erlang.mk/guide/plugins_list.html
- https://github.com/bullno1/reload.mk/tree/b51d89654a140679aeb07190670642243d3974e4

## Найти lint

- https://atom.io/packages/linter-erlang
- https://atom.io/packages/linter-erlc
- https://github.com/inaka/elvis

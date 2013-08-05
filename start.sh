#!/bin/bash 

erl -pa deps/*/ebin ebin -eval "application:start(sasl)" -eval "application:start(compiler)" -eval "application:start(syntax_tools)" -eval "application:start(crypto)" -eval "application:start(lager)" -eval "application:start(ranch)" -eval "application:start(cowboy)" -eval "application:start(wsexample)"

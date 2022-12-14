#!/bin/sh

sbcl --eval "(asdf:load-system :advent-of-code-2022-in-common-lisp)" \
     --eval "(2022-day-14::part-1)"

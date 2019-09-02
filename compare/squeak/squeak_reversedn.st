| default num res |
default := 20000000.
num := (Smalltalk commandLine arguments ifEmpty: [default] ifNotEmpty: [:a | a first asNumber]).
res := [num valuesBenchReversedn] on: Error do: [1].
Smalltalk quitPrimitive: res.

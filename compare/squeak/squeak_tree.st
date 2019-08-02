| default num res |
default := 18000000.
num := (Smalltalk commandLine arguments ifEmpty: [default] ifNotEmpty: [:a | a first asNumber]).
res := [num valuesBenchTree] on: Error do: [1].
Smalltalk quitPrimitive: res.

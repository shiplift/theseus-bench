| default num res |
default := 20000000.
num := (Smalltalk commandLine arguments ifEmpty: [default] ifNotEmpty: [:a | a first asNumber]).
res := [num valuesBenchCollect] on: Error do: [1].
Smalltalk quitPrimitive: res.
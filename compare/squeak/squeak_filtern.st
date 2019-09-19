| default num res |
default := 25000000.
num := (Smalltalk commandLine arguments ifEmpty: [default] ifNotEmpty: [:a | a first asNumber]).
res := [num valuesBenchFiltern] on: Error do: [1].
Smalltalk quitPrimitive: res.

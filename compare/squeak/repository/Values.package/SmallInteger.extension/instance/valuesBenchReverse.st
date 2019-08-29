*Values-lambcompare
valuesBenchReverse

	| e lst1 lst2 res start end gcAfter gcBefore |
	e := VVector new.
	lst1 := VCons withAll: (Array new: self withAll: e).
	lst2 := VCons withAll: (Array new: self withAll: e).
	Smalltalk garbageCollect.
	gcBefore := RSqueak gcTimings.
	start := Time utcMicrosecondClock.
	res := lst1 reverse.
	end := Time utcMicrosecondClock.
	gcAfter := RSqueak gcTimings.
	RSqueak reportBenchStartedAt: start until: end gcBefore: gcBefore after: gcAfter.
	self assert: (res isKindOf: VNil) not.
	^ 0
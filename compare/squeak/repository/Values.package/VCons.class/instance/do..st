collection api
do: aBlock

	| cons |
	cons := self.
	[cons isEmpty] whileFalse: 
		[cons isCons ifFalse: [^ self error: 'Not a proper list'].
		aBlock value: cons car.
		cons := cons cdr].

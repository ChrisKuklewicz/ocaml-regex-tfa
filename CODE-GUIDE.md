Code guide, started 2011-10-23 to recall what the pieces are doing.

* common.ml

Defines collection of types that underpin the tagged approach.
Actually not far from the Haskell regex-tdfa forms.
The repTasks is part of the new machinery to handle repeats with counting.
I have now commented out some more items that were unused at present.

* monoid.ml

Used by whichtest in WhichTestMonoid.
TODO needs to the module signature.

* pattern.ml

"pattern.ml defines the pattern type for storing parsed regular expression"
This is, usefully, a fairly correct-by-construction parse tree.
It manages to use sexplib to explicitly convert uchar and ustring.

Note: There is a bug in USet.fold_range, r is always the same as l; worked around in code.

TODO: check if the bug is still there

* readPattern.ml

Hand written parser for regular expressions into pattern.ml structure.

* whichTest.ml

This defines collections of tests, as a map from the test type to a boolean and a list of pattern indexes.  The boolean is the value of the test that constitutes a pass, a boolean True indicates the test should be true to pass.

This defines "nullView" which will be core of defining how zero-character-accepting possibilities get considered separately from character-accepting possibilities.

This also defines "dominates" which establishes a partial order useful for avoiding redundant checking of conditions.  With a plethora conditions to check it would be an optimization to use binary decisions diagrams.

* corePattern.ml

Defines corePattern and coreQ and does a few passes to convert pattern.ml to this structure.  This was a conversion from a single pass using lazy Haskell.  The passes also compute the tags array, groups array, and a depth count (see type coreResult).

This allocates and a mildly optimized assignment of tags.  The corePattern/coreQ is a more flexible syntax tree than the fairly correct-by-construction parse tree.  It is a busy structure, 

Also, USet to and from sexp are defined.

* simulate.ml

Walk the corePattern.ml to match against some text.  Do this in a depth-first manner, with exponential backtracking to cover all possibilities.  Only compare the collections of all results at the end to pick the single desired result.  This works and passes the standard test suite.

The meaning of much of the work in corePattern.ml can be seen in this module's code.  The maintenance of the mutable state of the possible match is largely done by doTagTask and doRepTask.   The combined call using doTasks is employed by the null matching code.  These recorded tags are ultimately interpreted by the compareHistory operation which is used to sort all the possible ways to match the text.

There is some subtlety on display in doRepTask, where the topCount computed by corePattern.ml is used to limit the number of distinguished states.  This is one of the refinements that makes it possible to avoid a huge number of explicit NFA states to cope with counted repeating patterns.

* simStep.ml

This is a baby step past simulate.ml that does no backtracking.  This takes the input one character at a time (with the rather explicit stepData type).

The system runs each possible match forward until it accepts the new character at some OneChar node.  The history map uses a key which is the pattern index of the OneChar node and the count of active repetitions, at most one possibility is kept for each such key.  Logically the possibilities are stored in a NFA nodes which are at post-OneChar positions.

The uses of copyHistory show where possibilities branch.  The "cycle" implements a variant of the mutable history swapping the the Haskell regex-tdfa engine uses.

Handling the end of the input stream is the work of the *End operations which are near-duplicates of the main operations.  This final cleanup pass must run through the history and try to finish the possibilities.  Winners get shipped out by simStep and sorted by the uWrap driver of simStep, which is itself wrapped by wrapSimStep.

* saveContext.ml

unfinished improved engine.  Actually probably bits of at least two engines.

* TODO

Get a proper design for the next engine after simStep.

Building the context stack is quite wasteful, this ought to be replaced.  Use the same post-OneChar model.  When a new character arrives pull out each possibility from the history and make it fly with a return call.  The trick is the Seq handling (?).  Coming up from qFront means that qFront has accepted and now can go to qBack as what was called SimEnterAny so null is possible.  Coming from outside the Seq to qBack meant that there is a live char that must be handled and force doEnter.

The trouble seems to be the interaction between doEnter and Seq handling.  Each doEnter is required to accept at least 1 character.  So each recursive doEnter creates a new logical "hasAccepted" flag.  The Or nodes split the possibility and then act as having a single contained node.  The Repeat node acts as its contained node, as does CaptureGroup.  The OneChar handling sinks the possibility into the history or lets it fade.  The single contained node means the logical "hasAccepted" flag is that of the child.  The Seq node has 2 children, so a nested Seq accumulate a stack of logical "hasAccepted" flags.  The stack handled this by recording an explicit stack of SimEnterAny and SimEnterAccept records.  But SimEnterAccept is never stored in the history!

To remove these flags from the stack requires recognizing that jumping from qFront to qBack was an "optimization" that needs to be rethought.  It must go to the parent in between.  The mean that Seq nodes do not have "doEnter" and "doReturn" but rather "doEnter" and "doReturnFromFront" and "doReturnFromBack".  The doReturnFromFront means that front accepted a character via doEnter previously and is now bubbling up and qBack has the choices of SimEnterAny.  What of returning from front without accepting a character?  This must have come into Seq this same cycle via doEnter and called the equivalent of doEnterNull on qFront.  The doEnterNull does need a way to dispatch back to doEnter on qBack.  But this can be done with not dispatching from doEnterNull but rather returning a (Some history) back to doEnter on the Seq.

doEnter Seq has two behaviors:

doEnter qFront -- -- this leads to doReturnFromFront
(if doEnterNull qFront is Some history then doEnter qBack else ())  -- -- this should lead to doReturnFromBack

doReturnFromFront Seq has two behaviors:

(if doEnterNull qBack is Some history then leave Seq and goto next item after Seq else ())
doEnter qBack -- -- this leads to doReturnFromBack

doReturnFromBack Seq has one behavior:

leave Seq and goto next item after Seq

To simplify the code the doReturnFromBack could be the normal doReturn and doReturnFromFront would be the very special case for Seq only.

With this architecture the flow is slightly more explicit and the continuation in coreQ will indicate doReturnFromFront as a special case.

The Note flag between NoNote and NoteNoLoop can be handled with a not-dissimilar change.  The NoteNoLoop is only set before doEnterNull.  Thus NoteNoLoop is never stored in the history stack!  So the same change to doEnterNull as in Seq applies: make it return (Some history).

The third user of doEnterNull is spark.  This can also handle (Some history) coming back.
The fourth use of doEnterNull was dispatch on SimEnterAny but this is now doReturnFromFront.
Thus doEnterNull can by wholly changed into the form of returning (Some history).

This design seems ambitious enough for the next phase of the code.

----

For single pass over the tree?  Repeat is the killer.

Logically pass to a node the incoming bundle of possibilities (because of repetition counts).

If these possibilities are real then need a real history to pass to contents of repeat, but this ought to be compared to looping things coming out of the repeat. The two-pass solution becomes 2^depth passes, so this is sad.  Cannot handle cake before it is cooked.

If these possibilities are merely indexes? Then the Repeat node can create new indexes to be filled in with a real histories later.  The tree is then processed in the first pass by recording what is done to each index.  The second pass can do the actual work.  This sounds like fully building the lazy graph and then reducing it.  The point is that it will not be 2^depth.

Perhaps: Store the history bundles indexed by real patIndex values.  On first pass the node receives virtual indexes.  The patIndex and virtual index represent bundles, not just single possibilities.  The Repeat can allocate a new virtual index to store the result of merging its looping output with the incoming indexes.

The Or node may allocate a new virtual index to be the merge of its branches, but it can probably count the number of successful branches and avoid this for a single branch.

The indexes must be passed in with an operation list attached.

For a single pass not over the tree? Go to a good NFA first? The fancy repeat nodes may make this NFA hard to build.  Would need epsilon transitions.  This actually sounds OK.


----

A vision of the level beyond that has come.  The tree is walked in 2.5 passes once presented with the next character C:

The first pass "flush up" is kin to all the doReturn operations.  At the top level this provides winners that accepted a character before C and stop just before C.  The intermediate nodes store all the "flush up" information, and are able to optimally combine possibilities so as to return just one pre-rep bundle.  In particular this should allow Repeat nodes to compute the looping bundle as well as the return bundle, allow Seq nodes to store a mid-point flushed value, and allow Or nodes to immediately merge their branches.  I am speculating that: the OneChar are all logically empty after this pass, capture groups do not need to store anything, test groups are ignored.

The 0.5 pass is "spark" and this provides any zero-width winner before C.  This also computes the initial incoming bundle for the next pass.

The last pass is the "flush down" pass that is kin to the doEnter phase.  Each node has an optional incoming bundle.  The logic around Repeat nodes may get hairy, but hope endures.  This pass logically fills the appropriate OneChar and empties the intermediate nodes.

The whole tree may be processed at first, then add flags to indicate which nodes need "flush up"/"flush down".  The nature of the tree pass is in patIndex order, so storing the history as a list and annotating the range of patIndex values under each node would serve as "flush up" flags.  The "flush down" occurrence is both spark-driven and covers the "flush-up" nodes.

* simCont.ml

make the changes to simStep.ml needed to remove the context stack, with extra contTo field in the coreQ.  This was refreshingly easy after typing up this code guide.

* simFlush.ml

Use a runPattern that stores the data across the tree instead of a history map.  Do two partial traversals of the tree for each step.

Next versions should implement pre-comparison to do compression of loop histories, and should implement earliest-history tracking to allow for streaming return of disjoint matches.  The efficiency of the system looks pretty good, the early merging of histories in simFlush may keep the simCont worst case at bay.

Another enhancements will be to pass in just characters without the index attached to each one.

The Haskell code is parameterized to work on different input streams, and the OCaml could use a module functor interface to handle different input sources.

The Haskell code has engines for combinations of "front-anchored" and "non-capturing" or "testing".  The anchor testing can be either singleline or multiline, which can be a module functor parameter in OCaml.

For testing, I should make a system for checking multiple matches, going back to simulate.ml for the answer generation.  Speed testing and comparison to Haskell regex-tdfa are also interesting.

In the longest run, making a byte-processing version that converts the pattern to UTF-8 would be interesting (compare with Russ Cox's re2).

In other realms, making an OCaml inspired port back to Haskell or on to C/C++/Java/Fortran...

Another optimization: make orbit log storage be indexed by something less sparse than the getOrbit (Some o) tag.  Perhaps there is a map from getOrbit to the RRepeat node which holds the orbit log?  No. There is a need to copy the orbitlog with the history.

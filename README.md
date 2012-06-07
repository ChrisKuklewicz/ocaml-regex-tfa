This is my "learn OCaml the hard way" project.  The project is to take the great and awesome Haskell [regex-tdfa][r-t] and achieve a similar goal in OCaml.

What does it do?  POSIX ERE

It implements POSIX extended regular expressions.  By following a [proper specification][att1]: Without dark corners, without weird results with ambiguous patterns, and with a good starting [test suite][att2].

Why was my Haskell library great?  Correctness

I have tested against a few other POSIX implementations and the AT&T one, including an expanded collection of hand written tests and via random fuzz testing against them.  All of them failed at least some tests ([results][wiki1]) -- except for my regex-tdfa which has fixed.

What was Haskell library awesome?  Efficiency

By following the efficient tagged single-pass design behind the TRE library I was able to get space usage dependent on only the pattern, not the length of searched text, even while capturing the proper substring.

But then I discovered that TRE was and is quite buggy.  Some serious bugs and and TRE does not follow the great spec for "ambiguous" patterns. I had to invent a new and more complicated algorithm to handle ambiguous patterns and improve it with a new kind of comparison pass to recover the constant space usage.

What is the new OCaml version?  Redesigned

I am not porting the Haskell code, but rather creating a very different implementation of the tagged automata.

One big change is that the old Haskell design handled "(p){10,100}" by creating 100 copies of the pattern "(p)" in the automata while the new OCaml design keeps a single copy.  The Haskell code builds an NFA into a DFA while the OCaml code uses an internal syntax tree as the automata state.  This is a much clearer approach.

Is it done?  Almost

The latest engine, simFlush.ml, does not do the optimizing comparison pass and so is not yet in true constant space usage.  The potted history:

* simulate.ml

"Depth first" implementation that generated all possible matches and then picks the correct one.  Much happiness in that this passed the same unit tests as the Haskell version.

* simStep.ml

Improved so that there is no bracktacking, possibilities are pruned at each step and complete patterns are reported promptly.   A big map of saved contexts is used to keep track of possibilities.  Selecting best winner at end passes unit tests.  This really shows that the new handling of "(p){10,100}" patterns worked.

* saveContext.ml

Unfinished and abandoned step.  Took break and came back later.

* simCont.ml

Worked the design out (see CODE-GUIDE.md) for improving simStep.ml, replacing the context map with "continue-to" fields in the tree.  Each character leads to "two and a half" passes of the tree.  Tests pass.

* simFlush.ml

Store the state in mutable tree instead of a map -- this improves the traversal of the state.  Each character leads to two passes of the tree.  The tree traversal allows merging possibilities to be compared earlier which improves efficiency. ( For pattern size 'm' I think a simCont pass had O(m^2) behavior and simFlush has O(m) ).

* to be written

Add the comparison pass to compress the state, this will change the worst case space usage for text of length 'n' from O(n) to O(1).

[r-t]: http://hackage.haskell.org/package/regex-tdfa
[att1]: http://www2.research.att.com/~gsf/testregex/re-interpretation.html
[att2]: http://www2.research.att.com/~gsf/testregex/
[wiki1]: http://www.haskell.org/haskellwiki/Regex_Posix
[tre]: http://laurikari.net/tre/

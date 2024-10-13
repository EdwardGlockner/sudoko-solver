# Functional Programming 1 - 1DL330 2024
# Individual Assignment: A Sudoku Solution Counter

Introduction
------------

Your task for this assignment is to implement a checker for generalized Sudoku in Haskell.

This is an individual assignment. It is acceptable to discuss abstract solution
methods with others, and to get inspiration from the Internet or other sources,
provided

- you give proper credit (via an explicit comment in your sources) whenever you
  use someone else's ideas, and

- you have constructed your submission yourself. It is not acceptable to share
  or copy source code.

See the course's [Ethics Rules](https://uppsala.instructure.com/courses/100580/pages/ethics-rules)
for further guidelines.

Generalised Sudoku Rules
----------

This description slightly generalises that of  <https://en.wikipedia.org/wiki/Sudoku>.

In generalised Sudoku, the objective is to fill a n^2 × n^2 grid with digits so that
each column, each row, and each of the n^2 subgrids of size n × n that compose the
grid (also called "boxes", "blocks", or "regions") contains all of the numbers
from 1 to n.  A grid with this property is called a solved Sudoku. In classic Sudoku, n is 3.

A Sudoku puzzle is a partially filled grid that can be uniquely completed into a
solved Sudoku.  To check for the existence of a solution, there exist many
[Sudoku solving
algorithms](https://en.wikipedia.org/wiki/Sudoku_solving_algorithms).  They can
often easily be augmented with code that checks for failure or multiple
solutions.

Your Task
---------

Your task is to write a single file called `SudokuSolver.hs` that
declares a module `SudokuSolver`.

Your module `SudokuSolver` must import the `Sudoku` module.
This exports types `Solutions` and `Board` (with their constructors) that have been
defined as follows:

```haskell
{- The three possible numbers of solutions: 0, 1, or more than 1.
 -}
data Solutions = NoSolution | UniqueSolution | MultipleSolutions


{- A Sudoku board
   The board is represented as a list of rows,
   where each row is a list of ints,
   where 0 represents an empty cell.
   
   INVARIANTS:
   The length of the Board is the square of a positive integer.
   The board is square, so the length of a Board is the same as the length of each of its elements.
   The board contains only numbers between 0 and the length of the Board (inclusive).
 -}
type Board     = [[Int]]

```

Do not define these types yourself!

Your module `SudokuSolver` must export the following values and functions:

```haskell
author :: String
nickname :: String
numSolutions :: Board -> Solutions
```

These are specified as follows:

- `author` is your (first and last) name.

- `nickname` is an arbitrary string (of at most 20 ASCII characters) chosen by
  you to identify your AI. It will be used to report the
  [evaluation](#evaluation) results pseudonymously.


- `numSolutions` takes a Board and returns the number of ways that it can be completed into a solved Sudoku: 0, 1, or several.

Near the top of your file must be a comment containing a brief (about 100-500
words) description of your implementation. All functions must be specified
according to our [Coding Convention](https://uppsala.instructure.com/courses/100580/pages/coding-convention).

Your file must compile with GHC.

Your implementation must not perform any kind of input or output (e.g., via the
console, file system or network) or any other side effects. Your implementation
must not raise any exceptions.

Your implementation must not spawn additional threads or processes.

Your implementation may use any types and values provided by the Prelude, or by
other Haskell packages that are available in the test environment, with the
exception of [unsafe](https://wiki.haskell.org/Unsafe_functions) functions. Your
implementation must not require the installation of additional packages.

Testing
-------

We have provided several partially filled Sudoku grids in text format,
that you can evaluate your solver on.

Submission
----------

Please submit your file `SudokuSolver.hs` in Studium.
(In case of technical problems—and only then!—email your
solution to <johannes.borgstrom@it.uu.se>.) The submission deadline is **Friday,
2024-10-25 at 18:00**. (Late submissions are accepted in accordance with the
completion deadlines that have been announced on Studium.)

Evaluation
----------

Your solver will be evaluated on a range of board sizes with varying number of
empty squares [TO BE MADE MORE PRECISE, though the test boards can be seen as
indicative].  All tests will be run on one of the Linux machines that are
available for student login.

Your solver will be given 2 minutes per board.  If the solver returns the wrong
answer or runs out of time, it will receive a score of -1 for that board.  If
your AI causes stability issues due to excessive memory requirements during a
game, we may treat this similar to a timeout. (We do not expect memory usage to
be an issue in practice, and will not actively monitor it unless problems
arise.)

Grading
-------

Grading for this assignment will be according to the [grading rubric](https://uppsala.instructure.com/courses/100580/assignments/)
that is published on Studium:

If your implementation conforms to the requirements stated [above](#your-task),
performs successfully on the small (n < TBD1) [boards that have been provided](#testing) <i>and</i> follows the [Coding Convention](https://uppsala.instructure.com/courses/100580/pages/coding-convention),
your assignment grade will be (at least) 3.

If your implementation additionally is functionally correct and returns the
correct result on all small (n < TBD1) boards during evaluation, your assignment
grade will be (at least) 4.

If your implementation additionally returns the correct result on all
medium-sized boards (n = TBD2) and at least one large (n = TBD3) one, your
assignment grade will be 5.

With this grading scheme, you can *decide for yourself* whether you merely want
to use a straight-forward solution, or whether you want to implement a
(moderately) strong solver—and hence, which grade you want to aim for.

Tournament
----------

We will report the test results on Studium, using your solver's nickname to
identify it. These results may bestow bragging rights, but they will not affect
the grading.


Hints
-----

Do not try to reinvent the wheel. Instead, research existing techniques. A good
starting point is the Wikipedia page on [Sudoku solving
algorithms](https://en.wikipedia.org/wiki/Sudoku_solving_algorithms).
Remember to give credit when you use someone else's ideas.

Test your code thoroughly before you submit. Buggy code may easily be worse than
no code.

If you are aiming for a very strong solver, make sure it doesn't run out of time.

Do not go overboard. We understand that one could spend a lifetime perfecting an
Sudoku solver, while you only have a few days.

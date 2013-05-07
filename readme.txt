CIS 192 Final Project - Sudoku Solver
by Oliver Manheim (omanheim) and Daniel Cates (dancates)

Our project:
We built a program that can read in an unsolved sudoku puzzle (in a certain
format), solve it, and print out the (or, in the event that there exists more
than one, a) solution.

Running:
> runHaskell SudokuIO.hs pathToPuzzle
e.g. runHaskell SudokuIO.hs puzzles/simpleSudoku.txt

The text file must be in the following format:
"4-68-15-2
 -18-92-6-
 592-47183
 -41269-75
 76-15-249
 2594-3-1-
 1--985426
 685-249--
 92-31-857"
with quotes indicating the beginning and ending of the puzzle and dashes
indicating a blank in the starting puzzle. You are welcome to create new
text files/transcribe puzzles from the internet and pass them to our solver.

What we did:
We started out by just having the ability to read in a file and convert it
into a suitable format ([[Int]]) and then convert that back to being printed
to the terminal.  We then added some solving functionality: if there is a single
value missing from a row, column, or box, put it in.  This involved writing a 
lot of code to convert between row, column, and box-oriented formats that did
not end up being useful at all when we got to the meaty part of the sudoku
solver.  We also declared a number of exceptions that we could see being 
useful over the duration of the project. This is the work we had completed at 
the time of our checkpoint demo.

We then began to implement the AC3 algorithm at Adi's suggestion, basically 
converting what we could understand from Wikipedia into Haskell code.
We began treating each cell as a domain of possible values instead, with the
puzzle only being solved when each domain was of length 1.  The next piece of 
functionality to build in was the arc queue, which is a list of pairs of 
coordinates.  The first coordinate in the pair is the cell whose domain is 
being constrained and the second is the cell whose domain is doing the 
constraining.  Most of our problems with this part occurred here, as we were 
in many different situations not appropriately adding arcs to the queue.  We 
then added functionality to actually constrain domains based on these arcs.
At this point, we could solve a lot of different puzzles.

Finally, we added the ability for the program to "guess" if AC3 was not enough.
We would pick the cell with the smallest domain, create different board-domains 
for each value in that cell's domain, and then apply AC3.  This process would 
be repeated until the board is solved.

Over the course of the project, we learned a lot about how to think about 
problems functionally, compared to the imperative code we found on Wikipedia.
Solving this problem in Haskell required us to think in a very different way.
We also didn't really think Haskell was the right tool for this job, 
necessarily.  We used lists of lists to represent sudoku boards, and having 
access to arbitrary indices would have been helpful compared to traversing
lists in many instances.  It was really interesting, however, and made us much
better Haskell programmers in the process.
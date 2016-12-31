# N-programming-language
A stack-based lazy language designed for brevity

# Examples

## Factorial Function

```N
{N@(N1N-$1¦*$)(1)0N=$?$}$
```

This function (from the standard library) first stores its argument. It then pushes code blocks for the two cases in its if block onto the stack and conditionally evaluates one of them.

## Program that Prints Characters Not In Program

[Code Golf Question](http://codegolf.stackexchange.com/questions/12368/print-every-character-your-program-doesnt-have/12373).

```N
ql$#126#32r$-$s$
```

Pushes the program's source (`q`) then converts to a list of codepoints (`l$`), then pushes the printable ASCII set (`#126#32r$`), then subtracts the two as sets (`-$`), and converts back to a string (`s$`).

## Truth Machine

[Code Golf Question](http://codegolf.stackexchange.com/questions/62732/implement-a-truth-machine).

```N
1i$(;p$`|2&1=$?$)$
```

This reads an integer from STDIN, then runs a program that prints the top of the stack at least once but then exits if and only if its a 0.

## Sum of a List

```N
0{N@(.$N+$1¦)(`N)2&n$?$}$
```

This program takes pushes 0 to the stack, then repeatedly removes the top of the list and adds it to this number.

## Machine that Runs Indefinitely But Halts

[Code Golf Question](http://codegolf.stackexchange.com/questions/36747/if-a-program-terminates-and-there-is-no-one-to-see-it-does-it-halt).

```N
9!$!$e$∑$
```

This calculates the factorial of the factorial of 9 (a number on the order of `10^1859933`) creates a list from 0 to that number, and then computes the sum of that list.

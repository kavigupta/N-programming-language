# N-programming-language
A stack-based lazy language designed for brevity

# Examples

## Factorial Function

```N
{n@|!@(n1n-$!$*$)(1)0n=$?$$}
```

This function first stores its argument and itself in its environment. It then pushes code blocks for the two cases in its if block onto the stack and conditionally evaluates one of them.

## Program that Prints Characters Not In Program

[Code Golf Question](http://codegolf.stackexchange.com/questions/12368/print-every-character-your-program-doesnt-have/12373).

```N
ql$#126#32r$-$s$
```

Pushes the program's source (`q`) then converts to a list of codepoints (`l$`), then pushes the printable ASCII set (`#126#32r$`), then subtracts the two as sets (`-$`), and converts back to a string (`s$`).

## Truth Machine

[Code Golf Question](http://codegolf.stackexchange.com/questions/62732/implement-a-truth-machine).

```N
1i$(;p$·|2&1=$?$$)$
```

This reads an integer from STDIN, then runs a program that prints the top of the stack at least once but then exits if and only if its a 0.

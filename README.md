# scripted-brainfucc

This is a Rust implementaion for the compiler in [this kata](https://www.codewars.com/kata/59f9cad032b8b91e12000035/train/c).

## What is working

- `msg <literal | constant>+` to spit messages.

Example:
```
msg "the answer is: " 42 "\n"
```

# NOTES

**Important**: The current implementation of compiling `msg` with a constant compiles writing the text representation of the numeric value.
This is not the target for the language but I will need it so it seems like I'm doing some kind of progress. The target thing to do when presented
with a constant is to load it into a cell (casting the `usize` to a `u8` and returning error if it overflows) and then use the `.` instruction to print it.
This will make character literal constants make sense.

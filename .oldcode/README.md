## Fraction calculator.

This was originally part of a library written for a Dotnet project for a scale modelling project to aide with converting Imperial to Metric measurements.

I decided to rewrite it in Fortran as an exercise to see if I can do it, but also to learn about types.

The Makefile has been updated to compile a library for use with other Fortran programs.

## Useful functions

- addfraction - Adds two fractions together.
- subfraction - Subtracts two fractions.
- multiplyfraction - Multiplies two fractions.
- dividefraction - Inverts the second fraction and calls the multiplyfraction function to complete the division.
- lcd - Finds the lowest common denominator of two fractions.
- GCDfunction - Worker for the above function.
- mixedFraction - Converts an improper fraction to a mixed fraction, eg 5/2 to 2 1/2.
- makemixedFraction - Converts a mixed fraction to an improper fraction, eg 2 1/2 to 5/2.
- chkoverflow - Checks for integer overflow by using long integer.
- returnfraction - Returns the fraction from a string.
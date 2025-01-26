## Fraction calculator.

This was originally part of a library written for a Dotnet project for a scale modelling project to aide with converting Imperial to Metric measurements.

I decided to rewrite it in Fortran as an exercise to see if I can do it, but also to learn about types.

The Makefile has been updated to compile a library for use with other Fortran programs.

## Updates 2025

This project was originally written in 2024 keeping as close to Fortran 95 standards as possible.  Since gaining more experience, understanding and exposure to the language over the past year I have begun to incorporate newer standards and language aspects that have made my life so much easier.

The first was using [Fortran Package Manager](https://fpm.fortran-lang.org/index.html) and the next was adopting the later Fortran standards which allowed for some flexibility and creativity in solving some problems that required multiple lines of code previously and less now.

## Useful functions

- add_fraction - Adds two fractions together.
- sub_fraction - Subtracts two fractions.
- multiply_fraction - Multiplies two fractions.
- divide_fraction - Inverts the second fraction and calls the multiplyfraction function to complete the division.
- lowest_common_denom - Finds the lowest common denominator of two fractions.
- mixed_fraction - Converts an improper fraction to a mixed fraction, eg 5/2 to 2 1/2.
- approx_fraction - Creates an approximate fraction from a decimal digit.

## Public constants and datatypes

- maxdenom - 1 * 10 ** 9.
- fractiontype - A datatype that represents a fraction: unit & numerator / denominator both as 32 and 64bit integer including a status message.
- decimalpaces - The number of decimal places used to convert a decimal number to a fraction.

## Private functions

- GCDfunction - Worker for the above function.
- chkoverflow - Checks for integer overflow by using long integer.
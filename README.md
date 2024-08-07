# Enhanced Haskell Functionality

## Overview
The "Modified Prelude and Haskell Functions" project comprises a custom Haskell module `ModifiedPrelude`, which replaces the standard Haskell Prelude to provide a tailored functionality for advanced number operations and list manipulations. Accompanied by the `Assign2` module, it showcases various recursive functions and algorithms to solve problems ranging from number digit manipulation to complex list operations using the custom Prelude.

## Features
  - **Custom Prelude:** A modified version of the Haskell Prelude that specifically includes and modifies key functions like `foldl'` and `foldr` for enhanced performance and utility.
  - **Extensive Function Library:** The `Assign2` module includes a diverse set of functions demonstrating recursive programming, list processing, and arithmetic operations on large integers (`BigInt`).
  - **Test Driven Development:** Embedded examples and expected outputs for each function, allowing for easy testing and verification of functionality.

## Usage
To run the project:
  ```
  import Assign2
  
  -- To find the digital root of 9874
  main = print $ digitalRoot 9874
  ```
Use the functions in the `Assign2` module, import the module into your Haskell environment. You can then call any of the available functions by passing appropriate parameters as described in the function definitions.

## Prerequisites
  - The Glasgow Haskell Compiler (GHC), version 8.0 or above
  - An editor that supports Haskell syntax (e.g., VSCode with Haskell plugin, Atom, Sublime Text)

## Input
Input types vary based on the function being used. Typically, inputs can be integers, lists of integers, strings, or custom types like `BigInt`. Inputs are passed directly to functions as arguments.

## Output
Outputs will vary according to the function:
  - Scalar values (e.g., integer or boolean)
  - Lists or sequences of values
  - Strings representing transformed or computed results

## Notes
  - Some functions, particularly those dealing with `BigInt` or recursive list manipulations, may perform slower on very large datasets due to inherent computational complexity.
  - Minimal error handling is assumed; it is expected that inputs are validated as per function requirements prior to invocation.
  - The `ModifiedPrelude` can be further extended by adding new functions or modifying existing ones to cater to more specific or optimized use cases.
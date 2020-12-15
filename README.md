# CAS

This project simplifies and solves basic algebraic equations.

It supports only a limited set of problems, similar to those in a first high school algebra course.

Expressions are written in a TeX-like format. Operations include sums, products, quotients, powers, and functions.

Quotients are written in the TeX \frac{p}{q} operator, where p and q are both expressions.

Functions like sin(..), cos(..), tan(..), log(..), etc can be in the input, but they are not simplified or solved.

When an equation is entered as input, it is automatically simplified.
Then it is solved step-by-step, and each step is shown.

## Building

To build and run:

1. Install Stack
https://docs.haskellstack.org/en/stable/README/#how-to-install

2. In the root directory of the project ("CAS") run:
stack build

3. Then run:
stack run

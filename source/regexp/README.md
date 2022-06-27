# RegExp engine

This regexp engine should implement
[ECMAScript Regular Expression](https://tc39.es/ecma262/#sec-regexp-regular-expression-objects)
(Unicode Mode), but currently only part of specification is implemented.

For now we have:

|  RegExp                   | Description                               |
| ------------------------- | ----------------------------------------- |
| *x* *y*                   | Match the *x* then *y*                    |
| *x* **\|** *y*            | Match either the *x* or *y*               |
| *x* **\***                | Match the *x* zero or more times          |
| **(:?** *x* **)**         | Non-capturing group                       |
| **(** *x* **)**           | Capturing group                           |
| **\p{** *N* **}**         | Char of the general category *N*          |
| **\P{** *N* **}**         | Char not of the general category *N*      |
| **[** *x* **]**           | Character class *x*                       |
| **[^** *x* **]**          | Character not in the class *x*            |
| **[** *x* **-** *y* **]** | Character in range *x..y*                 |
| **[\p{** *N* **}]**       | Char of the general category *N*          |
| **[\P{** *N* **}]**       | Char not of the general category *N*      |
|  **^**                    | Start of line assertion                   |
|  **$**                    | End of line assertion                     |
|  **\b**                   | Word boundary assertion                   |
|  **\B**                   | Not a word boundary assertion             |
|  *x*                      | Character literal *x*, not in `^$.*+?]{}` |
|  **\\** *x*               | Character literal *x* in `^$.*+?]{}`      |

## Useful articles

* https://swtch.com/~rsc/regexp/

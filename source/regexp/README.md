# RegExp engine

This regexp engine should implement
[ECMAScript Regular Expression](https://tc39.es/ecma262/#sec-regexp-regular-expression-objects)
(Unicode Mode), but currently only part of specification is implemented.

For now we have:

|  RegExp                   | Description                               |
| ------------------------- | ----------------------------------------- |
| *.*                       | Match any character except new line       |
| *x* *y*                   | Match the *x* then *y*                    |
| *x* **\|** *y*            | Match either the *x* or *y*               |
| *x* **\***                | Match the *x* zero or more times          |
| *x* **+**                 | Match the *x* one or more times           |
| *x* **?**                 | Match the *x* zero or one times           |
| **(:?** *x* **)**         | Non-capturing group                       |
| **(** *x* **)**           | Capturing group                           |
| **\p{** *N* **}**         | Char of the general category *N*          |
| **\P{** *N* **}**         | Char not of the general category *N*      |
| **[** *x* **]**           | Character class *x*                       |
| **[^** *x* **]**          | Character not in the class *x*            |
| **[** *x* **-** *y* **]** | Character in range *x..y*                 |
| **[\p{** *N* **}]**       | Char of the general category *N*          |
| **[\P{** *N* **}]**       | Char not of the general category *N*      |
| **^**                     | Start of line assertion                   |
| **$**                     | End of line assertion                     |
| **\b**                    | Word boundary assertion                   |
| **\B**                    | Not a word boundary assertion             |
| **\d**                    | A digit (like `[0-9]`)                    |
| **\D**                    | Not a digit (like `[^0-9]`)               |
| **\s**                    | A whitespace (like `[\p{z}\r\n\t\f\v]`)   |
| **\S**                    | Not a whitespace                          |
| **\w**                    | A word character (like `[A-Za-z0-9_]`)    |
| **\W**                    | Not a word char (like `[^A-Za-z0-9_]`)    |
| *x*                       | Character literal *x*, not in `^$.*+?]{}` |
| **\\** *x*                | Character literal *x* in `^$.*+?]{}`      |
| **\\n** **\\r** **\\t**   | New line, tabulation and other controls   |
| **[\\n\\r]**              | The same in a character class             |

## Useful articles

* https://swtch.com/~rsc/regexp/

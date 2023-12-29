# Simple Object-Oriented Language Interpreter

**Overview**

This project presents a Scheme interpreter that executes code written in a simplified object-oriented language, inspired by Java/C syntax. It was developed as a college project for CSDS 345.

**Interpreter Usage**

The interpreter accepts two parameters:

- `file`: The name of the file containing the code to be interpreted.
- `classname`: The name of the class whose `main` method will be executed.

**Example Usage:**

```scheme
(interpret "MyProgram.j" "B")
```

## Interpreter Process

1. **Parsing:** Parses the specified file using the Scheme parser.
2. **Class Lookup:** Retrieves the designated class from the interpreter's state.
3. **Main Method Execution:** Calls the `main` method of the specified class.
4. **Return Value:** Returns the value produced by the `main` method.

## Language Features

### Object-Oriented Constructs:

- Classes
- Inheritance
- Objects
- Static and non-static (instance) variables and methods
- `this` and `super` object references
- Constructor methods
- Nested dot operator usage

### Data Types:

- Integers
- Booleans
- Objects

### Operators:

- Mathematical and comparison (for integers)
- Logical (for booleans)
- Dot operator for object member access
- `new` operator for object creation

## Parser Constructs

| Language Construct | Equivalent Scheme Representation |
|---|---|
| `class A { ... }` | `(class A () body)` |
| `class B extends A { ... }` | `(class B (extends A) body)` |
| `static var x = 5;` | `(static-var x 5)` |
| `var y = true;` | `(var y true)` |
| `static function main() { ... }` | `(static-function main () body)` |
| `function f() { ... }` | `(function f () body)` |
| `function g();` | `(abstract-function g ())` |
| `class A { A(x) { ... } }` | `(constructor (x) body)` |
| `new A()` | `(new A)` |
| `a.x` | `(dot a x)` |
| `new A().f(3, 5)` | `(funcall (dot (new A) f) 3 5)` |

## Example Program

```java
class A {
  var x = 6;
  var y = 7;

  function prod() {
    return this.x * this.y;
  }

  function set2(a, b) {
    x = a;
    y = b;
  }
}

class B extends A {
  function set1(a) {
    set2(a, a);
  }

  static function main () {
    var b = new B();
    b.set1(10);
    return b.prod();
  }
}
```
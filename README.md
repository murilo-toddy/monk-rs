# monk-rs

An interpreter for the Monkey Language written in Rust.

The language comes from the book [Writing An Interpreter In Go](https://interpreterbook.com/)
by Thorsten Ball.

## Language features

### Integer, boolean, string, array and hash data types

```monkey
let age = 1;
let isTrue = true;
let name = "Monkey";
let array = [1, 2, 3, 4];
let hash = {"one": 1, "two": 2};
```

### Basic operations 

```monkey
let result = 1 + 2 * 3 / 4 - 5;
let isTrue = 1 < 2;
let isFalse = !isTrue;
"Hello" + " " + "World";
```

### Logic expressions

```monkey
let x = if (10 > 5) {
    true;
} else {
    false;
}
```

### While loops

```monkey
let i = 0;
let j = while (i < 2) {
    let i = i + 1;
}
```

### For loops

```monkey
let sum = 0;
for (let i = 0; i < 11; let i = i + 1) {
    let sum = sum + i;
}
sum == 55
```

### Functions and closures

```monkey
let adder = fn(x) {
    fn (y) {
        return x + y;
    };
};

let addTwo = adder(2);
addTwo(5) == 7;
```

### Built-in Functions

`print(arg1, arg2, ...)`

`len("string") == 6`

`len(["a", "b"]) == 2`

`first([1, 2, 3]) == 1`

`rest([1, 2, 3]) == [1, 2, 3]`

`last([1, 2, 3]) == 3`

`push([1, 2], 3) == [1, 2, 3]`

## Getting started

### Using repl

```bash
$ make repl
```


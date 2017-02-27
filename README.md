# rust-calc
A text-based calculator (a.k.a. math shell) written in Rust

* Contains a library crate and a binary crate
* Uses 64 bit arithmetics
* Function and value bindings
* Vector support
* Parallel evaluation
* Bitwise operators

## Language Features

##### Evaluation of mathematical expressions
```
1.25 * 12 + cos pi
= 14
```

##### Functions and value bindings
```
let f(x) = sin x * cos x
Function OK

f(pi/4)
= 0.5

let p4 = pi/4
= 0.7853981633974483
```

##### Integer and floating point arithmetics
```
1 + 4
= 5
  0x5
  0b101

1.0 + 4.0
= 5.0
```

##### Binary and hexadecimal in- and output for integers
```
0b0101_1101
= 93
  0x5d
  0b1011101

0xACDC_FF10
= 2900164368
  0xacdcff10
  0b10101100110111001111111100010000
```

##### Vector support
```
[1,2,3.5]
= [1, 2, 3.5]

[0..10]
= [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

[0..10 : 2.5]
= [0, 2.5, 5, 7.5]
```

##### Basic arithmetic operators
```
((1 + 2) * 3 / 4) % 2
= 0.25
```

##### Floating-point and integer division
```
5 / 2
= 2.5

5 \ 2
= 2
  0x2
  0b10
```

##### C-like bitwise operators
```
(1 << 3) | 0xC00 ^ 0b101 & ~1
= 3084
  0xc0c
  0b110000001100
```

##### Exponents and logarithms
```
(e ** 2) log e
= 2

sqrt 2
= 1.4142135623730951
```

##### Trigonometric operators
```
sin (pi/2) * cos pi + asin (sin pi)
= -1
```

##### Vector-specific operators
```
len [1, 1]
= 1.4142135623730951

[1, 1] dot [2, 2]
= 4

count [1, 2, 3]
= 3
```

##### Bulk operations using vectors
```
[1, 2, 3] * 10
= [10, 20, 30]

2 ** [1..9]
= [2, 4, 8, 16, 32, 64, 128, 256]
```

## Shell Features

###### Last evaluation result: `it`
```
> 1+2
= 3
  0x3
  0b11
> it
= 3
  0x3
  0b11
```

###### Parallel evaluation
```
> #:
> 1+1
> 2*2
> 3/2
> #!
= [2, 4, 1.5]
```

##### JSON file output
```
> #:
> let a=1
> let b=2
> let c=3
> #!
= [1, 2, 3]
> #j
{
    "a": 1,
    "b": 2,
    "c": 3,
    "d": 4,
    "e": 2.718281828459045,
    "it": [1, 2, 3],
    "pi": 3.141592653589793,
}
```

## Installation
* Install rust 1.13 or higher
* Check out the code to some directory RUST-CALC-DIR
* Point your shell to RUST-CALC-DIR/rcalc
* Enter `cargo run --release`

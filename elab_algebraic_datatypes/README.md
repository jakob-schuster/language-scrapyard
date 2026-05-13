# Algebraic Datatypes

Language with structs and (discriminated) enums. Also supports error recovery.

## Example

```
enum Animal { Cat, Dog }
enum Noise { Meow, Woof }

fn make_noise(animal: Animal): Noise {
  match animal {
    Animal.Cat => Noise.Meow,
    Animal.Dog => Noise.Woof
  }
}

# evaluates to Noise.Meow
make_noise(Animal.Cat)
```
Desugared, this is:

```
let Animal = enum { Cat, Dog }
let Noise = enum { Meow, Woof }

let make_noise = fn(animal: Animal): Noise {
  match animal {
    Animal.Cat => Noise.Meow,
    Animal.Dog => Noise.Woof
  }
}

make_noise(Animal.Cat)
```

```
enum Height { Short, Tall }
enum Language { Haskell, Scheme }
enum Job { 
  Programmer { language: Language }, 
  Florist 
}
struct Person { height: Height, job: Job }
```

If we were to add recursive types in future, we could do this:

```
enum Expr {
  Num { n: Num },
  Add { e1: Expr, e2: Expr }
}

fn eval(e: Expr): Num {
  match e {
    Expr.Num { n } => n,
    Expr.Add { e1, e2 } => eval(e1) + eval(e2)
  }
}

let e = Expr.Add {
  e1: Expr.Num { n: 10 },
  e2: Expr.Num { n: 20 }
}

# evaluate it
let n = eval(e)

# print it out
print(n)
```

```
let Color = enum { Red, Green, Blue }
let c = (enum {Red,Green}).Green {}
enum Color {
    Red, Green, Blue
}

struct Pos { x: Num, y: Num }

struct Pixel { color: Color, pos: Pos }

enum Message {
    Draw { pos: Pos },
    Erase { pos: Pos },
    SetBrush { color: Color },
    Exit
}

struct Canvas {
    rows: Map<Pos, Color>
}

fn draw(c: Canvas): Canvas {
    #todo
}

enum Option<T> { Some { data: T }, None }

struct State {
    canvas: Canvas,
    brush: Color
}

fn eval(m: Message, s: State): Option<State> {
    match m {
        Draw { pos } => Some { data: State { canvas: s.canvas.draw(pos, brush), brush }},
        Erase { pos } => Some { data: State { canvas: s.canvas.erase(pos), brush }},
        SetBrush { color } => Some { data: State { canvas: s.canvas, brush: color }},
        Exit => None
    }
}

fn main() {

}
```


```
let Col1 = enum { Red, Blue }
let Col2 = enum { Red, Blue }
let Col3 = enum { Red, Green, Blue }

if Col1.Red == Col2.Red {
  # this will happen
}

if Col1.Red == Col3.Red {
  # this will not happen!
}

let c = Col.Red

if c == Col.Red {

```

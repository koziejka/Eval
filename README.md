# Eval
This lib allows for evaluating simple math expresions passed in string.

## Example

### F#
```f#
open Eval
let test = eval "2+2*2"
```

### C#
```c#
using static Eval;

class Program {
    static void Main(string[] args) {
        double test = eval("2+2*2");
    }
}
```
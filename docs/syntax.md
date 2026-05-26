Currently, SimpleTypeChecker only supports a subset of Julia's syntax. In the future, more syntax will be supported.

Following are unsupported syntax tables and supported syntax tables. If you use some unsupported syntax, SimpleTypeChecker will raise `SyntaxError`. Using `runCheck!(ctx; checkSyntax = false)` can disable syntax checking and ignore all unsupported syntax.

## Unsupported Syntax:
|Description|Example  | Note|
|--|--|--|
| splatting | `push!(x, y...)`<br /> `x,y,... = z` | may never support|
| nested function | `x->x+1` <br /> function nested in function | may support, but don't recommend using due to implementation difficulties. Please use toplevel function as much as possible|
| inline quote | `:(x + y)` |  may never support, use `quote ... end` to ensure type stability|
| named tuple | `(x = 1, y = 3)` | will support |
| multiple for assignment | `for x in arr1, y = arr2 println(1) end` |will support|
| generator |`[i in for i in 1:10]` | may support, but don't recommend using due to implementation difficulties|
| subtyping shorthand and where| `Array{<:Int, 1}`|will support|
| assertion (type narrowing)| `x::Int`|will support|
| macrocall|`@inline`| currently silient ignore, in the future user can provide custom annotation to easy analysis|
| try-catch-finally|| will support|
| vect |`[1,2,3]`|may support, but recommend using typed vect`Int[1,2,3]`|
| outer | |may never support|
| broadcast | x. = x.+ 1|will support|

---

Note : If you use some unsupported syntax but don't want to change them, don't worry. You can isolate them in a function (unless it's closure that capture some variables). For example:
```
function large_function(x)
    ...
    # ops, we can't check generator
    arrs = [i+1 for i in x]
    ...
end
```
We can modify it to
```
@inline function createArray(x)
return [i+1 for i in x]
end

function large_function(x)
    ...
    arrs = createArray(x)
    ...
end
```
In extreme case, you can always bypass checking of a specific function.

## Supported Syntax:
|Description|Example  | Note|
|--|--|--|
| assignment to a single variable | `x = 1+1`| can use update operator (`x+=1`)|
| assignment by tuple destruct |  `(x, (y[4], z.l)) = (3, (4, 5))`| can't use update operator (`(x, y) +=1`)|
| assignment with storage type |  `x::Int = 1`| can't use update operator (`x::Int +=1`) <br/> a variable can only be declared once|
| array assignment | `x[i,j,k] += 1` | can use update operator (`x[1]+=1`)|
| setproperty | `x.k = 1` | can use update operator (`x.k += 1`)|
| array ref| `x[1]`||
| getproperty | `x.k`||
| literal | `1`, `"x"` | |
| variable | x ||
| block | `begin println(1); x = 1+1` ||
| tuple | `(y, z)` ||
| function call|`f(x, y;z =1`)|Keyword arguments require explicit `semi-colon`<br/> Use `f(x;y=1)` instead of `f(x,y=1)`|
| ifelse/while/return/reak/continue/` ? :`|||
| let|`let x = 1, y, z::Int; println(x) end`|a variable can only be declared once<br>tuple destruct is disallowed(`let (x, y) = z end`)|
| for|`for (x,(y,z)) in iter end`||
| local|`local x = 1, y, z::Int`|a variable can only be declared once<br>tuple destruct is disallowed(`local (x, y)`)|
| global|`global x,y,z`|a variable can only be declared once<br>only variable is allowed)|
1. Unreachable code elimimation
2. Error reporting and formatting
3. Poison type handling
4. Logging

Julia's scope problem
Consider this program
```
function f(x)
    if (x > 0)
        for i in 1:10
            # the scope of t is the function
            # t is not the local scope of the for loop
            t = 2
        end
        print(t)
    else
        t = 2
    end
end
```
We require that:
1. if a variable a of scope X is assigned in a inner scope Y, then variable a is also declared or assigned in X before that assignment in Y
(every assignment in inner scope is dominated by some assignment or declaration in outer scope of the same variable)
2. There can be at most one declaration for each variable, and every declaration is located before the assignment (if there is one)
Note:
1. If a variable is declared, then we know that declaration dominates all other assignemnt expression

We need addtional rule for if clause, because if clause is not scoped
1, If a variable is declared in an if clause, that variable can only in that 
1. If a variable is declared in the condition of a if clause, 
# ftools
Set of tools to use with Fortran programs.

## Array related oprations

### Array masking
Mask an array based on a boolean expresion.

```fortran
x = [1, 5, 2, 1, 6]
y = mask(x < 3)

print *, y
! [1, 2, 1]
```

### Diff
Finite difference on an array

```fortran
x = [1, 5, 2, 1, 6]
y = diff(x)

! y = [4, 3, -1, 5]
```


## Optional values handler
Simplify the check of optional values, giving possible defaults

```fortran
! if xin was present in the procedure call, x will have the xin value,
! else it will be 5

x = optval(xin, 5)
```

## IO

### Convert numeric values into strings

```fortran
i = 5
c = str(i)

! c == "5" (True)
```
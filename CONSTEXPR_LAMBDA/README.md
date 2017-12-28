# CONSTEXPR_LAMBDA
Simple macro to allow 'constexpr' generic no-capture lambdas in C++14

*{ Written by [Aaron McDaid](https://aaronmcdaid.github.io/) - aaron.mcdaid@gmail.com }*

In C++14, lambdas are very useful but they have some restrictions. They can't be used in certain contexts and they aren't very friendly with `constexpr`.

*{ Update: Breaking change pushed on 28th December 2017 - fewer commas required now, simpler interface }*

For example, the following code works in C++17, but not C++14 (`error: call to non-constexpr function 'main()::<lambda(int)>'`):

```
    constexpr int a =
            [](int x){return x*x;}  // a lambda
            (4)                     // call the lambda
            ;
    static_assert(a == 16 ,"");
```
With this library, you can write this in C++14:
```
    constexpr auto a =
            CONSTEXPR_LAMBDA(x)( return x*x; ) // a macro created a constexpr pseudo-lambda
            (4);                                // call this pseudo-lambda
    static_assert(a == 16 ,"");
```

Within the first set of parentheses, you specify a 'capture type' (by &-reference, by value, or by && forwarding reference) as well as a name for each parameter.
This essentially creates a generic lambda, i.e. as if all parameters are `auto` or `auto &` or `auto &&`.
For example:
```
CONSTEXPR_LAMBDA(&&a,&&b)  ....// capture two args called 'a' and 'b', both of them by forwarding reference
CONSTEXPR_LAMBDA( &x,  y)  ....// capture two args called 'x' and 'y', 'x' is by reference, 'y' by value

// the above are essentially equivalent to:

    (auto && a , auto && b)
    (auto  & x , auto    y)
```

You can even use the last parameter to capture a pack, with `CONSTEXPR_LAMBDApack` instead of `CONSTEXPR_LAMBDA`, i.e. `CONSTEXPR_LAMBDApack(first_arg, second_arg, pack)`, but I can't get it to work on MSVC (tested on gcc.godbolt.org).

Here is a more complicated demo showing the reference capture:
```
constexpr auto
test_reference_capture()
{
    int A = 10;
    int B = 100;
    int C = 1000;
    int product = CONSTEXPR_LAMBDA(&a,b,c)
                    (
                        int product = a*b*c;
                        a=2; // 'a' was captured by reference
                        b=0; // 'b' and 'c' were captured by value,
                        c=0; // so these assignments don't realy do anything
                        return product;
                    )
                    (A,B,C);
    // 'product' should be 1000000 now
    // 'A' was captured by reference, and hence is now 2
    return A + B + C + product;
}
static_assert(test_reference_capture() == 2 + 100 + 1000 + 1000000 ,"");
```

## Compatibility

Through testing on gcc.godbolt.org, this works on gcc >= 5.1 and clang >= 3.6. Just remember to use `-std=c++14.
It also works with the version of `MSVC' currently on gcc.godbolt.org, "MSVC 19 2017 RTW".
However, I can't get packs to work with MSVC.
I think this is standard C++14, and if not I think we can fix it if necessary. So don't hesitate to send me any
improvements!

## Capture lambdas?
I haven't given any serious thought to how to implement capturing-lambdas here, but I think it should be easy
enough to do so. However, if you really need such a thing in a particular context, maybe you should just write
your own class out-of-line!

## How it works
To begin, you would write a new class with the appropriate call operator:
```
    struct x {
        constexpr auto
        operator() (int x)
        { return x * x; }
    };
```
But this can't (usually) be written in just any location, for example you can't define new types
in the middle of expression.
In order to work around this, and define a new type in the middle of an existing expression,
we create a lambda which
returns this `x` 'type' indirectly via a pointer. This allows us to write our type anywhere
we want: (except in *unevaluated contexts*, for which you should consider my {crazy!} [cambda](https://github.com/aaronmcdaid/cambda) library)

```
    auto * ptr =
    [](){

        struct x {
            constexpr auto
            operator() (int x)
            { return x * x; }
        };
        return (x*)nullptr;

    } // define a lambda
    (); // call it, returning the nullptr pointer-to-x
```
But we want a generic function call operator. This is not yet generic as the argument is fixed to be `int`.
Unfortunately, we can't write templates inside these local classes. i.e. this isn't allowed.
```
    auto * ptr =
    [](){

        struct x {
            template<typename T> // templates not allowed in local classes
            constexpr auto
            operator() (int x)
            { return x * x; }
        };
        return (x*)nullptr;

    } // define a lambda
    (); // call it, returning the nullptr pointer-to-x
```
... so we use the outer lambda to 'encode' the type of the parameter
```
    auto funny_lambda_returning_our_pseudo_lambda =
    [](auto outer_x){   /* this 'outer_x' is never really used as a value, its
                         * purpose is just to encode a type. */

        struct x {
            constexpr auto
            operator() (decltype(outer_x) x)
            { return x * x; }
        };
        return (x*)nullptr;

    };
```
Now, we can extract an instance of 'x' for the correct argument type:
```
using x_int     = std::decay_t<decltype(*funny_lambda_returning_our_pseudo_lambda(std::declval<int>()))>;
static_assert(std::is_same< int     , decltype(x_int{}(3)) >{} ,"");

using x_double  = std::decay_t<decltype(*funny_lambda_returning_our_pseudo_lambda(std::declval<double>()))>;
static_assert(std::is_same< double  , decltype(x_double{}(3)) >{} ,"");

// next, construct and call these two objects. The second one is
// just to confirm that it truly is the int-to-int version

static_assert(2.25 == x_double{} (1.5) ,"");
static_assert(1    == x_int   {} (1.5) ,"");
```

The `call_forwarder` class template automates this for us, generically for any number
and type of arguments. The arguments passed by the user are used to lookup the appropriate
inner class `X` and then perfectly-forward the arguments into the call operator inside `X`.
`L` is the type of the generic lambda (essentially, `decltype(funny_lambda_returning_our_pseudo_lambda)`:

```
    template<typename L>
    struct call_forwarder
    {
        template<typename ... T>
        constexpr auto
        operator() (T && ... t)
        ->decltype(auto)
        {
            using X = std::decay_t<decltype( *std::declval<L&>()(std::forward<T>(t)...) )>;
            // X is the nested 'CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja' type
            return X{} (std::forward<T>(t)...);
        }

    };
```

We therefore need a function to construct a `call_forwarder<L>`. That's `make_CONSTEXPR_LAMBDA` as called here,
where `null_address_of` is a small helper function to return a null pointer pointing to the type of its argument.
In other words, `null_address_of(3.5)` gives us `(double*)nullptr`.

```
    auto pseudo_lambda =
        make_CONSTEXPR_LAMBDA(
            null_address_of(
                [](auto && arg0) {
                    struct x
                    {
                        constexpr auto
                        operator() (decltype(arg0)&& arg0)
                        ->decltype(auto)
                        {
                            return arg0*arg0;
                        }
                    };
                    return (x*) nullptr;
                }
            )
        );
```
The final problem is that is isn't itself a constant expression. The lambda itself, `[](){...}`, isn't a constant
expression and therefore this pervades the entire expression, resulting in a non-constant `pseudo_lambda`.
This is unfortunate as we know the return value of `null_address_of( )` is exactly `nullptr` - how can
we 'force' it to be `constexpr`?
In other words, we can do this:

```
    constexpr auto * p1 = (int*) nullptr;
```
but not:
```
    constexpr auto * p1 = null_address_of(3); // error as the entire expression isn't a constant expression
```
We can resolve this with the "`?:`" trick:
```
    constexpr auto * p1 = true ? nullptr : null_address_of(3); // this works
```
Normally, if any argument to a function is non-constant, the entire expression is non-constant. But there is
an exception here for this `?:` operator. As the boolean condition (`true`) is a constant, the value is
taken directly from in between the `?` and `:`, and it ignores the 'non-constant-ness' of the `null_address_of(3)`
after the `:`. This means that the entire expression is a constant expression.
The 'value' of `null_address_of(3)` is ignored, in fact it is never even evaluated, but it is not
entirely useless as it is used to control the type.


This finally gives us a way to construct the pseudo_lambda that we desire:
```
    auto pseudo_lambda = make_CONSTEXPR_LAMBDA(true ? nullptr : null_address_of([](auto && arg0) {
        struct x
        {
            constexpr auto
            operator() (decltype(arg0)&& arg0)
            ->decltype(auto)
            {
                return arg0*arg0;
            }
        };
        return (x*) nullptr;
    }));

    static_assert(pseudo_lambda(4) == 16 ,"");
```

The header, `CONSTEXPR_LAMBDA.hh`, includes implementations of `null_address_of`, `struct call_forwarder`, and `make_CONSTEXPR_LAMBDA`
and a macro `CONSTEXPR_LAMBDA` to detect the number of arguments and generate the boilerplate.

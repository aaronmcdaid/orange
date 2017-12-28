# orange
Simple range library. Header-only. C++14 (gcc and clang, I don't think MSVC works).
A range can be an "owning" range, hence the 'o' in 'orange'. Such a range cannot be copied, in order to maintain reasonable semantics.

I might also say that 'o' also stands for "output" - but that's for the future!

(Early stages yet, but the examples here do work)

Ranges can be zipped together. Ranges (including zipped ranges) can be sorted. Sorting a zipped range is interesting (TODO: explain why this is cool!)

*{ Written by [Aaron McDaid](https://aaronmcdaid.github.io/) - aaron.mcdaid@gmail.com }*


Look at `test.orange.cc` for a few tests that show the functionality, in particular a `zip` example

First simple example ranges over the numbers 1 to 6, keeps only the even ones (via `filter`) and then multiplies
those elements by 10 via `mapr`, finally `collect`-ing the results into a vector

```
#include "orange.hh"
using namespace orange;
        vector<int> vec{1,2,3,4,5,6};
        auto result = vec
                    |filter|    [](auto x){return x % 2 == 0;}
                    |mapr|      [](auto x) {return x*10;}
                    |collect;
        assert(result == std::vector<int>({20,40,60}));

```


```
#include "orange.hh"
void README_tests()
{
    {
        // 'mapr' - apply a function to each element
        vector<int> arr{1,2,3};
        auto res =
            arr
                |mapr|
                    [](auto x) {return x*10;}
                |collect; // collect all the results into a vector
        assert(res == std::vector<int>({10,20,30}));
    };

    {
        // 'filter' - keep only the even ones
        vector<int> arr{1,2,3,4,5,6};
        auto res =
            arr
                |filter|
                    [](auto x){return x % 2 == 0;}
                |collect;
        assert(res == std::vector<int>({2,4,6}));
    };

    {
        vector<int> arr{10,20,30};
        arr
            |foreach|
                [](auto&x){x=x*3;}
            ;
        assert(arr == std::vector<int>({30,60,90}));
    };

    {
        int a[]{5,6,7};
        auto res =
            a
                |mapr|
                    [](auto x){ return x*1.5; }
                |collect;
        assert( res == std::vector<double>({7.5,9.0,10.5}));
    }
}
```

#include "orange.hh"
#include "cambda/cambda.hh"
#include "../module-bits.and.pieces/PP.hh"
#include "../module-bits.and.pieces/utils.hh"
#include<iostream>
#include<vector>
#include<memory>
using std:: vector;
using std:: string;
using namespace orange;
using utils:: operator<<;
using utils:: type_as_string;

using cambda::operator"" _cambda;
using cambda::operator"" _binding;

namespace std {

template<typename T, size_t N>
std::ostream& operator<< (std::ostream &o, std::array<T, N> const &arr)
{
    o << '[';
    for(auto it = begin(arr); it != end(arr); ++it)
    {
        if(it != begin(arr))
            o << ',';
        o << *it;
    }
    o << ']';
    return o;
}
}

template<typename It>
It constexpr
cx_partition(It b, It e)
{
    // Actually, these first two 'ifs' will never be satisfied.
    // So they're not really interesting at all.
    if(b==e)
        return b;
    if(b+1 == e)
        return b;

    // ... in other words, there will always be at least
    // two items in this. And the returned iterator will
    // always point to an item (*not* to one-past-the-end)

    while(b+1 != e) // while there are at least two elements
    {
        if(*(b+1) < *b)
        {
            cx_swap(*(b+1),*b);
            ++b;
            continue;
        }
        else
        {
            if(b+1 != e-1) // so it doesn't swap with itself
                cx_swap(*(b+1), *(e-1));
            --e;
            continue;
        }
    }
    return b;
}

template<typename It>
void constexpr
cx_sort(It const b, It const e) // constexpr sort
{
    if(b==e)
        return; // empty range
    if(b+1 == e)
        return; // just one item

    auto p = cx_partition(b, e);
    (void)p;
    cx_sort(b, p);
    cx_sort(p+1, e);
}

constexpr
auto test_zip_sorted_in_place()
{
    // sorting in place
    int ai[] = {4,7,2,9,3,6};
    char ac[] = {'h','e','l','l','o','_'};
    double ad[] = {0.1,0.2,0.3,0.4,0.5,0.6};

    auto ar = zip(ai, ac, ad);

    cx_sort   (begin(ar), end(ar));

    auto res =
    ar
        |mapr|
            apply_pack % "{[_ _ d] / [d]}"_cambda()
        |collect_at_most<10>;
    return res;
}

struct partition
{
    template<typename B, typename E>
    constexpr B
    operator()(B b, E e) const
    {
        // should never be called on fewer than two items,
        // TODO: think about testing/asserting this
        (void)b;
        (void)e;
        while((b != e) && ((b+1)!=e))
        {
            if( *(b+1) < *b )
            {
                std::cx_swap(*(b+1), *b);
                ++b;
                continue;
            }
            else
            {
                --e;
                std::cx_swap(*(b+1), *e);
                continue;
            }
        }
        return b;
    }
};

constexpr bool
test_partition()
{
        int a[] = {6,2,5,8,3,9,7};
        "(partition.pivot.is.first b e)"_cambda
                [   "partition.pivot.is.first"_binding = partition{}
                ,   "b"_binding = std::begin(a)
                ,   "e"_binding = std::end(a)
                ]();
        return cambda_utils::equal_array(a, (int[]){2,5,3,6,9,7,8});
}

static_assert(test_partition() ,"");

int main () {
    static_assert(test_zip_sorted_in_place() == make_compact_vector_with_max_size(0.3,0.5,0.1,0.6,0.2,0.4), "");


    PP(replicate(5, std::string("five")) | collect);
    {
        using ints_t = decltype(ints(42));
        (ints_t[]) { ints(3), ints(100,105) }
            |concat
            |foreach| [](auto &&x ) { PP(x); }
            ;
        ints(4)
            |mapr| intsFrom0
            |memoize
            |concat
            |foreach| [](auto &&x ) {
                PP(x);
            }
        ;

        int a[]{0,1,2};
        auto ar = as_range(a);
        PP(ar|collect);

        std::cout << '\n';
        (decltype(ar)[]) { ar, ar }
            |concat
            //|memoize // memoize is optional here, but it changes the result
            |foreach| [](auto &&x ) {
                PP(x);
                x += 100;
            };
        PP(ar|collect);
    }
}

namespace testing_namespace
{
    constexpr auto
    test_simple_map()
    {
        int arr[]{1,2,3};
        return
            arr
                |mapr|
                    "(lambda [x] [{x * 10}])"_cambda()
                |collect_at_most<100>;
    };
    static_assert(test_simple_map() == make_compact_vector_with_max_size(10,20,30) ,"");

    auto constexpr
    test_assign_in_vector()
    {
        auto d = make_compact_vector_with_max_size(10,20,30);
        d
            |foreach|
                "(lambda [x] [{x = {x * 3}}])"_cambda()
            ;
        return d;
    };

    static_assert(test_assign_in_vector() == make_compact_vector_with_max_size(30,60,90) ,"");

    auto constexpr
    test_mapr_with_floating_point()
    {
        int a[]{5,6,7};
        return
        a
            |mapr|
                "{[x] / [{x * 1.5}]}"_cambda()
            | collect_at_most<100>;
    }
    static_assert( test_mapr_with_floating_point() == make_compact_vector_with_max_size(7.5,9.0,10.5) ,"");
}

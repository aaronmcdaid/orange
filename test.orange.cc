#include "orange.hh"
#include "cambda/cambda.hh"
#include "../module-bits.and.pieces/PP.hh"
#include "../module-bits.and.pieces/utils.hh"
#include "../module-TEST_ME/TEST_ME.hh"
#include<iostream>
#include<vector>
#include<memory>
using std:: vector;
using std:: string;
using namespace orange;
using utils:: operator<<;
using utils:: type_as_string;
using TEST_ME::test_me;

using cambda::operator"" _cambda;

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

int main () {

    {
        // sorting in place
        int ai[] = {4,7,2,9,3,7};
        char ac[] = {'h','e','l','l','o','_'};
        double ad[] = {0.1,0.01,0.001,1,1,1};
        auto ar = zip(ai, ac, ad);

        std:: cout << '\n';
        for(auto i = begin(ar); i!=end(ar); ++i) {
            PP(*i);
        }

        std::swap( *(begin(ar))
                 , *(begin(ar)+1)
                );

        std:: sort(begin(ar), end(ar));

        std:: cout << '\n';
        for(auto i = begin(ar); i!=end(ar); ++i) {
            PP(*i);
        }
    }

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

    TEST_ME ( "|concat with refs. side-effects."
            , std::vector<double>{1.5,3,4.5}
            ) ^ []()
            {
                int a[]{1,2,3};
                return
                a
                    |mapr|
                        [](auto x){return x * 1.5;}
                    |collect;
            };

    TEST_ME ( "simple use of cambda"
            , std::array<int, 3>{{2,4,6}}
            ) ^ []()
            {
                std::array<int, 3> a{{1,2,3}};
                a
                    |foreach|
                        "(lambda [x] [(assign x {x * 2})])"_cambda()
                        ;
                return a;
            };

    TEST_ME ( "collectAtMost"
            , std::vector<double>{1.5,3,4.5}
            ) ^ []()
            {
                int a[]{1,2,3};
                auto res =
                a
                    |mapr|
                        [](auto x){return x * 1.5;}
                    | collect_at_most<100>;
                return res;
            };

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
}

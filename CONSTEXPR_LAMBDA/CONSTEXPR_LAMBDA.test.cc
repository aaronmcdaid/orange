#include "CONSTEXPR_LAMBDA.hh"
int main()
{
    auto l = CONSTEXPR_LAMBDA_namespace::make_CONSTEXPR_LAMBDA(true ? nullptr : CONSTEXPR_LAMBDA_namespace::null_address_of([](auto && arg0) {
        struct CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja
        {
            constexpr auto
            operator() (decltype(arg0)&& arg0)
            ->decltype(auto)
            {
                return arg0*arg0;
            }
        };
        return (CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja*) nullptr;
    }));

    static_assert(l(10) == 100 ,"");
    static_assert(l(0.5) == 0.25 ,"");

    //I can't get packs working on MSVC
    /*
    auto l2 = CONSTEXPR_LAMBDA_namespace::make_CONSTEXPR_LAMBDA(true ? nullptr : CONSTEXPR_LAMBDA_namespace::null_address_of([](auto && ... arg0) {
        struct CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja
        {
            constexpr auto
            operator() (decltype(arg0)&& ... arg0)
            ->decltype(auto)
            {
                return sizeof...(arg0);
            }
        };
        return (CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja*) nullptr;
    }));

    static_assert(l2(10) == 1 ,"");
    static_assert(l2(1,2,3) == 3 ,"");
    */

    {
        constexpr auto res3 = CONSTEXPR_LAMBDA_with_this_many_args_1(m)( return m*m;)(3);
        static_assert(res3 == 9 ,"");

        constexpr auto res4 = CONSTEXPR_LAMBDA_with_this_many_args_2(a,b)( return a*b;)(21,3) ;
        static_assert(res4 == 63 ,"");

        constexpr auto res5 = CONSTEXPR_LAMBDA_with_this_many_args_1(m)( return m*m;) (3);
        static_assert(res5 == 9 ,"");
    }
    {
        constexpr auto res3 = CONSTEXPR_LAMBDA(m)( return m*m;)(3);
        static_assert(res3 == 9 ,"");

        constexpr auto res4 = CONSTEXPR_LAMBDA(a,b)( return a*b;)(21,3) ;
        static_assert(res4 == 63 ,"");

        constexpr auto res0 = CONSTEXPR_LAMBDA0()( return 42;) (); // NOTE: special name when using exactly zero args
        static_assert(res0 == 42 ,"");
    }
    auto funny_lambda_returning_our_pseudo_lambda =
    [](auto outer_x){

        struct x {
            constexpr auto
            operator() (decltype(outer_x) x)
            { return x * x; }
        };
        return (x*)nullptr;

    };
    using x_int     = std::decay_t<decltype(*funny_lambda_returning_our_pseudo_lambda(std::declval<int>()))>;
    static_assert(std::is_same< int     , decltype(x_int{}(3)) >{} ,"");
    using x_double  = std::decay_t<decltype(*funny_lambda_returning_our_pseudo_lambda(std::declval<double>()))>;
    static_assert(std::is_same< double  , decltype(x_double{}(3)) >{} ,"");

    static_assert(2.25 == x_double{} (1.5) ,"");
    static_assert(1    == x_int   {} (1.5) ,"");

    auto pseudo_lambda = CONSTEXPR_LAMBDA_namespace::make_CONSTEXPR_LAMBDA(true ? nullptr : CONSTEXPR_LAMBDA_namespace::null_address_of([](auto && arg0) {
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

#if 0
    { // test using packs - they don't work on MSVC
        constexpr auto i = CONSTEXPR_LAMBDA(...,pack)
                            (
                                return sizeof...(pack);
                            ) (42,"hi");
        static_assert(i == 2 ,"");

    }
#endif

    {
        constexpr auto square = CONSTEXPR_LAMBDA(a)( return a*a;) ;
        static_assert(square(4) == 16 ,"");
    }
}

constexpr auto
test_simple_reference_capture1()
{
    int A = 1;
    int B = 10;
    int res = CONSTEXPR_LAMBDA_with_this_many_args_2(&a,b)
                    (
                        a=2; // 'a' was captured by reference
                        (void)a;
                        return b;
                    )
                    (A,B);
    // 'product' should be 1000000 now
    // 'A' was captured by reference, and hence is now 2
    return A * res;
}
static_assert(test_simple_reference_capture1() == 20 , "");
constexpr auto
test_simple_reference_capture2()
{
    int A = 1;
    int B = 10;
    int res = CONSTEXPR_LAMBDA_with_this_many_args_2(a,b)
                    (
                        a=2; // 'a' was captured by value, hence this has no effect
                        (void)a;
                        return b;
                    )
                    (A,B);
    // 'product' should be 1000000 now
    // 'A' was captured by reference, and hence is now 2
    return A * res;
}
static_assert(test_simple_reference_capture2() == 10 , "");

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
                        c=0; // so these assignment don't realy do anything
                        return product;
                    )
                    (A,B,C);
    // 'product' should be 1000000 now
    // 'A' was captured by reference, and hence is now 2
    return A + B + C + product;
}
static_assert(test_reference_capture() == 2 + 100 + 1000 + 1000000 ,"");

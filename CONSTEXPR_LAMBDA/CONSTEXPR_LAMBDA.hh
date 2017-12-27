/* CONSTEXPR_LAMBDA - a macro to allow constexpr lambdas in C++14.
 *
 * More info, including a full explanation: https://github.com/aaronmcdaid/CONSTEXPR_LAMBDA
 *
 * Aaron McDaid - aaron.mcdaid@gmail.com - https://aaronmcdaid.github.io/
 */
#include <type_traits>
#include <utility>

namespace CONSTEXPR_LAMBDA_namespace
{
    /*
     * 'call_forwarder' has the call operator that is exposed to the outside,
     * capturing the arguments and forwarding them through to the call operator
     * in the inner class. */
    template<typename L>
    struct call_forwarder
    {
        template<typename ... T>
        constexpr auto
        operator() (T && ... t) const
        ->decltype(auto)
        {
            using X = std::decay_t<decltype( *std::declval<L&>()(std::forward<T>(t)...) )>;
            // X is the nested 'CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja' type
            return X{} (std::forward<T>(t)...);
        }

        template<typename ... T>
        constexpr auto
        operator() (T && ... t) const volatile
        ->decltype(auto)
        {
            using X = std::decay_t<decltype( *std::declval<L&>()(std::forward<T>(t)...) )>;
            // X is the nested 'CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja' type
            return X{} (std::forward<T>(t)...);
        }

    };

    /* 'make_CONSTEXPR_LAMBDA' constructs a 'call_forwarder' of the appropriate type */
    template<typename L>
    auto constexpr
    make_CONSTEXPR_LAMBDA(L*)
    -> call_forwarder<L>
    { return {}; }

    /* 'null_address_of' doesn't really do anything, it's never really called
     * as it appears on the 'wrong' side of a   ?:  expression.
     *
     *  constexpr auto * p = true?nullptr : null_address_of(foo());
     *
     * 'p' is a constant expression there. It's value is 'nullptr', which isn't
     * interesting. But it's useful as it gives us the type of the return value
     * of 'foo()' in contexts where we can't type 'decltype'.
     */
    template<typename L>
    auto
    null_address_of(L &&)
    ->std::decay_t<L>*
    {
        return nullptr;
    }

}


/* The next seven macros are simply to count the number of arguments to the CONSTEXPR_LAMBDA
 * macro and forward accordingly. i.e.
 *  CONSTEXPR_LAMBDA(&,a)       => CONSTEXPR_LAMBDA_with_this_many_args_2(&,a)
 *  CONSTEXPR_LAMBDA(&,a,&&,b)  => CONSTEXPR_LAMBDA_with_this_many_args_4(&,a,&&,b)
 * I don't quite know why CONSTEXPR_LAMBDA_MACRO_EXPAND is needed, but it is needed on MSVC.
 */
#define CONSTEXPR_LAMBDA(...)   CONSTEXPR_LAMBDA_MACRO_EXPAND(CONSTEXPR_LAMBDA_select_based_on_arg_count(CONSTEXPR_LAMBDA_COUNT_MACRO_ARGS(__VA_ARGS__))(__VA_ARGS__))
#define CONSTEXPR_LAMBDA_COUNT_MACRO_ARGS(...)       CONSTEXPR_LAMBDA_COUNT_MACRO_ARGS_A(unused_sdkjflkajfdl, __VA_ARGS__)
#define CONSTEXPR_LAMBDA_COUNT_MACRO_ARGS_A(...)     CONSTEXPR_LAMBDA_MACRO_EXPAND(CONSTEXPR_LAMBDA_COUNT_MACRO_ARGS_B(__VA_ARGS__, 12,11,10,9,8,7,6,5,4,3,2,1,0))
#define CONSTEXPR_LAMBDA_MACRO_EXPAND(x)             x
#define CONSTEXPR_LAMBDA_COUNT_MACRO_ARGS_B(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12, VAL, ...)     VAL
#define CONSTEXPR_LAMBDA_select_based_on_arg_count(n)       CONSTEXPR_LAMBDA_select_based_on_arg_count_impl(n)
#define CONSTEXPR_LAMBDA_select_based_on_arg_count_impl(n)  CONSTEXPR_LAMBDA_with_this_many_args_ ## n




// Next are the main macros, one for each number of possible arguments:

#define JUST_THE_NAME(ref_qualifier) /*nothing*/
#define JUST_AUTO(ref_qualifier) auto
#define AUTO_WITH_REF(ref_qualifier) auto ref_qualifier

#define CONSTEXPR_LAMBDA_with_this_many_args_1(NAME_AND_REF_0)                   \
            CONSTEXPR_LAMBDA_start_of_the_macro_stuff                                       \
                        (                           auto && arg0                \
                        )   {                                                               \
            CONSTEXPR_LAMBDA_start_the_nested_class                                         \
            operator()  (   decltype(arg0)  NAME_AND_REF_0                \
                        )                                                                   \
            ->decltype(auto)                                                                \
            {   CONSTEXPR_LAMBDA_BODY_OF_THE_FUNCTION

#define CONSTEXPR_LAMBDA_with_this_many_args_2(NAME_AND_REF_0, NAME_AND_REF_1)                   \
            CONSTEXPR_LAMBDA_start_of_the_macro_stuff                                       \
                        (                           auto && arg0, auto && arg1                \
                        )   {                                                               \
            CONSTEXPR_LAMBDA_start_the_nested_class                                         \
            operator()  (   std::remove_reference_t<decltype(arg0)>  NAME_AND_REF_0                \
                        ,   std::remove_reference_t<decltype(arg1)>  NAME_AND_REF_1                \
                        )                                                                   \
            ->decltype(auto)                                                                \
            {   CONSTEXPR_LAMBDA_BODY_OF_THE_FUNCTION


// Finally, three macros to avoid repetitive code in the previous few macros
#define CONSTEXPR_LAMBDA_start_of_the_macro_stuff                                           \
            CONSTEXPR_LAMBDA_namespace::make_CONSTEXPR_LAMBDA(true ? nullptr :              \
            CONSTEXPR_LAMBDA_namespace::null_address_of([]

#define CONSTEXPR_LAMBDA_start_the_nested_class                                             \
        struct CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja             \
        {   constexpr auto/*start the declaration of the call operator*/

#define CONSTEXPR_LAMBDA_BODY_OF_THE_FUNCTION(...)                                                                          \
                __VA_ARGS__                                                                 \
            }/*end of the call operator in the nested struct*/                              \
        };                                                                                  \
        return (CONSTEXPR_LAMBDA_arbitrary_hidden_struct_namelkdsjflkafdlksafdja*) nullptr; \
    })) /* { closes the lambda, then one parenthesis for 'null_address_of', then one parenthesis for 'make_CONSTEXPR_LAMBDA' */


;;; cpp-auto-include.el --- auto include header file for C++

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-cpp-auto-include
;; Version: 0.01
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'rx)

(defvar cpp-auto-include--header-regexp
  `(

    ;; [support.types]
    ("cstddef" t t
     ,(rx (and symbol-start
               (or "size_t" "ptrdiff_t" "nullptr_t" "max_align_t")
               symbol-end)))
    ("cstddef" nil t
     ,(rx (and symbol-start
               (or (and "offsetof" (* space) "(")
                   (and "NULL" symbol-end)))))
    ;; [limits.syn]
    ("limits" t t
     ,(rx (and symbol-start
               (or (and "numeric_limits" (* space) "<")
                   (and  (or "round_indeterminate"
                             "round_toward_zero"
                             "round_to_nearest"
                             "round_toward_infinity"
                             "round_toward_neg_infinity"
                             "denorm_indeterminate"
                             "denorm_absent"
                             "denorm_present")
                         symbol-end)))))
    ;; [c.limits]
    ("climits" nil t
     ,(rx (and symbol-start
               (or "CHAR_BIT" "CHAR_MAX" "CHAR_MIN"
                   "INT_MIN" "INT_MAX" "LLONG_MAX" "LLONG_MIN"
                   "LONG_MAX" "LONG_MIN" "MB_LEN_MAX"
                   "SCHAR_MIN" "SCHAR_MAX" "SHRT_MAX" "SHRT_MIN"
                   "UCHAR_MAX" "UINT_MAX" "ULLONG_MAX" "ULONG_MAX" "USHRT_MAX")
               symbol-end)))
    ("cfloat" nil t
     ,(rx (and symbol-start
               (or "FLT_RADIX" "DECIMAL_DIG"
                   "FLT_MIN" "DBL_MIN" "LDBL_MIN"
                   "FLT_MAX" "DBL_MAX" "LDBL_MAX"
                   "FLT_EPSILON" "DBL_EPSILON" "LDBL_EPSILON"
                   "FLT_DIG" "DBL_DIG" "LDBL_DIG"
                   "FLT_MANT_DIG" "DBL_MANT_DIG" "LDBL_MANT_DIG"
                   "FLT_MIN_EXP" "DBL_MIN_EXP" "LDBL_MIN_EXP"
                   "FLT_MIN_10_EXP" "DBL_MIN_10_EXP" "LDBL_MIN_10_EXP"
                   "FLT_MAX_EXP" "DBL_MAX_EXP" "LDBL_MAX_EXP"
                   "FLT_MAX_10_EXP" "DBL_MAX_10_EXP" "LDBL_MAX_10_EXP"
                   "FLT_ROUNDS" "FLT_EVAL_METHOD")
               symbol-end)))
    ;; [cstdint.syn]
    ("cstdint" t t
     ,(rx (and symbol-start
               (or (and (zero-or-one "u")
                        "int"
                        (zero-or-one (or "_fast" "_least"))
                        (or "8" "16" "32" "64")
                        "_t")
                   "intmax_t" "intptr_t"
                   "uintmax_t" "uintptr_t")
               symbol-end)))
    ("cstdint" t t
     ,(rx (and symbol-start
               (or (and (zero-or-one "U")
                        "INT"
                        (or "8" "16" "32" "64" "MAX")
                        "_C"
                        (* space) "(")
                   (and (or (and "INT_"
                                 (zero-or-one (or "FAST" "LEAST"))
                                 (or "8" "16" "32" "64")
                                 "_MIN")
                            (and (zero-or-one "U")
                                 "INT_"
                                 (zero-or-one (or "FAST" "LEAST"))
                                 (or "8" "16" "32" "64")
                                 "_MAX")
                            (and "INT"
                                 (or "MAX" "PTR")
                                 "_MIN")
                            (and (zero-or-one "U")
                                 "INT"
                                 (or "MAX" "PTR")
                                 "_MAX")
                            (and (or "PTRDIFF" "SIG_ATOMIC" "WCHAR" "WINT")
                                 (or "_MAX" "_MIN")))
                        symbol-end)))))

    ;; [support.dynamic]
    ("new" t t
     ,(rx (and symbol-start
               (or (and (or "get_new_handler" "set_new_handler")
                        (* space) "(")
                   (and (or "new_handler"
                            "nothrow" "nothrow_t"
                            "bad_alloc" "bad_array_new_length")
                        symbol-end)))))
    ;; [support.rtti]
    ("typeinfo" t t
     ,(rx (and symbol-start
               (or "type_info" "bad_cast" "bad_typeid")
               symbol-end)))
    ;; [support.exception]
    ("exception" t t
     ,(rx (and symbol-start
               (or (and (or "get_unexpected" "set_unexpected" "unexpected"
                            "get_terminate" "set_terminate" "terminate"
                            "uncaught_exception"
                            "current_exception"
                            "rethrow_exception")
                        (* space) "(")
                   (and (or "make_exception_ptr"
                            "throw_with_nested"
                            "rethrow_if_nested")
                        (* space) (or "(" "<"))
                   (and (or "unexpected_handler" "terminate_handler"
                            "bad_exception" "nested_exception"
                            "exception" "exception_ptr")
                        symbol-end)))))
    ;; [support.initlist]
    ("initializer_list" t t
     ,(rx (and symbol-start
               "initializer_list"
               (* space) "<")))
    ;; [support.runtime]


    ("csignal" t t
     ,(rx (and symbol-start
               (or (and (or "signal" "raise")
                        (* space) "(")
                   (and "sig_atomic_t"
                        symbol-end)))))
    ("csignal" nil t
     ,(rx (and symbol-start
               "SIG"
               (or "ABRT" "FPE" "ILL"
                   "INT" "SEGV" "TERM"
                   "_DFL" "_IGN" "_ERR")
               symbol-end)))

    ("cstdarg" t t
     ,(rx (and symbol-start
               "va_list"
               symbol-end)))
    ("cstdarg" nil t
     ,(rx (and symbol-start
               (or "va_start" "va_arg" "va_copy" "va_end")
               (* space) "(")))

    ("cstdlib" t t
     ,(rx (and symbol-start
               (or "system" "getenv")
               (* space) "(")))
    ("ctime" t t
     ,(rx (and symbol-start
               (or (and (or "clock" "time" "difftime"
                            "ctime" "asctime" "strftime"
                            "wcsftime" "gmtime" "localtime" "mktime")
                        (* space) "(")
                   (and  (or "clock_t" "time_t" "tm")
                         symbol-end)))))
    ("ctime" nil t
     ,(rx (and symbol-start
               "CLOCKS_PER_SEC"
               symbol-end)))
    ;; [std.exceptions]
    ("stdexcept" t t
     ,(rx (and symbol-start
               (or "logic_error" "domain_error"
                   "invalid_argument" "length_error"
                   "out_of_range" "runtime_error"
                   "range_error" "overflow_error" "underflow_error")
               symbol-end)))
    ;; [assertions]
    ("cassert" nil t
     ,(rx (and symbol-start
               "assert"
               (* space) "(")))
    ;; [errno]
    ("cerrno" nil t
     ,(rx (and symbol-start
               (or "E2BIG" "EACCES" "EADDRINUSE" "EADDRNOTAVAIL" "EAFNOSUPPORT" "EAGAIN"
                   "EALREADY" "EBADF" "EBADMSG" "EBUSY" "ECANCELED" "ECHILD" "ECONNABORTED"
                   "ECONNREFUSED" "ECONNRESET" "EDEADLK" "EDESTADDRREQ" "EDOM" "EEXIST"
                   "EFAULT" "EFBIG" "EHOSTUNREACH" "EIDRM" "EILSEQ" "EINPROGRESS" "EINTR"
                   "EINVAL" "EIO" "EISCONN" "EISDIR" "ELOOP" "EMFILE" "EMLINK" "EMSGSIZE"
                   "ENAMETOOLONG" "ENETDOWN" "ENETRESET" "ENETUNREACH" "ENFILE" "ENOBUFS"
                   "ENODATA" "ENODEV" "ENOENT" "ENOEXEC" "ENOLCK" "ENOLINK" "ENOMEM" "ENOMSG"
                   "ENOPROTOOPT" "ENOSPC" "ENOSR" "ENOSTR" "ENOSYS" "ENOTCONN" "ENOTDIR"
                   "ENOTEMPTY" "ENOTRECOVERABLE" "ENOTSOCK" "ENOTSUP" "ENOTTY" "ENXIO"
                   "EOPNOTSUPP" "EOVERFLOW" "EOWNERDEAD" "EPERM" "EPIPE" "EPROTO"
                   "EPROTONOSUPPORT" "EPROTOTYPE" "ERANGE" "EROFS" "ESPIPE" "ESRCH" "ETIME"
                   "ETIMEDOUT" "ETXTBSY" "EWOULDBLOCK" "EXDEV" "errno")
               symbol-end)))
    ;; [syserr]
    ("system_error" t t
     ,(rx (and symbol-start
               (or  (and (or "generic_category" "system_category"
                             "make_error_code" "make_error_condition")
                         (* space) "(")
                    (and (or "is_error_code_enum" "is_error_condition_enum")
                         (* space) "<")
                    (and (or "error_category" "error_code"
                             "error_condition" "system_error"
                             (and "errc::"
                                  (or
                                   "address_family_not_supported" "address_in_use"
                                   "address_not_available" "already_connected"
                                   "argument_list_too_long" "argument_out_of_domain"
                                   "bad_address" "bad_file_descriptor" "bad_message"
                                   "broken_pipe" "connection_aborted"
                                   "connection_already_in_progress" "connection_refused"
                                   "connection_reset" "cross_device_link"
                                   "destination_address_required" "device_or_resource_busy"
                                   "directory_not_empty" "executable_format_error"
                                   "file_exists" "file_too_large" "filename_too_long"
                                   "function_not_supported" "host_unreachable"
                                   "identifier_removed" "illegal_byte_sequence"
                                   "inappropriate_io_control_operation" "interrupted"
                                   "invalid_argument" "invalid_seek" "io_error"
                                   "is_a_directory" "message_size" "network_down"
                                   "network_reset" "network_unreachable" "no_buffer_space"
                                   "no_child_process" "no_link" "no_lock_available"
                                   "no_message_available" "no_message" "no_protocol_option"
                                   "no_space_on_device" "no_stream_resources"
                                   "no_such_device_or_address" "no_such_device"
                                   "no_such_file_or_directory" "no_such_process"
                                   "not_a_directory" "not_a_socket" "not_a_stream"
                                   "not_connected" "not_enough_memory" "not_supported"
                                   "operation_canceled" "operation_in_progress"
                                   "operation_not_permitted" "operation_not_supported"
                                   "operation_would_block" "owner_dead" "permission_denied"
                                   "protocol_error" "protocol_not_supported"
                                   "read_only_file_system" "resource_deadlock_would_occur"
                                   "resource_unavailable_try_again" "result_out_of_range"
                                   "state_not_recoverable" "stream_timeout" "text_file_busy"
                                   "timed_out" "too_many_files_open_in_system"
                                   "too_many_files_open" "too_many_links"
                                   "too_many_symbolic_link_levels" "value_too_large"
                                   "wrong_protocol_type")))
                         symbol-end)))))
    ;; [utility]
    ("utility" t t
     ,(rx (and symbol-start
               (or (and (or "swap" "exchange" "move" "move_if_noexcept")
                        (* space) (or "(" "<"))
                   (and (or "forward" "pair" "declval"
                            "integer_sequence" "index_sequence"
                            "make_integer_sequence" "make_index_sequence"
                            "index_sequence_for")
                        (* space) "<")
                   (and  (or "piecewise_construct" "piecewise_construct_t")
                         symbol-end)))))
    ;; [tuple]
    ("tuple" t t
     ,(rx (and symbol-start
               (or (and (or "ignore")
                        (* space) "(")
                   (and (or "make_tuple" "forward_as_tuple"
                            "tie" "tuple_cat")
                        (* space) (or "(" "<"))
                   (and (or "tuple" "tuple_size" "tuple_element")
                        (* space) "<")))))
    ;; [template.bitset]
    ("bitset" t t ;; ("string" "iosfwd")
     ,(rx (and symbol-start
               "bitset"
               (* space) "<")))
    ;; [memory.syn]
    ("memory" t t
     ,(rx (and symbol-start
               (or (and (or "align" "declare_reachable"
                            "declare_no_pointers" "undeclare_no_pointers"
                            "get_pointer_safety" "addressof"
                            "shared_from_this")
                        (* space) "(")
                   (and (or "undeclare_unreachable"
                            "return_temporary_buffer"
                            "uninitialized_copy" "uninitialized_copy_n"
                            "uninitialized_fill" "uninitialized_fill_n")
                        (* space) (or "(" "<"))
                   (and (or "unique_ptr" "shared_ptr" "weak_ptr"
                            "pointer_traits" "uses_allocator"
                            "allocator" "raw_storage_iterator"
                            "get_temporary_buffer"
                            "default_delete"
                            "make_unique" "make_shared" "allocate_shared"
                            (and (or "static" "dynamic" "const")
                                 "_pointer_cast")
                            "get_deleter" "owner_less")
                        (* space) "<")
                   (and (or (and "pointer_safety::"
                                 (or "relaxed" "preferred" "strict"))
                            "allocator_arg"
                            "allocator_arg_t"
                            "bad_weak_ptr"
                            "enable_shared_from_this")
                        symbol-end)))))

    ;; [c.malloc]
    ("cstdlib" t t
     ,(rx (and symbol-start
               (or "calloc" "free" "malloc" "realloc")
               (* space) "(")))
    ("cstring" t t
     ,(rx (and symbol-start
               (or "memchr" "memcmp" "memmove" "memset")
               (* space) "(")))
    ;; [functional]
    ("functional" t t
     ,(rx (and symbol-start
               (or (and (or "ref" "cref"
                            "not1" "not2"
                            "bind" "mem_fn")
                        (* space) (or "(" "<"))
                   (and (or "reference_wrapper"
                            "plus" "minus" "multiplies" "divides"
                            "modulus" "negate"
                            "equal_to" "not_equal_to"
                            "greater" "less" "greater_equal" "less_equal"
                            "logical_and" "logical_or" "logical_not"
                            "bit_and" "bit_or" "bit_xor" "bit_not"
                            "unary_negate"
                            "is_bind_expression" "is_placeholder"
                            "function" "hash")
                        (* space) "<")
                   (and "bad_function_call"
                        symbol-end)))))

    ;; [meta.type.synop]
    ("type_traits" t t
     ,(rx (and symbol-start
               (or (and (or "integral_constant"
                            "is_void" "is_null_pointer"
                            "is_integral" "is_floating_point"
                            "is_array" "is_pointer"
                            "is_lvalue_reference" "is_rvalue_reference"
                            "is_member_object_pointer" "is_member_function_pointer"
                            "is_enum" "is_union" "is_class" "is_function" "is_reference"
                            "is_arithmetic" "is_fundamental" "is_object" "is_scalar"
                            "is_compound" "is_member_pointer" "is_const" "is_volatile"
                            "is_trivial" "is_trivially_copyable" "is_standard_layout"
                            "is_pod" "is_literal_type" "is_empty" "is_polymorphic"
                            "is_abstract" "is_final" "is_signed" "is_unsigned"
                            "is_constructible" "is_default_constructible"
                            "is_copy_constructible" "is_move_constructible"
                            "is_assignable" "is_copy_assignable" "is_move_assignable"
                            "is_destructible" "is_trivially_constructible"
                            "is_trivially_default_constructible"
                            "is_trivially_copy_constructible"
                            "is_trivially_move_constructible" "is_trivially_assignable"
                            "is_trivially_copy_assignable" "is_trivially_move_assignable"
                            "is_trivially_destructible" "is_nothrow_constructible"
                            "is_nothrow_default_constructible" "is_nothrow_copy_constructible"
                            "is_nothrow_move_constructible" "is_nothrow_assignable"
                            "is_nothrow_copy_assignable" "is_nothrow_move_assignable"
                            "is_nothrow_destructible" "has_virtual_destructor"
                            "alignment_of" "rank" "extent" "is_same" "is_base_of"
                            "is_convertible"
                            (and (or "remove_const" "remove_volatile" "remove_cv"
                                     "add_const" "add_volatile" "add_cv"
                                     "remove_reference" "add_lvalue_reference"
                                     "add_rvalue_reference" "make_signed" "make_unsigned"
                                     "remove_extent" "remove_all_extents" "remove_pointer"
                                     "add_pointer" "aligned_storage" "aligned_union"
                                     "decay" "enable_if" "conditional" "common_type"
                                     "underlying_type" "result_of")
                                 (zero-or-one "_t")))
                        (* space) "<")
                   (and (or "true_type" "false_type")
                        symbol-end)))))
    ;; [ratio.syn]
    ("ratio" t t
     ,(rx (and symbol-start
               (or (and "ratio"
                        (zero-or-one (or "_add" "_subtract"
                                         "_multiply" "_divide"
                                         "_equal" "_not_equal"
                                         "_less" "_less_equal"
                                         "_greater" "_greater_equal"))
                        (* space) "<")
                   (and (or "yocto" "zepto" "atto" "femto" "pico"
                            "nano" "micro" "milli" "centi" "deci"
                            "deca" "hecto" "kilo" "mega" "giga"
                            "tera" "peta" "exa" "zetta" "yotta")
                        symbol-end)))))
    ;; [time.syn]
    ("chrono" t t
     ,(rx (and symbol-start
               (or (and (or "duration_cast" "time_point_cast")
                        (* space) (or "(" "<"))
                   (and (or "duration" "time_point"
                            "treat_as_floating_point" "duration_values")
                        (* space) "<")
                   (and (or "nanoseconds" "microseconds" "milliseconds"
                            "seconds" "minutes" "hours"
                            "system_clock" "steady_clock" "high_resolution_clock")
                        symbol-end)))))
    ;; [allocator.adaptor.syn]
    ("scoped_allocator" t t
     ,(rx (and symbol-start
               "scoped_allocator_adaptor"
               (* space) "<")))
    ;; [type.index.synopsis]
    ("typeindex" t t
     ,(rx (and symbol-start
               "type_index"
               symbol-end)))
    ;; [string.classes]
    ("string" t t
     ,(rx (and symbol-start
               (or (and (or "to_string" "to_wstring"
                            "stoi" "stol" "stoul" "stoll" "stoull"
                            "stof" "stod" "stold")
                        (* space) "(")
                   (and (or "char_traits" "basic_string")
                        (* space) "<")
                   (and (or "string" "wstring" "u16string" "u32string")
                        symbol-end)))))
    ;; [c.strings]
    ("cctype" t t
     ,(rx (and symbol-start
               (or "isalnum" "tolower" "toupper" "isblank" "isalpha"
                   "iscntrl" "isdigit" "isgraph" "islower" "isprint"
                   "ispunct" "isspace" "isupper" "isxdigit")
               (* space) "(")))
    ;; [sequences.general]
    ("array" t t
     ,(rx (and symbol-start
               "array"
               (* space) "<")))
    ("deque" t t
     ,(rx (and symbol-start
               "deque"
               (* space) "<")))
    ("forward_list" t t
     ,(rx (and symbol-start
               "forward_list"
               (* space) "<")))
    ("list" t t
     ,(rx (and symbol-start
               "list"
               (* space) "<")))
    ("vector" t t
     ,(rx (and symbol-start
               "vector"
               (* space) "<")))
    ;; [associative.map.syn]
    ("map" t t
     ,(rx (and symbol-start
               (or "map" "multimap")
               (* space) "<")))
    ;; [associative.set.syn]
    ("set" t t
     ,(rx (and symbol-start
               (or "set" "multiset")
               (* space) "<")))
    ;; [unord.map.syn]
    ("unordered_map" t t
     ,(rx (and symbol-start
               (or "unordered_map" "unordered_multimap")
               (* space) "<")))
    ;; [unord.set.syn]
    ("unordered_set" t t
     ,(rx (and symbol-start
               (or "unordered_set" "unordered_multiset")
               (* space) "<")))
    ;; [queue.syn]
    ("queue" t t
     ,(rx (and symbol-start
               (or "queue" "priority_queue")
               (* space) "<")))
    ;; [stack.syn]
    ("stack" t t
     ,(rx (and symbol-start
               "stack"
               (* space) "<")))
    ;; [iterator.syn]
    ("iterator" t t
     ,(rx (and symbol-start
               (or (and (or "iterator_traits" "iterator"
                            "reverse_iterator" "move_iterator"
                            "istream_iterator" "ostream_iterator"
                            "istreambuf_iterator" "ostreambuf_iterator")
                        (* space) "<")
                   (and (or "advance" "distance" "next" "prev"
                            "make_reverse_iterator"
                            "back_inserter"
                            "front_inserter"
                            "inserter"
                            "make_move_iterator"
                            "begin" "end" "cbegin" "cend"
                            "rbegin" "rend" "crbegin" "crend"
                            "size" "empty" "data")
                        (* space) (or "<" "("))
                   (and (or "input_iterator_tag"
                            "output_iterator_tag"
                            "forward_iterator_tag"
                            "bidirectional_iterator_tag"
                            "random_access_iterator_tag")
                        symbol-end)))))
    ;; [algorithms.general]
    ("algorithm" t t
     ,(rx (and
           (or "all_of" "any_of" "none_of"
               "for_each"
               "find" "find_if" "find_if_not"
               "find_end" "find_first_of"
               "adjacent_find"
               "count" "count_if"
               "mismatch" "equal"
               "is_permutation"
               "search" "search_n"
               "copy" "copy_n"
               "copy_if" "copy_backward"
               "move" "move_backward"
               "swap_ranges" "iter_swap"
               "transform"
               "replace" "replace_if"
               "replace_copy" "replace_copy_if"
               "fill" "fill_n"
               "generate" "generate_n"
               "remove" "remove_if"
               "remove_copy" "remove_copy_if"
               "unique" "unique_copy"
               "reverse" "reverse_copy"
               "rotate" "shuffle"
               "is_partitioned"
               "partition" "stable_partition"
               "partition_copy" "partition_point"
               "sort" "stable_sort"
               "partial_sort" "partial_sort_copy"
               "is_sorted" "is_sorted_until"
               "nth_element"
               "lower_bound" "upper_bound"
               "equal_range" "binary_search"
               "merge" "inplace_merge"
               "includes"
               "set_union" "set_intersection"
               "set_difference" "set_symmetric_difference"
               "push_heap" "pop_heap"
               "make_heap" "sort_heap"
               "is_heap" "is_heap_until"
               "min" "max" "minmax"
               "min_element" "max_element" "minmax_element"
               "lexicographical_compare"
               "next_permutation" "prev_permutation")
           (* space) (or "<" "("))))
    ;; [alg.c.library]
    ("cstdlib" t t
     ,(rx (and symbol-start
               (or "bsearch" "qsort")
               (* space) "(")))
    ;; [rand.synopsis]
    ("random" t t
     ,(rx (and symbol-start
               (or (and (or "linear_congruential_engine"
                            "mersenne_twister_engine"
                            "subtract_with_carry_engine"
                            "discard_block_engine"
                            "independent_bits_engine"
                            "shuffle_order_engine"
                            "generate_canonical"
                            "uniform_int_distribution"
                            "uniform_real_distribution"
                            "binomial_distribution"
                            "geometric_distribution"
                            "negative_binomial_distribution"
                            "poisson_distribution"
                            "exponential_distribution"
                            "gamma_distribution"
                            "weibull_distribution"
                            "extreme_value_distribution"
                            "normal_distribution"
                            "lognormal_distribution"
                            "chi_squared_distribution"
                            "cauchy_distribution"
                            "fisher_f_distribution"
                            "student_t_distribution"
                            "discrete_distribution"
                            "piecewise_constant_distribution"
                            "piecewise_linear_distribution")
                        (* space) "<")
                   (and (or "random_device" "seed_seq"
                            "minstd_rand0" "minstd_rand"
                            "mt19937" "mt19937_64"
                            "ranlux24_base" "ranlux48_base"
                            "ranlux24" "ranlux48"
                            "knuth_b"
                            "default_random_engine"
                            "bernoulli_distribution")
                        symbol-end)))))
    ;; [valarray.syn]
    ("valarray" t t
     ,(rx (and symbol-start
               (or (and (or "valarray"
                            "slice_array" "gslice_array"
                            "mask_array" "indirect_array")
                        (* space) "<")
                   (and (or "slice" "gslice")
                        symbol-end)))))
    ;; [numeric.ops.overview]
    ("numeric" t t
     ,(rx (and symbol-start
               (or "accumulate"
                   "inner_product"
                   "partial_sum"
                   "adjacent_difference"
                   "iota")
               (* space) (or "<" "("))))
    ;; [c.math]
    ("cmath" t t
     ,(rx (and symbol-start
               (or (and (or "abs" "cosh" "fmod" "logb" "remquo"
                            "acos" "erf" "frexp" "lrint" "rint"
                            "acosh" "erfc" "hypot" "lround" "round"
                            "asin" "exp2" "ilogb" "modf" "scalbln"
                            "asinh" "exp" "ldexp" "nan" "scalbn"
                            "atan" "expm1" "lgamma" "nanf" "sin"
                            "atan2" "fabs" "llrint" "nanl" "sinh"
                            "atanh" "fdim" "llround" "nearbyint" "sqrt"
                            "cbrt" "floor" "log" "nextafter" "tan"
                            "ceil" "fma" "log10" "nexttoward" "tanh"
                            "copysign" "fmax" "log1p" "pow" "tgamma"
                            "cos" "fmin" "log2" "remainder" "trunc"
                            "fpclassify" "isgreaterequal" "islessequal" "isnan" "isunordered"
                            "isfinite" "isinf" "islessgreater" "isnormal" "signbit"
                            "isgreater" "isless")
                        (* space) "(")
                   (and (or "float_t" "double_t")
                        symbol-end)))))
    ("cmath" nil t
     ,(rx (and symbol-start
               (or "FP_FAST_FMA" "FP_ILOGBNAN" "FP_SUBNORMAL" "HUGE_VALL" "MATH_ERRNO"
                   "FP_FAST_FMAF" "FP_INFINITE" "FP_ZERO INFINITY" "MATH_ERREXCEPT"
                   "FP_FAST_FMAL" "FP_NAN HUGE_VAL" "NAN" "math_errhandling"
                   "FP_ILOGB0" "FP_NORMAL" "HUGE_VALF")
               symbol-end)))
    ("cstdlib" t t
     ,(rx (and symbol-start
               (or (and (or "ldiv" "lldiv"
                            "labs" "llabs"
                            "div" "rand" "srand")
                        (* space) "(")
                   (and (or "div_t" "ldiv_t" "lldiv_t")
                        symbol-end)))))
    ("cstdlib" t t
     ,(rx (and symbol-start
               "RAND_MAX"
               symbol-end)))
    ;; [iostream.objects.overview]
    ("iostream" t t ;; ("ios" "streambuf" "istream" "ostream")
     ,(rx (and symbol-start
               (or "cin" "cout" "cerr" "clog"
                   "wcin" "wcout" "wcerr" "wclog")
               symbol-end)))
    ;; [iostreams.base.overview]
    ("ios" t t ;; ("iosfwd")
     ,(rx (and symbol-start
               (or (and (or "fpos" "basic_ios")
                        (* space) "<")
                   (and (or "streamoff" "streamsize"
                            "internal" "left" "right"
                            "dec" "hex" "oct"
                            "fixed" "scientific" "hexfloat" "defaultfloat"
                            "io_errc"
                            (and (zero-or-one "no")
                                 (or "boolalpha" "showbase" "showpoint"
                                     "skipws" "uppercase" "unitbuf")))
                        symbol-end)))))
    ;; [stream.buffers.overview]
    ("streambuf" t t
     ,(rx (and symbol-start
               (or (and (or "basic_streambuf")
                        (* space) "<")
                   (and (or "streambuf" "wstreambuf")
                        symbol-end)))))
    ;; [iostream.format.overview]
    ("istream" t t
     ,(rx (or
           (and (not (in "<[a-zA-Z_0-9]"))
                "iostream"
                symbol-end)
           (and symbol-start
                (or (and (or "basic_istream"
                             "basic_iostream")
                         (* space) "<")
                    (and (or "istream" "wistream" "wiostream" "ws")
                         symbol-end))))))
    ("ostream" t t
     ,(rx (and symbol-start
               (or (and (or "basic_ostream")
                        (* space) "<")
                   (and (or "ostream" "wostream" "endl" "ends" "flush")
                        symbol-end)))))
    ("iomanip" t t
     ,(rx (and symbol-start
               (or (and (or "resetiosflags" "setiosflags"
                            "setbase" "setprecision" "setw")
                        (* space) "(")
                   (and (or "setfill"
                            "get_money" "put_money"
                            "get_time" "put_time"
                            "quoted")
                        (* space) (or "<" "("))))))
    ;; [string.streams.overview]
    ("sstream" t t
     ,(rx (and symbol-start
               (or (and (or "basic_stringbuf"
                            "basic_istringstream"
                            "basic_ostringstream"
                            "basic_stringstream")
                        (* space) "<")
                   (and (or "stringbuf" "wstringbuf"
                            "stringstream" "wstringstream"
                            "istringstream" "wistringstream"
                            "ostringstream" "wostringstream")
                        symbol-end)))))
    ;; [fstreams]
    ("fstream" t t
     ,(rx (and symbol-start
               (or (and (or "basic_filebuf"
                            "basic_ifstream"
                            "basic_ofstream"
                            "basic_fstream")
                        (* space) "<")
                   (and (or "filebuf" "wfilebuf"
                            "fstream" "wfstream"
                            "ifstream" "wifstream"
                            "ofstream" "wofstream")
                        symbol-end)))))
    ;; [c.files]
    ("cstdio" t t
     ,(rx (and symbol-start
               (or (and (or "clearerr" "fopen" "fsetpos" "putchar" "snprintf" "vscanf"
                            "fclose" "fprintf" "ftell" "puts" "sprintf" "vsnprintf"
                            "feof" "fputc" "fwrite" "remove" "sscanf" "vsprintf"
                            "ferror" "fputs" "getc" "rename" "tmpfile" "vsscanf"
                            "fflush" "fread" "getchar" "rewind" "tmpnam"
                            "fgetc" "freopen" "perror" "scanf" "ungetc"
                            "fgetpos" "fscanf" "printf" "setbuf" "vfprintf"
                            "fgets" "fseek" "putc" "setvbuf" "vprintf")
                        (* space) "(")
                   (and (or "FILE" "fpos_t")
                        symbol-end)))))
    ("cstdio" nil t
     ,(rx (and symbol-start
               (or "BUFSIZ" "FOPEN_MAX" "SEEK_CUR" "TMP_MAX" "_IONBF" "stdout"
                   "EOF" "L_tmpnam" "SEEK_END" "_IOFBF" "stderr"
                   "FILENAME_MAX" "SEEK_SET" "_IOLBF" "stdin")
               symbol-end)))
    ("cinttypes" t t
     ,(rx (and symbol-start
               (or (and (or "imaxabs" "strtoimax" "wcstoimax"
                            "imaxdiv" "strtoumax" "wcstoumax")
                        (* space) "(")
                   (and "imaxdiv_t"
                        symbol-end)))))
    ("cinttypes" nil t
     ,(rx (and symbol-start
               (and (or "PRI" "SCN")
                    (or "d" "i" "o" "u" "x" "X")
                    (or "MAX"
                        "PTR"
                        (and (zero-or-one (or "FAST" "LEAST"))
                             (or "8" "16" "32" "64"))))
               symbol-end)))
    ;; [re.syn]
    ("regex" t t
     ,(rx (and symbol-start
               (or (and (or "regex_traits"
                            "basic_regex"
                            "sub_match"
                            "match_results"
                            "regex_iterator"
                            "regex_token_iterator")
                        (* space) "<")
                   (and (or "regex_match"
                            "regex_search"
                            "regex_replace")
                        (* space) (or "<" "("))
                   (and (or "error_type" "regex_error"
                            "regex" "wregex"
                            "csub_match" "wcsub_match"
                            "ssub_match" "wssub_match"
                            "cmatch" "wcmatch"
                            "smatch" "wsmatch"
                            "cregex_iterator" "wcregex_iterator"
                            "sregex_iterator" "wsregex_iterator"
                            "cregex_token_iterator" "wcregex_token_iterator"
                            "sregex_token_iterator" "wsregex_token_iterator"
                            "syntax_option_type" "icase" "nosubs" "optimize"
                            "collate" "ECMAScript" "basic" "extended" "awk" "grep" "egrep"
                            "match_flag_type" "match_default"
                            "match_not_bol" "match_not_eol"
                            "match_not_bow" "match_not_eow"
                            "match_any" "match_not_null"
                            "match_continuous" "match_prev_avail"
                            "format_default" "format_sed" "format_no_copy"
                            "error_collate" "error_ctype"
                            "error_escape" "error_backref"
                            "error_brack" "error_paren" "error_brace"
                            "error_badbrace" "error_range" "error_space"
                            "error_badrepeat" "error_complexity" "error_stack")
                        symbol-end)))))
    ;; [atomics.syn]
    ("atomic" t t
     ,(rx (and symbol-start
               (or (and (or "kill_dependency"
                            "atomic")
                        (* space) "<")
                   (and (or (and "atomic_flag_"
                                 (or "test_and_set"
                                     "clear")
                                 (zero-or-one "_explicit"))
                            "atomic_thread_fence"
                            "atomic_signal_fence")
                        (* space) "(")
                   (and (or "atomic_is_lock_free"
                            "atomic_init"
                            (and "atomic_"
                                 (or "load"
                                     "store"
                                     "exchange"
                                     "compare_exchange_weak"
                                     "compare_exchange_strong"
                                     "fetch_add"
                                     "fetch_sub"
                                     "fetch_and"
                                     "fetch_or"
                                     "fetch_xor")
                                 (zero-or-one "_explicit")))
                        (* space) (or "<" "("))
                   (and (or "memory_order"
                            "atomic_flag"
                            (and "memory_order_"
                                 (or "relaxed" "consume" "acquire"
                                     "release" "acq_rel" "seq_cst")))
			(and "atomic_"
			     (or "char" "schar" "uchar"
				 "short" "ushort" "int" "uint"
				 "long" "ulong" "llong" "ullong"
				 "char16_t" "char32_t" "wchar_t"))
			(and "atomic_"
			     (zero-or-one "u")
			     "int_"
			     (or "least" "fast")
			     (or "8" "16" "32" "64")
			     "_t")
			"atomic_intptr_t"
			"atomic_uintptr_t"
			"atomic_size_t"
			"atomic_ptrdiff_t"
			"atomic_intmax_t"
			"atomic_uintmax_t"
                        symbol-end)))))
    ("atomic" nil t
     ,(rx (and symbol-start
               (or (and (or "ATOMIC_VAR_INIT"
                            "ATOMIC_FLAG_INIT")
                        (* space) "(")
                   (and "ATOMIC_"
                        (or "BOOL" "CHAR" "CHAR16_T"
                            "CHAR32_T" "WCHAR_T"
                            "SHORT" "INT" "LONG" "LLONG" "POINTER")
                        "_LOCK_FREE"
                        symbol-end)))))
    ;; [thread.threads]
    ("thread" t t
     ,(rx (and symbol-start
               "thread"
               symbol-end)))
    ("thread" t t
     ,(rx (and symbol-start
               (or (and (or "sleep_until" "sleep_for")
                        (* space) (or "<" "("))
                   (and (or "get_id" "yield")
                        (* space) "(")))))
    ;; [thread.mutex]
    ("mutex" t t
     ,(rx (and symbol-start
               (or (and (or "lock" "try_lock" "call_once")
                        (* space) (or "<" "("))
                   (and (or "lock_guard" "unique_lock")
                        (* space) "<")
                   (and (or "mutex" "recursive_mutex"
                            "timed_mutex" "recursive_timed_mutex"
                            "defer_lock_t" "try_to_lock_t" "adopt_lock_t"
                            "defer_lock" "try_to_lock" "adopt_lock"
                            "once_flag")
                        symbol-end)))))
    ("shared_mutex" t t
     ,(rx (and symbol-start
               (or (and "shared_lock"
                        (* space) "<")
                   (and "shared_timed_mutex"
                        symbol-end)))))
    ("condition_variable" t t
     ,(rx (and symbol-start
               (or (and "notify_all_at_thread_exit"
                        (* space) "(")
                   (and (or "condition_variable" "condition_variable_any"
                            (and "cv_status::"
                                 (or "no_timeout"
                                     "timeout")))
                        symbol-end)))))
    ;;;
    ("cstdio" nil t
     ,(rx (and symbol-start
               (or  (and (or "scanf" "sscanf" "puts" "sprintf" "printf"
                             "gets" "fgets" "putchar")
                         (* space) "(")
                    (and  (or "FILE" "stdin" "stdout" "stderr")
                          symbol-end)))))
    ("cassert" nil t "\\bassert\\s-+(")
    ("cstring" nil t
     ,(rx (and symbol-start
               (or "memcpy" "memset" "memcmp" "memncmp"
                   "strlen" "strcmp" "strncmp" "strcpy" "strncpy" "strerr" "strcat"
                   "strstr" "strchr")
               symbol-end)))
    ("cstdlib" nil t
     ,(rx (and symbol-start
               (or (and (or "system" "abs" "atoi" "atof" "itoa"
                            "strtod" "strtold" "strtoul" "strtof" "strtol"
                            "strtoll" "strtoull" "strtoq" "strtouq"
                            "free" "at_quick_exit" "quick_exit"
			    "abort" "exit" "at_exit"  "labs" "srand" "srandom" "srandom_r"
                            "rand" "rand_r" "random" "random_r" "qsort")
                        (* space) "(")
                   (and (or (and "EXIT_" (1+ (in "A-Z")))
                            "NULL"))))))
    ("cmath" nil t
     ,(rx (and symbol-start
               (or (and (or "powf" "powl"
                            "acos" "acosf" "acosh" "acoshf" "acoshl" "acosl"
                            "asin" "asinf" "asinh" "asinhf" "asinhl" "asin"
                            "atan" "atan2" "atan2f" "atan2l" "atanf" "atanh" "atanhf"
                            "atanhl" "atanl" "exp" "expf" "expl" "exp10" "exp10f"
                            "exp10l" "exp2" "exp2f" "exp2l" "expm1" "expm1f" "expm1l"
                            "fabs" "fabsf" "fabsl" "log" "logf" "logl"
                            "log2" "log2f" "log2l" "log10" "log10f" "log10l" "log1p"
                            "log1pf" "log1pl" "nan" "nanf" "nanl"
                            "ceil" "ceilf" "ceill" "floor" "floorf" "floorl"
                            "round" "roundf" "roundl" "lround" "lroundf" "lroundl"
                            "llround" "llroundf" "llroundl" "sqrt" "sqrtf" "sqrtl")
                        (* space) "(")
                   (and (or "NAN" "INFINITY" "HUGE_VAL" "HUGE_VALF" "HUGE_VALL")
                        symbol-end)))))
    ("strings.h" nil t
     ,(rx (and symbol-start
               (or "bcmp" "bcopy" "bzero" "strcasecmp" "strncasecmp")
               (* space) "(")))
    ("typeinfo" nil t "\\btypeid\\b")
    ("new" t t ,(rx (and symbol-start
                         (or "set_new_handler" "nothrow")
                         (* space) "(")))
    ("limits" t t "\\bnumeric_limits\\s-*<\\b")
    ("algorithm" t t
     ,(rx (and symbol-start
               (or "sort" "stable_sort" "partial_sort" "partial_sort_copy"
                   "unique" "unique_copy" "reverse" "reverse_copy"
                   "nth_element" "lower_bound" "upper_bound" "binary_search"
                   "next_permutation" "prev_permutation"
                   "min" "max" "count" "random_shuffle" "swap")
               (* space) "(")))
    ("numeric" t t
     ,(rx (and symbol-start
               (or "partial_sum" "accumulate" "adjacent_difference" "inner_product")
               (* space) "(")))
    ("iostream" t t ,(rx (and symbol-start
                              (or "cin" "cout" "cerr")
                              symbol-end)))
    ("sstream" t t ,(rx (and symbol-start
                             (or "stringstream" "istringstream" "ostringstream")
                             symbol-end)))
    ("bitset" t t "\\bbitset\\s-*<\\b")
    ("complex" t t "\\bcomplex\\s-*<\\b")
    ("deque" t t "\\bdeque\\s-*<\\b")
    ("queue" t t ,(rx (and symbol-start
                           (or "queue" "priority_queue")
                           (* space) "<" word-boundary)))
    ("list" t t "\\blist\\s-*<")
    ("map" t t ,(rx (and symbol-start
                         (or "map" "multimap")
                         (* space) "<" word-boundary)))
    ("set" t t ,(rx (and symbol-start
                         (or "set" "multiset")
                         (* space) "<" word-boundary)))
    ("vector" t t "\\bvector\\s-*<")
    ("iomanip" t t ,(rx (and symbol-start
                             (or (and (or "setprecision" "setbase" "setw")
                                      (* space) "(")
                                 (and (or "fixed" "hex")
                                      symbol-end)))))
    ("fstream" t t "\\bfstream\\s-*<")
    ("ctime" nil t ,(rx (and symbol-start
                             (or (and (or "time" "clock")
                                      (* space) "(")
                                 (and (or "fixed" "hex")
                                      symbol-end)))))
    ("string" t t "\\bstring\\b")
    ("utility" t t "\\b\\(?:pair\\s-*<\\|make_pair\\)")))

(defun cpp-auto-include--include-line (header)
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward (concat "<" header ">") nil t)
         (line-number-at-pos))))

(defsubst cpp-auto-include--in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun cpp-auto-include--has-keyword-p (regexp line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (forward-line line))
    (let (finish)
      (while (and (not finish) (re-search-forward regexp nil t))
        (unless (cpp-auto-include--in-string-or-comment-p)
          (setq finish t)))
      finish)))

(defun cpp-auto-include--parse-file ()
  (cl-loop with use-std = nil
           with added = nil
           with removed = nil
           with case-fold-search = nil
           for info in cpp-auto-include--header-regexp
           for header = (nth 0 info)
           for regexp = (nth 3 info)
           for included-line = (cpp-auto-include--include-line header)
           for has-keyword = (cpp-auto-include--has-keyword-p regexp included-line)

           when (and (not use-std) has-keyword)
           do (setq use-std t)

           do
           (cond ((and has-keyword (not included-line))
                  (cl-pushnew header added :test 'equal))
                 ((and included-line (not has-keyword))
                  (cl-pushnew (cons header included-line) removed :test 'equal)))

           finally
           return (list :use-std use-std
                        :added added :removed removed)))

(defun cpp-auto-include--header-files ()
  (save-excursion
    (goto-char (point-min))
    (let ((re "^\\s-*#\\s-*include\\s-*<\\([^>]+\\)>")
          headers)
      (while (re-search-forward re nil t)
	(cl-pushnew (match-string-no-properties 1) headers :test 'equal))
      headers)))

(defun cpp-auto-include--header-insert-point ()
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^#\\s-*include\\s-*[<\"]" nil t)
      (forward-line 1)
      (point))))

(defun cpp-auto-include--add-headers (headers)
  (save-excursion
    (let ((insert-point (or (cpp-auto-include--header-insert-point) (point-min))))
      (goto-char insert-point)
      (dolist (header headers)
        (insert (format "#include <%s>\n" header)))
      (unless (re-search-forward "^\\s-*$" (line-end-position) t)
        (insert "\n")))))

(defun cpp-auto-include--remove-headers (headers)
  (save-excursion
    (cl-loop with deleted-lines = 0
             initially (goto-char (point-min))
             for (header . line) in (sort headers (lambda (a b) (< (cdr a) (cdr b))))
             for curline = 1 then (line-number-at-pos)
             do
             (progn
               (forward-line (- line curline deleted-lines))
               (let ((beg (point)))
                 (forward-line 1)
                 (delete-region beg (point))
                 (cl-incf deleted-lines))))))

;;;###autoload
(defun cpp-auto-include ()
  (interactive)
  (let* ((info (cpp-auto-include--parse-file))
         (added (plist-get info :added))
         (removed (plist-get info :removed)))
    (when removed
      (cpp-auto-include--remove-headers removed))
    (when added
      (cpp-auto-include--add-headers added))))

(provide 'cpp-auto-include)

;;; cpp-auto-include.el ends here

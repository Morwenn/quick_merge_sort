/*
 * Copyright (c) 2020 Morwenn
 * SPDX-License-Identifier: BSL-1.0
 */
#ifndef MORWENN_QUICK_MERGE_SORT_H_
#define MORWENN_QUICK_MERGE_SORT_H_

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <limits>
#include <type_traits>
#include <utility>

// Returns floor(log2(n)), assumes n > 0

template<typename Integer>
constexpr auto log2(Integer n)
    -> Integer
{
    Integer log = 0;
    while (n >>= 1) {
        ++log;
    }
    return log;
}

////////////////////////////////////////////////////////////
// swap_if, iter_swap_if

template<typename Iterator, typename Compare>
auto iter_swap_if(Iterator lhs, Iterator rhs, Compare comp)
    -> void
{
    if (comp(*rhs, *lhs)) {
        std::iter_swap(lhs, rhs);
    }
}

template<typename BidirectionalIterator, typename Compare>
auto insertion_sort_n(BidirectionalIterator first,
                      typename std::iterator_traits<BidirectionalIterator>::difference_type size,
                      Compare comp)
    -> BidirectionalIterator
{
    using difference_type = typename std::iterator_traits<BidirectionalIterator>::difference_type;

    if (size < 2) {
        return std::next(first, size);
    }

    auto it = std::next(first);
    for (difference_type i = 1 ; i < size ; ++i) {
        auto dst_it = it;
        auto src_it = std::prev(it);

        if (comp(*dst_it, *src_it)) {
            auto tmp = std::move(*dst_it);
            do {
                *dst_it = std::move(*src_it);
            } while (--dst_it != first && comp(tmp, *--src_it));
            *dst_it = std::move(tmp);
        }

        ++it;
    }

    return it;
}

template<typename ForwardIterator, typename Compare>
auto iter_sort3(ForwardIterator a, ForwardIterator b, ForwardIterator c,
                Compare compare)
    -> ForwardIterator
{
    iter_swap_if(b, c, compare);
    iter_swap_if(a, c, compare);
    iter_swap_if(a, b, std::move(compare));
    return b; // Return median of 3
}

template<typename ForwardIterator, typename Compare>
auto iter_median_5(ForwardIterator it1, ForwardIterator it2, ForwardIterator it3,
                   ForwardIterator it4, ForwardIterator it5,
                   Compare comp)
    -> ForwardIterator
{
    // Median of 5, adapted from https://stackoverflow.com/a/481398/1364752

    iter_swap_if(it1, it2, comp);
    iter_swap_if(it3, it4, comp);

    if (comp(*it1, *it3)) {
        std::iter_swap(it1, it5);
        iter_swap_if(it1, it2, comp);
    } else {
        std::iter_swap(it3, it5);
        iter_swap_if(it3, it4, comp);
    }

    if (comp(*it1, *it3)) {
        if (comp(*it2, *it3)) {
            return it3;
        }
        return it2;
    } else {
        if (comp(*it4, *it1)) {
            return it1;
        }
        return it4;
    }
}

template<typename ForwardIterator, typename Compare>
auto iter_median_rest(ForwardIterator first,
                      typename std::iterator_traits<ForwardIterator>::difference_type size,
                      Compare comp)
    -> ForwardIterator
{
    switch (size) {
        case 0:
        case 1:
        case 2:
            return first;
        case 3: {
            auto it1 = first;
            auto it2 = ++first;
            auto it3 = ++first;
            iter_swap_if(it1, it2, comp);
            iter_swap_if(it2, it3, comp);
            iter_swap_if(it1, it2, comp);
            return it2;
        }
        case 4: {
            auto it1 = first;
            auto it2 = ++first;
            auto it3 = ++first;
            auto it4 = ++first;
            iter_swap_if(it1, it2, comp);
            iter_swap_if(it3, it4, comp);
            iter_swap_if(it1, it3, comp);
            iter_swap_if(it2, it4, comp);
            iter_swap_if(it2, it3, comp);
            return it2;
        }
        case 5: {
            auto it1 = first;
            auto it2 = ++first;
            auto it3 = ++first;
            auto it4 = ++first;
            auto it5 = ++first;
            return iter_median_5(it1, it2, it3, it4, it5, std::move(comp));
        }
        default: ;
            // TODO: unreachable
    }
}

template<typename ForwardIterator, typename Compare>
auto median_of_medians(ForwardIterator first,
                       typename std::iterator_traits<ForwardIterator>::difference_type size,
                       Compare comp)
    -> ForwardIterator
{
    while (size > 5) {
        // Iterator over the collection
        auto it = first;
        // Points to the next value to replace by a median-of-5
        auto medians_it = first;

        // We handle first the biggest part that can be rounded to a power
        // of 5, then we handle the rest
        auto rounded_size = (size / 5) * 5;

        // Handle elements 5 by 5
        for (typename std::iterator_traits<ForwardIterator>::difference_type i = 0 ; i < rounded_size / 5 ; ++i) {
            auto it1 = it;
            auto it2 = ++it;
            auto it3 = ++it;
            auto it4 = ++it;
            auto it5 = ++it;

            auto median = iter_median_5(it1, it2, it3, it4, it5, comp);
            std::iter_swap(medians_it, median);
            ++medians_it;
            ++it;
        }

        // Handle remaining elements
        if (rounded_size != size) {
            auto last_median = iter_median_rest(it, size - rounded_size, comp);
            std::iter_swap(last_median, medians_it);
            ++medians_it;
        }

        // Update size for the next iteration
        size = rounded_size == size ? size / 5 : size / 5 + 1;
    }
    return iter_median_rest(first, size, std::move(comp));
}

////////////////////////////////////////////////////////////
// Pick a pivot for quickselect

template<typename BidirectionalIterator, typename Compare>
auto pick_pivot(BidirectionalIterator first, BidirectionalIterator last,
                typename std::iterator_traits<BidirectionalIterator>::difference_type size,
                int bad_allowed, Compare comp)
    -> std::pair<BidirectionalIterator, BidirectionalIterator>
{
    if (bad_allowed > 0) {
        auto it1 = std::next(first, size / 8);
        auto it2 = std::next(it1, size / 8);
        auto it3 = std::next(it2, size / 8);
        auto middle = std::next(it3, size/2 - 3*(size/8));
        auto it4 = std::next(middle, size / 8);
        auto it5 = std::next(it4, size / 8);
        auto it6 = std::next(it5, size / 8);
        auto last_1 = std::prev(last);

        iter_sort3(first, it1, it2, comp);
        iter_sort3(it3, middle, it4, comp);
        iter_sort3(it5, it6, last_1, comp);
        auto median_it = iter_sort3(it1, middle, it6, std::move(comp));
        return std::make_pair(median_it, last_1);
    } else {
        auto last_1 = std::prev(last);
        auto median_it = median_of_medians(first, size, std::move(comp));
        return std::make_pair(median_it, last_1);
    }
}

////////////////////////////////////////////////////////////
// Forward nth_element based on introselect

template<typename BidirectionalIterator, typename Compare>
auto introselect(BidirectionalIterator first, BidirectionalIterator last,
                 typename std::iterator_traits<BidirectionalIterator>::difference_type nth_pos,
                 typename std::iterator_traits<BidirectionalIterator>::difference_type size,
                 int bad_allowed, Compare comp)
    -> BidirectionalIterator
{
    while (size > 32) {
        // Choose pivot as either median of 9 or median of medians
        auto temp = pick_pivot(first, last, size, bad_allowed--, comp);
        auto median_it = temp.first;
        auto last_1 = temp.second;

        // Put the pivot at position std::prev(last) and partition
        std::iter_swap(median_it, last_1);
        auto middle1 = std::partition(
            first, last_1,
            [&](auto&& elem) { return comp(elem, *last_1); }
        );

        // Put the pivot in its final position and partition
        std::iter_swap(middle1, last_1);
        auto middle2 = std::partition(
            std::next(middle1), last,
            [&](auto&& elem) { return not comp(*middle1, elem); }
        );

        // Recursive call: heuristic trick here: in real world cases,
        // the middle partition is more likely to be smaller than the
        // right one, so computing its size should generally be cheaper
        auto size_left = std::distance(first, middle1);
        auto size_middle = std::distance(middle1, middle2);
        auto size_right = size - size_left - size_middle;

        // Recurse in the partition where the value we are
        // searching for belongs
        if (nth_pos < size_left) {
            // nth_pos is in the left partition
            last = middle1;
            size = size_left;
        } else if (nth_pos > size_left + size_middle) {
            // nth_pos is in the right partition
            first = middle2;
            nth_pos = nth_pos - size_left - size_middle;
            size = size_right;
        } else {
            // nth_pos is in the middle partition, we are done
            return std::next(middle1, nth_pos - size_left);
        }
    }

    // The remaining partition is small enough, use insertion sort
    insertion_sort_n(first, size, std::move(comp));
    return std::next(first, nth_pos);
}

////////////////////////////////////////////////////////////
// nth_element for different categories of iterators

template<typename BidirectionalIterator, typename Compare>
auto nth_element(BidirectionalIterator first, BidirectionalIterator last,
                 typename std::iterator_traits<BidirectionalIterator>::difference_type nth_pos,
                 typename std::iterator_traits<BidirectionalIterator>::difference_type size,
                 Compare comp,
                 std::bidirectional_iterator_tag)
    -> BidirectionalIterator
{
    return introselect(first, last, nth_pos, size, log2(size), std::move(comp));
}

////////////////////////////////////////////////////////////
// nth_element for random-access iterators from libc++

template<typename RandomAccessIterator, typename Compare>
auto nth_element(RandomAccessIterator first, RandomAccessIterator last,
                 typename std::iterator_traits<RandomAccessIterator>::iterator_category nth_pos,
                 typename std::iterator_traits<RandomAccessIterator>::iterator_category size,
                 Compare comp,
                 std::random_access_iterator_tag)
    -> RandomAccessIterator
{
    std::nth_element(first, first + nth_pos, last);
    return first + nth_pos;
}

////////////////////////////////////////////////////////////
// Generic nth_element overload, slightly modified compd
// to the standard library one to avoid recomputing sizes
// over and over again, which might be too expensive for
// forward and bidirectional iterators

template<typename BidirectionalIterator, typename Compare>
auto nth_element(BidirectionalIterator first, BidirectionalIterator last,
                 typename std::iterator_traits<BidirectionalIterator>::difference_type nth_pos,
                 typename std::iterator_traits<BidirectionalIterator>::difference_type size,
                 Compare comp)
    -> BidirectionalIterator
{
    using category = typename std::iterator_traits<BidirectionalIterator>::iterator_category;
    return nth_element(first, last, nth_pos, size, std::move(comp), category{});
}

constexpr int qmsort_limit = 32;

template<typename ForwardIterator, typename OutputIterator, typename Compare>
auto internal_half_inplace_merge(ForwardIterator first1, typename std::iterator_traits<ForwardIterator>::difference_type size1,
                                 ForwardIterator first2, typename std::iterator_traits<ForwardIterator>::difference_type size2,
                                 OutputIterator result, Compare comp)
    -> ForwardIterator
{
    auto min_size = size1;
    for (; min_size != 0 ; --min_size) {
        // TODO: assume
        if (comp(*first2, *first1)) {
            std::iter_swap(result, first2);
            ++first2;
            --size2;
        } else {
            std::iter_swap(result, first1);
            ++first1;
            --size1;
        }
        ++result;
    }

    for (; size1 != 0 ; ++result) {
        if (size2 == 0) {
            auto last = std::next(result, size1);
            std::swap_ranges(first1, std::next(first1, size1), result);
            return last;
        }

        if (comp(*first2, *first1)) {
            std::iter_swap(result, first2);
            ++first2;
            --size2;
        } else {
            std::iter_swap(result, first1);
            ++first1;
            --size1;
        }
    }
    // first2 through last2 are already in the right place
    return std::next(first2, size2);
}

template<typename ForwardIterator, typename Compare>
auto internal_buffered_inplace_merge(ForwardIterator first1, typename std::iterator_traits<ForwardIterator>::difference_type size1,
                                     ForwardIterator first2, typename std::iterator_traits<ForwardIterator>::difference_type size2,
                                     ForwardIterator buffer, Compare comp)
    -> ForwardIterator
{
    auto buffer_end = std::swap_ranges(first1, first2, buffer);
    return internal_half_inplace_merge(buffer, size1, first2, size2, first1, std::move(comp));
}

template<typename BidirectionalIterator, typename Compare>
auto internal_mergesort(BidirectionalIterator first, BidirectionalIterator last,
                        typename std::iterator_traits<BidirectionalIterator>::difference_type size,
                        BidirectionalIterator buffer,
                        Compare comp)
    -> void
{
    if (size <= qmsort_limit) {
        insertion_sort_n(first, size, std::move(comp));
        return;
    }

    auto first_run_size = size % qmsort_limit;
    auto it = insertion_sort_n(first, first_run_size, comp);
    do {
        it = insertion_sort_n(it, qmsort_limit, comp);
    } while (it != last);

    for (auto run_size = qmsort_limit ; run_size < size ; run_size *= 2) {
        // Handle uneven merges at the begining of the collection
        // TODO: explain handling of first runs thingy
        auto it = first;
        auto nb_remaining_elements = size % (run_size * 2);
        if (first_run_size == nb_remaining_elements) {
            // Nothing to do, proced with the other runns
            std::advance(it, first_run_size);
        } else {
            // Merge first run
            auto middle = std::next(first, first_run_size);
            it = internal_buffered_inplace_merge(
                first, first_run_size,
                middle, nb_remaining_elements - first_run_size,
                buffer, comp
            );
            first_run_size = nb_remaining_elements;
        }

        // Merge the rest of the runs pairwise
        while (it != last) {
            auto size_left = run_size;
            auto middle = std::next(it, size_left);

            // Reduce left partition as possible
            // TODO: explain why it is a good thing
            while (it != middle && not comp(*middle, *it)) {
                ++it;
                --size_left;
            }
            if (it == middle) {
                it = std::next(middle, run_size);
                continue;
            }

            it = internal_buffered_inplace_merge(it, size_left, middle, run_size, buffer, comp);
        }
    }
}

template<typename BidirectionalIterator, typename Compare>
auto quick_merge_sort(BidirectionalIterator first, BidirectionalIterator last,
                      typename std::iterator_traits<BidirectionalIterator>::difference_type size,
                      Compare comp)
    -> void
{
    // TODO: fix comment
    // This flavour of QuickMergeSort splits the collection in [2/3, 1/3]
    // partitions where the right partition is used as an internal buffer
    // to apply mergesort to the left partition, then QuickMergeSort is
    // recursively applied to the smaller right partition

    while (size > qmsort_limit) {
        // This represents both the size of the left partition
        // and the position of the pivot
        auto size_left = 2 * (size / 3) - 2;
        auto pivot = nth_element(first, last, size_left, size, comp);
        internal_mergesort(first, pivot, size_left, pivot, comp);

        if (std::is_base_of<std::random_access_iterator_tag, typename std::iterator_traits<BidirectionalIterator>::iterator_category>::value) {
            // Avoid weird codegen bug with MinGW-w64
            std::advance(first, size_left);
        } else {
            first = pivot;
        }
        size -= size_left;
    }
    insertion_sort_n(first, size, std::move(comp));
}

#endif // MORWENN_QUICK_MERGE_SORT_H_

package week2;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

class MergeSortTest {
    @Test
    public void shouldCopyArray() {
        // given
        int[] a = {6, 5, 3, 1, 8, 7, 2, 4};

        // when
        final int[] result1 = MergeSort.copyArray(a, 0, 4);
        final int[] result2 = MergeSort.copyArray(a, 5, 8);

        // then
        final int[] expected1 = {6, 5, 3, 1, 8};
        final int[] expected2 = {7, 2, 4};
        assert (Arrays.equals(result1, expected1));
        assert (Arrays.equals(result2, expected2));
    }

    @Test
    public void shouldMergeSort() {
        // given
        int[] unsortedArray = {6, 5, 3, 1, 8, 7, 2, 4};

        // when
        MergeSort.mergeSort(unsortedArray, 0, unsortedArray.length);

        // then
        System.out.println(Arrays.toString(unsortedArray));
    }
}
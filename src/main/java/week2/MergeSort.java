package week2;

import java.util.Arrays;

public class MergeSort {

    static void mergeSort(int[] arr, int low, int high) {
        if (high > low) {
            int mid = (low + high) / 2;
            final int[] firstHalf = copyArray(arr, low, mid);
            mergeSort(firstHalf, low, mid);
            final int[] latterHalf = copyArray(arr, mid + 1, high);
            mergeSort(latterHalf, mid + 1, high);
            merge(arr, low, mid, high);
        }
    }

    private static void merge(int[] arr, int l, int m, int r) {
        // Find sizes of two subarrays to be merged
        int n1 = m - l + 1;
        int n2 = r - m;

        /* Create temp arrays */
        int L[] = new int[n1];
        int R[] = new int[n2];

        /*Copy data to temp arrays*/
        for (int i = 0; i < n1; ++i)
            L[i] = arr[l + i];
        for (int j = 0; j < n2; ++j)
            R[j] = arr[m + 1 + j];


        /* Merge the temp arrays */

        // Initial indexes of first and second subarrays
        int i = 0, j = 0;

        // Initial index of merged subarry array
        int k = l;
        while (i < n1 && j < n2) {
            if (L[i] <= R[j]) {
                arr[k] = L[i];
                i++;
            } else {
                arr[k] = R[j];
                j++;
            }
            k++;
        }

        /* Copy remaining elements of L[] if any */
        while (i < n1) {
            arr[k] = L[i];
            i++;
            k++;
        }

        /* Copy remaining elements of R[] if any */
        while (j < n2) {
            arr[k] = R[j];
            j++;
            k++;
        }
    }


    /**
     * assumption are
     * - startIndex < endIndex
     * - both startIndex and endIndex are less than source length
     *
     * @param source
     * @param startIndex
     * @return
     */
    static int[] copyArray(int[] source, int startIndex, int endIndex) {
        System.out.format("%nSource=%s, startIndex=%d, endIndex=%s", Arrays.toString(source), startIndex, endIndex);
        int[] destination;
        if (startIndex == 0) {
            destination = new int[endIndex - startIndex + 1];
        } else {
            destination = new int[endIndex - startIndex];
        }
        for (int i = 0; i < destination.length; i++) {
            destination[i] = source[i + startIndex];
        }
        System.out.format(", Destination:%s", Arrays.toString(destination));
        return destination;
    }
}

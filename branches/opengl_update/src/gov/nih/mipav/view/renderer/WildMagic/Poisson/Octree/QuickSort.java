package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;


import java.util.Comparator;

public class QuickSort {

	private Comparator mComparator;

	public QuickSort(Comparator comparator) {
		mComparator = comparator;
	}

	public void qsort(Object[] v, int left, int right, Comparator comp) {
		int i, last;
		if (left >= right) return;
		swap(v, left, (left+right)/2);
		last = left;
		for ( i = left+1; i <= right; i++ ) 
			if ( comp.compare(v[i], v[left]) < 0 )
				swap(v, ++last, i);
		swap(v, left, last);
		qsort(v, left, last-1, comp);
		qsort(v, last+1, right, comp);
	}
	
   /** This is a generic version of C.A.R Hoare's Quick Sort 
    * algorithm.  This will handle arrays that are already
    * sorted, and arrays with duplicate keys.<BR>
    *
    * If you think of a one dimensional array as going from
    * the lowest index on the left to the highest index on the right
    * then the parameters to this function are lowest index or
    * left and highest index or right.  The first time you call
    * this function it will be with the parameters 0, a.length - 1.
    *
    * @param a       an Object array
    * @param lo0     left boundary of array partition
    * @param hi0     right boundary of array partition
    */
	private void qsort(Object[] a, int lo0, int hi0) {
		int lo = lo0;
		int hi = hi0;

		if ( hi0 > lo0) {
			/* Arbitrarily establishing partition element as the midpoint of
			* the array.
			*/
			Object mid = a[ ( lo0 + hi0 ) / 2 ];

			// loop through the array until indices cross
			while ( lo <= hi ) {
				/* find the first element that is greater than or equal to 
				* the partition element starting from the left Index.
				*/
				while (( lo < hi0 ) && ( mComparator.compare(a[lo], mid) < 0 ))
					++lo;

				/* find an element that is smaller than or equal to 
				* the partition element starting from the right Index.
				*/
				while (( hi > lo0 ) && ( mComparator.compare(a[hi], mid) > 0 ))
					--hi;

				// if the indexes have not crossed, swap
				if ( lo <= hi ) {
					swap(a, lo, hi);

					++lo;
					--hi;
				}
			}

			/* If the right index has not reached the left side of array
			* must now sort the left partition.
			*/
			if ( lo0 < hi )
				qsort( a, lo0, hi );

			/* If the left index has not reached the right side of array
			* must now sort the right partition.
			*/
			if ( lo < hi0 )
				qsort( a, lo, hi0 );
		}
	}

	private static void swap(Object[] a, int i, int j) {
		Object temp = a[i]; 
		a[i] = a[j];
		a[j] = temp;
	}

	public void sort(Object[] a) {
		// qsort(a, 0, a.length - 1);
		qsort(a, 0, a.length-1, mComparator);
	}
	
	public void sort(Object[] a, int length) {
		qsort(a, 0, length - 1);
	}
}
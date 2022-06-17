package gov.nih.mipav.model.algorithms;


import java.util.Random;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**
 Original code Copyright (C) 2007-2011 John Tsiombikas <nuclear@member.fsf.org>
 Ported to Java by William Gandler

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.

kdtree
======

.. image:: http://nuclear.mutantstargoat.com/sw/kdtree/img/kdtree_logo.png

Overview
--------
kdtree is a simple, easy to use C library for working with kd-trees.

Kd-trees are an extension of binary search trees to k-dimensional data. They
facilitate very fast searching, and nearest-neighbor queries.

This particular implementation is designed to be efficient and very easy to
use. It is completely written in ANSI/ISO C, and thus completely
cross-platform. 

See under the doc/ and examples/ directories to find out how to use the
kdtree library.

License
-------
Author: John Tsiombikas <nuclear@member.fsf.org>

kdtree is free software. You may use, modify, and redistribute it under the
terms of the 3-clause BSD license.

Download
--------
Latest release (0.5.7): http://nuclear.mutantstargoat.com/sw/kdtree/files/kdtree-0.5.7.tar.gz

You can find previous releases here:
http://nuclear.mutantstargoat.com/sw/kdtree/files/

You can also grab a copy of the source from github: https://github.com/jtsiomb/kdtree

Programming guide
Using the kdtree library in your programs is very easy.

Creating a tree
Call the kd_create function to create a tree. It takes a single argument specifying the dimensionality of the data,
and returns a pointer to the tree. You need to pass that pointer as an argument to any functions that manipulate the kd-tree.

For example you may create a 3-dimensional kd-tree with:
void *kd = kd_create(3);

Destroying a tree
Call kd_free with a pointer returned from kd_create in order to free the memory occupied by the tree.
Note that any data pointers passed by the user with the kd_insert functions will not be freed, unless a "data destructor" function is provided (see below).

Managing user pointers
When inserting data to the tree, you may pass a pointer to be stored in the node. If you wish, 
you may provide a custom "destructor" function to be called for each of these pointers when their node is removed from the tree.
 You can do that by supplying a pointer to that destructor function with a call to kd_data_destructor. 
The first argument is again a valid pointer to a kd-tree, while the second argument is a function pointer with the signature: void (*)(void*).

Populating the tree
To insert data to the kd-tree you may use one of the kd_insert functions.
All of the insertion functions take a valid tree pointer as their first argument, 
and an optional pointer to user data to be stored along with the node as their last argument (it can be null if no user data are needed).

kd_insert, and kd_insertf expect a pointer to an array of k doubles or floats respectively as a second argument, 
which contain the position of the inserted point. So for example, for a 3D tree you need to pass an array of 3 values.

The convenience kd_insert3, and kd_insert3f are meant to be called for 3-dimensional kd-trees
(which is considered the most common case), and expect 3 values (doubles or floats) signifying the position of the 3-dimensional point to be stored.

Performing nearest-neighbor queries
After you have your data in the kd-tree, you can perform queries for discoverying nearest neighbors in a given range around an arbitrary point.
The query returns a pointer to the "result set", which can from then on be accessed with the kd_res_* functions.

The nearest-neighbor queries are performed with the kd_nearest_range functions. Like the kd_insert functions described above,
they also provide generic array argument versions for k-dimensional trees, and 3-dimensional convenience functions, all in double and float varieties.

For example in order to query for the nearest neighbors around the 2D point (10, 15), inside a radius of 3 units, you could do:
void *result_set; double pt[] = {10.0, 15.0}; result_set = kd_nearest_range(kd, pt, 3.0); 
where "kd" is a pointer to a 2-dimensional kd-tree returned by kd_create(2).

A result set aquired with one of the kd_nearest_range functions, must be freed by calling kd_res_free and supplying the result set pointer as its only argument.

 */

/* single nearest neighbor search written by Tamas Nepusz <tamas@cs.rhul.ac.uk> */

public class KDTree {
	
	/* special list node allocators. */
	static res_node free_nodes;
	final Lock alloc = new ReentrantLock();
	
	public KDTree() {
		
	}
	
	public void test() {
		// One sample run:
		// Inserting 10 random vectors...
		// Execution time 1 = 0.002 seconds
		// Range query returned 0 items for range 40
		// Execution time 2 = 0.002 seconds
		// Range query returned 2 items for range 80
		// Execution time 3 = 0.0 seconds
		// Range query returned 10 items for range 400
		// Execution time 4 = 0.0 seconds
	    int i;
	    int vcount = 10;
	    kdtree kd;
	    kdres set;
	    Random rand = new Random();
	    double x,y,z;
	    int retVal;
	    System.out.println("Inserting " + vcount + " random vectors...");
	    kd = kd_create(3);
	    long start = System.currentTimeMillis();
	    for (i = 0; i < vcount; i++) {
	        x = 200.0 * rand.nextDouble() - 100.0;
	        y = 200.0 * rand.nextDouble() - 100.0;
	        z = 200.0 * rand.nextDouble() - 100.0;
	        
	        retVal = kd_insert3(kd, x, y, z, null);
	        if (retVal != 0) {
	        	System.err.println("Error! kd_insert3 returned " + retVal + " instead of the expected 0");
	        }
	    }
	    
	    long executionTime1 = System.currentTimeMillis() - start;
	    System.out.println("Execution time 1 = " + (executionTime1/1000.0) + " seconds");
	    
	    start = System.currentTimeMillis();
	    set = kd_nearest_range3(kd, 0, 0, 0, 40);
	    long executionTime2 = System.currentTimeMillis() - start;
	    System.out.println("Range query returned " + kd_res_size(set) + " items for range 40");
	    System.out.println("Execution time 2 = " + (executionTime2/1000.0) + " seconds");
	    
	    start = System.currentTimeMillis();
	    set = kd_nearest_range3(kd, 0, 0, 0, 80);
	    long executionTime3 = System.currentTimeMillis() - start;
	    System.out.println("Range query returned " + kd_res_size(set) + " items for range 80");
	    System.out.println("Execution time 3 = " + (executionTime3/1000.0) + " seconds");
	    
	    start = System.currentTimeMillis();
	    set = kd_nearest_range3(kd, 0, 0, 0, 400);
	    long executionTime4 = System.currentTimeMillis() - start;
	    System.out.println("Range query returned " + kd_res_size(set) + " items for range 400");
	    System.out.println("Execution time 4 = " + (executionTime4/1000.0) + " seconds");
	    kd_res_free(set);

		kd_free(kd);
		return;
	}
	
	public void test2() {
		// Sample test run works:
		// Found 3 results
		// Node at (0.342441182898126,3.080631838451662,9.022622816724176) is 8.600583430631325 away and has data = 7.5
		// Node at (-5.390746577442254,4.895129480676994,3.288425374918962) is 7.632780095834766 away and has data = 5.5
		// Node at (0.04258996752281874,-2.6600690773943185,-8.12944018791437) is 9.509177648280055 away and has data = 4.5
		/* Extended test program, contributed by David Underhill */
		int DEF_NUM_PTS = 10;
		Random rnd = new Random();
		int i;
		int num_pts = DEF_NUM_PTS;
		kdtree ptree;
		double data[][][];
		double pch[][];
		kdres presults;
		double pos[] = new double[3];
		double dist;
		double pt[] = new double[]{ 0, 0, 1 };
		double radius = 10;
		int retVal;
		
		data = new double[num_pts][1][1];
		
		/* create a k-d tree for 3-dimensional points */
		ptree = kd_create( 3 );
		
		/* add some random nodes to the tree (assert nodes are successfully inserted) */
		  for( i=0; i<num_pts; i++ ) {
		    data[i][0][0] = i + 0.5;
		    retVal =  kd_insert3( ptree, rd(rnd), rd(rnd), rd(rnd), data[i]);
		    if (retVal != 0) {
		    	System.err.println("kd_insert3 returned " + retVal + " instead of the expected 0");
		    }
		  }
		  
		  /* find points closest to the origin and within distance radius */
		  presults = kd_nearest_range( ptree, pt, radius );
		  
		  /* print out all the points found in results */
		  System.out.println("Found " + kd_res_size(presults) + " results");
		  
		  while(kd_res_end(presults) == 0) {
			  /* get the data and position of the current result item */
			  pch = kd_res_item( presults, pos );
			    
			  /* compute the distance of the current result from the pt */
			  dist = Math.sqrt( dist_sq( pt, pos, 3 ) );
			  
			  /* print out the retrieved data */
			  System.out.println("Node at (" + pos[0]+","+pos[1]+","+pos[2]+") is " + dist + " away and has data = " + pch[0][0]);
			  
			  /* go to the next entry */
			  kd_res_next( presults );
		  } // while(kd_res_end( presults ) != 0)
		  
		  /* free our tree, results set, and other allocated memory */
		  data = null;
		  kd_res_free( presults );
		  kd_free( ptree );

		  return;
	}
	
	/* returns the distance squared between two dims-dimensional double arrays */
	public double dist_sq(double a1[], double a2[], int dims) {
		double dist_sq = 0, diff;
		  while( --dims >= 0 ) {
		    diff = (a1[dims] - a2[dims]);
		    dist_sq += diff*diff;
		  }
		  return dist_sq;	
	}
	
	/* get a random double between -10 and 10 */
	public double rd(Random rnd) {
	   return (20.0 * rnd.nextDouble() - 10.0 );	
	}
	
	public double SQ(double x) {
		return x*x;
	}
	
	class kdhyperrect {
		int dim;
		/* minimum/maximum coords */
		double min[];
		double max[];  
		
		public kdhyperrect() {
			
		}
	};
	
	class kdnode {
		double pos[];
		int dir;
		//void *data;
		double data[][];
		/* negative/positive side */
		kdnode left;
		kdnode right;
		
		public kdnode() {
			
		}
	};
	
	class res_node {
	    kdnode item;
		double dist_sq;
		res_node next;
		
		public res_node() {
			
		}
	};
	
	class kdtree {
		int dim;
		kdnode root;
		kdhyperrect rect;
		//void (*destr)(void*);
		
		public kdtree() {
			
		}
	};
	
	class kdres {
		kdtree tree;
		res_node rlist;
		res_node riter;
		int size;
		
		public kdres() {
			
		}
	};
	
	/* create a kd-tree for "k"-dimensional data */
	public kdtree kd_create(int k) {
	    kdtree tree	= new kdtree();
	    tree.dim = k;
		tree.root = null;
		//tree.destr = null;
		tree.rect = null;

		return tree;
	}
	
	/* free the struct kdtree */
	public void kd_free(kdtree tree)
	{
		if(tree != null) {
			kd_clear(tree);
			tree = null;
		}
	}
	
	/* remove all the elements from the tree */
	public void kd_clear(kdtree tree)
	{
		clear_rec(tree.root/*, tree->destr*/);
		tree.root = null;

		if (tree.rect != null) {
			hyperrect_free(tree.rect);
			tree.rect = null;
		}
	}
	
	public static void clear_rec(kdnode node/*, void (*destr)(void*)*/)
	{
		if(node == null) return;

		clear_rec(node.left/*, destr*/);
		clear_rec(node.right/*, destr*/);
		
		//if(destr) {
		//	destr(node->data);
		//}
		node.pos = null;
		node = null;
	}
	
	public int insert_rec(kdnode nptr, boolean nptrwasnull, double pos[], double data[][], int dir, int dim)
	{ 
		int i;
		int new_dir;
		boolean leftnptrwasnull;
		boolean rightnptrwasnull;

		if(nptrwasnull) {
			nptr.pos = new double[dim];
			for (i = 0; i < dim; i++) {
				nptr.pos[i] = pos[i];
			}
			nptr.data = data;
			nptr.dir = dir;
			nptr.left = nptr.right = null;
			return 0;
		}

		new_dir = (nptr.dir + 1) % dim;
		if(pos[nptr.dir] < nptr.pos[nptr.dir]) {
			if (nptr.left == null) {
				nptr.left = new kdnode();
				leftnptrwasnull = true;
			}
			else {
				leftnptrwasnull = false;
			}
			return insert_rec(nptr.left, leftnptrwasnull, pos, data, new_dir, dim);
		}
		if (nptr.right == null) {
			nptr.right = new kdnode();
			rightnptrwasnull = true;
		}
		else {
			rightnptrwasnull = false;
		}
		return insert_rec(nptr.right, rightnptrwasnull, pos, data, new_dir, dim);
	}
	
	/* insert a node, specifying its position, and optional data */
	public int kd_insert(kdtree tree, double pos[], double data[][])
	{
		boolean nptrwasnull;
		if (tree.root == null) {
			tree.root = new kdnode();
			nptrwasnull = true;
		}
		else {
			nptrwasnull = false;
		}
		if (insert_rec(tree.root, nptrwasnull, pos, data, 0, tree.dim) != 0) {
			return -1;
		}

		if (tree.rect == null) {
			tree.rect = hyperrect_create(tree.dim, pos, pos);
		} else {
			hyperrect_extend(tree.rect, pos);
		}

		return 0;
	}
	
	public int kd_insertf(kdtree tree, double pos[], double data[][])
	{
		int index;
		double sbuf[] = new double[16];
		double bptr[] = null;
		double buf[] = null;
		int res;
		int dim = tree.dim;

		if(dim > 16) {
			bptr = buf = new double[dim];
		} else {
			bptr = buf = sbuf;
		}

		index = 0;
		while(dim-- > 0) {
			bptr[index] = pos[index];
			index++;
		}

		res = kd_insert(tree, buf, data);
	
		if(tree.dim > 16) {
			buf = null;
		}
		return res;
	}
	
	public int kd_insert3(kdtree tree, double x, double y, double z, double data[][])
	{
		double buf[] = new double[3];
		buf[0] = x;
		buf[1] = y;
		buf[2] = z;
		return kd_insert(tree, buf, data);
	}
	
	public int kd_insert3f(kdtree tree, float x, float y, float z, double data[][])
	{
		double buf[] = new double[3];
		buf[0] = x;
		buf[1] = y;
		buf[2] = z;
		return kd_insert(tree, buf, data);
	}
	
	/* Find the nearest node from a given point.
	 *
	 * This function returns a pointer to a result set with at most one element.
	 */
	public int find_nearest(kdnode node, double pos[], double range, res_node list, int ordered, int dim)
	{
		double dist_sq, dx;
		int i, ret, added_res = 0;

		if(node == null) return 0;

		dist_sq = 0;
		for(i=0; i<dim; i++) {
			dist_sq += SQ(node.pos[i] - pos[i]);
		}
		if(dist_sq <= SQ(range)) {
			if(rlist_insert(list, node, (ordered != 0) ? dist_sq : -1.0) == -1) {
				return -1;
			}
			added_res = 1;
		}

		dx = pos[node.dir] - node.pos[node.dir];

		ret = find_nearest(dx <= 0.0 ? node.left : node.right, pos, range, list, ordered, dim);
		if(ret >= 0 && Math.abs(dx) < range) {
			added_res += ret;
			ret = find_nearest(dx <= 0.0 ? node.right : node.left, pos, range, list, ordered, dim);
		}
		if(ret == -1) {
			return -1;
		}
		added_res += ret;

		return added_res;
	}
	
	public void kd_nearest_i(kdnode node, double pos[], kdnode result, double result_dist_sq[], kdhyperrect rect)
	{
		int dir = node.dir;
		int i;
		double dummy, dist_sq;
		kdnode nearer_subtree, farther_subtree;
		double nearer_hyperrect_coord[] = new double[rect.dim];
		double farther_hyperrect_coord[] = new double[rect.dim];

		/* Decide whether to go left or right in the tree */
		dummy = pos[dir] - node.pos[dir];
		if (dummy <= 0) {
			nearer_subtree = node.left;
			farther_subtree = node.right;
			for (i = 0; i < rect.dim; i++) {
			    nearer_hyperrect_coord[i] = rect.max[i] + dir;
			    farther_hyperrect_coord[i] = rect.min[i] + dir;
			}
		} else {
			nearer_subtree = node.right;
			farther_subtree = node.left;
			for (i = 0; i < rect.dim; i++) {
			   nearer_hyperrect_coord[i] = rect.min[i] + dir;
			   farther_hyperrect_coord[i] = rect.max[i] + dir;
			}
		}

		if (nearer_subtree != null) {
			/* Slice the hyperrect to get the hyperrect of the nearer subtree */
			dummy = nearer_hyperrect_coord[0];
			nearer_hyperrect_coord[0] = node.pos[dir];
			/* Recurse down into nearer subtree */
			kd_nearest_i(nearer_subtree, pos, result, result_dist_sq, rect);
			/* Undo the slice */
			nearer_hyperrect_coord[0] = dummy;
		}

		/* Check the distance of the point at the current node, compare it
		 * with our best so far */
		dist_sq = 0;
		for(i=0; i < rect.dim; i++) {
			dist_sq += SQ(node.pos[i] - pos[i]);
		}
		if (dist_sq < result_dist_sq[0]) {
			result = node;
			result_dist_sq[0] = dist_sq;
		}

		if (farther_subtree != null) {
			/* Get the hyperrect of the farther subtree */
			dummy = farther_hyperrect_coord[0];
			farther_hyperrect_coord[0] = node.pos[dir];
			/* Check if we have to recurse down by calculating the closest
			 * point of the hyperrect and see if it's closer than our
			 * minimum distance in result_dist_sq. */
			if (hyperrect_dist_sq(rect, pos) < result_dist_sq[0]) {
				/* Recurse down into farther subtree */
				kd_nearest_i(farther_subtree, pos, result, result_dist_sq, rect);
			}
			/* Undo the slice on the hyperrect */
			farther_hyperrect_coord[0] = dummy;
		}
	}
	
	public kdres kd_nearest(kdtree kd, double pos[])
	{
		kdhyperrect rect;
		kdnode result;
		kdres rset;
		double dist_sq[] = new double[1];
		int i;

		if (kd == null) return null;
		if (kd.rect == null) return null;

		/* Allocate result set */
		rset = new kdres();
		rset.rlist = alloc_resnode();
		rset.rlist.next = null;
		rset.tree = kd;

		/* Duplicate the bounding hyperrectangle, we will work on the copy */
		rect = hyperrect_duplicate(kd.rect);

		/* Our first guesstimate is the root node */
		result = kd.root;
		dist_sq[0] = 0;
		for (i = 0; i < kd.dim; i++)
			dist_sq[0] += SQ(result.pos[i] - pos[i]);

		/* Search for the nearest neighbour recursively */
		kd_nearest_i(kd.root, pos, result, dist_sq, rect);

		/* Free the copy of the hyperrect */
		hyperrect_free(rect);

		/* Store the result */
		if (result != null) {
			if (rlist_insert(rset.rlist, result, -1.0) == -1) {
				kd_res_free(rset);
				return null;
			}
			rset.size = 1;
			kd_res_rewind(rset);
			return rset;
		} else {
			kd_res_free(rset);
			return null;
		}
	}
	
	public kdres kd_nearestf(kdtree tree, float pos[])
	{
		double sbuf[] = new double[16];
		double bptr[];
		double buf[] = null;
		int dim = tree.dim;
		kdres res;
		int index;

		if(dim > 16) {
			bptr = buf = new double[dim];
					
		} else {
			bptr = buf = sbuf;
		}

		index = 0;
		while(dim-- > 0) {
			bptr[index] = pos[index];
			index++;
		}

		res = kd_nearest(tree, buf);
	
		if(tree.dim > 16) {
			buf = null;
		}
	
		return res;
	}
	
	public kdres kd_nearest3(kdtree tree, double x, double y, double z)
	{
		double pos[] = new double[3];
		pos[0] = x;
		pos[1] = y;
		pos[2] = z;
		return kd_nearest(tree, pos);
	}
	
	public kdres kd_nearest3f(kdtree tree, float x, float y, float z)
	{
		double pos[] = new double[3];
		pos[0] = x;
		pos[1] = y;
		pos[2] = z;
		return kd_nearest(tree, pos);
	}
	
	/* Find any nearest nodes from a given point within a range.
	 *
	 * This function returns a pointer to a result set, which can be manipulated
	 * by the kd_res_* functions.
	 * The returned pointer can be null as an indication of an error. Otherwise
	 * a valid result set is always returned which may contain 0 or more elements.
	 * The result set must be deallocated with kd_res_free after use.
	 */
	public kdres kd_nearest_range(kdtree kd, double pos[], double range)
	{
		int ret;
		kdres rset;

		rset = new kdres();
		rset.rlist = alloc_resnode();
		rset.rlist.next = null;
		rset.tree = kd;

		if((ret = find_nearest(kd.root, pos, range, rset.rlist, 0, kd.dim)) == -1) {
			kd_res_free(rset);
			return null;
		}
		rset.size = ret;
		kd_res_rewind(rset);
		return rset;
	}
	
	public kdres kd_nearest_rangef(kdtree kd, float pos[], float range)
	{
		double sbuf[] = new double[16];
		double bptr[];
		double buf[] = null;
		int dim = kd.dim;
		kdres res;
		int index;

		if(dim > 16) {
			bptr = buf = new double[dim];
		} else {
			bptr = buf = sbuf;
		}

		index = 0;
		while(dim-- > 0) {
			bptr[index] = pos[index];
			index++;
		}

		res = kd_nearest_range(kd, buf, range);
		if(kd.dim > 16) {
			buf = null;
		}
		return res;
	}
	
	public kdres kd_nearest_range3(kdtree tree, double x, double y, double z, double range)
	{
		double buf[] = new double[3];
		buf[0] = x;
		buf[1] = y;
		buf[2] = z;
		return kd_nearest_range(tree, buf, range);
	}
	
	public kdres kd_nearest_range3f(kdtree tree, float x, float y, float z, float range)
	{
		double buf[] = new double[3];
		buf[0] = x;
		buf[1] = y;
		buf[2] = z;
		return kd_nearest_range(tree, buf, range);
	}
	
	/* frees a result set returned by kd_nearest_range() */
	public void kd_res_free(kdres rset)
	{
		clear_results(rset);
		free_resnode(rset.rlist);
		rset = null;
	}
	
	/* returns the size of the result set (in elements) */
	public int kd_res_size(kdres set)
	{
		return (set.size);
	}
	
	/* rewinds the result set iterator */
	public void kd_res_rewind(kdres rset)
	{
		rset.riter = rset.rlist.next;
	}
	
	/* returns non-zero if the set iterator reached the end after the last element */
	public int kd_res_end(kdres rset)
	{
		if (rset.riter == null) {
			return 1;
		}
		else {
			return 0;
		}
	}
	
	/* advances the result set iterator, returns non-zero on success, zero if
	 * there are no more elements in the result set.
	 */
	public int kd_res_next(kdres rset)
	{
		rset.riter = rset.riter.next;
		if (rset.riter != null) {
			return 1;
		}
		else {
			return 0;
		}
	}
	
	/* returns the data pointer (can be null) of the current result set item
	 * and optionally sets its position to the pointers(s) if not null.
	 */
	public double[][] kd_res_item(kdres rset, double pos[])
	{
		int i;
		if(rset.riter != null) {
			if(pos != null) {
				for (i = 0; i < rset.tree.dim; i++) {
					pos[i] = rset.riter.item.pos[i];
				}
			}
			return rset.riter.item.data;
		}
		return null;
	}
	
	public double[][] kd_res_itemf(kdres rset, float pos[])
	{
		int i;
		if(rset.riter != null) {
			if(pos != null) {
				for(i=0; i<rset.tree.dim; i++) {
					pos[i] = (float)rset.riter.item.pos[i];
				}
			}
			return rset.riter.item.data;
		}
		return null;
	}
	
	public double[][] kd_res_item3(kdres rset, double x[], double y[], double z[])
	{
		if(rset.riter != null) {
			if(x != null) x[0] = rset.riter.item.pos[0];
			if(y != null) y[0] = rset.riter.item.pos[1];
			if(z != null) z[0] = rset.riter.item.pos[2];
			return rset.riter.item.data;
		}
		return null;
	}
	
	public double[][] kd_res_item3f(kdres rset, float x[], float y[], float z[])
	{
		if(rset.riter != null) {
			if(x != null) x[0] = (float)rset.riter.item.pos[0];
			if(y != null) y[0] = (float)rset.riter.item.pos[1];
			if(z != null) z[0] = (float)rset.riter.item.pos[2];
			return rset.riter.item.data;
		}
		return null;
	}
	
	/* equivalent to kd_res_item(set, 0) */
	public double[][] kd_res_item_data(kdres set)
	{
		return kd_res_item(set, null);
	}

	
	/* ---- hyperrectangle helpers ---- */
	public kdhyperrect hyperrect_create(int dim, double min[], double max[])
	{ 
		int i;
		kdhyperrect rect = null;

		rect = new kdhyperrect();

		rect.dim = dim;
		rect.min = new double[dim];
		rect.max = new double[dim];
		for (i = 0; i < dim; i++) {
			rect.min[i] = min[i];
			rect.max[i] = max[i];
		}
		
		return rect;
	}
	
	public static void hyperrect_free(kdhyperrect rect)
	{
		rect.min = null;
		rect.max = null;
		rect = null;
	}
	
	public kdhyperrect hyperrect_duplicate(kdhyperrect rect)
	{
		return hyperrect_create(rect.dim, rect.min, rect.max);
	}
	
	public static void hyperrect_extend(kdhyperrect rect, double pos[])
	{
		int i;

		for (i=0; i < rect.dim; i++) {
			if (pos[i] < rect.min[i]) {
				rect.min[i] = pos[i];
			}
			if (pos[i] > rect.max[i]) {
				rect.max[i] = pos[i];
			}
		}
	}
	
	public double hyperrect_dist_sq(kdhyperrect rect, double pos[])
	{
		int i;
		double result = 0;

		for (i=0; i < rect.dim; i++) {
			if (pos[i] < rect.min[i]) {
				result += SQ(rect.min[i] - pos[i]);
			} else if (pos[i] > rect.max[i]) {
				result += SQ(rect.max[i] - pos[i]);
			}
		}

		return result;
	}

	
	public res_node alloc_resnode()
	{
		res_node node;
		
		alloc.lock();
	

		if(free_nodes == null) {
			node = new res_node();
		} else {
			node = free_nodes;
			free_nodes = free_nodes.next;
			node.next = null;
		}
		
		alloc.unlock();

		return node;
	}
	
	public void free_resnode(res_node node)
	{
	    alloc.lock();

		node.next = free_nodes;
		free_nodes = node;

	    alloc.unlock();
	}
	
	/* inserts the item. if dist_sq is >= 0, then do an ordered insert */
	/* TODO make the ordering code use heapsort */
	public int rlist_insert(res_node list, kdnode item, double dist_sq)
	{
		res_node rnode;

		rnode = alloc_resnode();
		rnode.item = item;
		rnode.dist_sq = dist_sq;

		if(dist_sq >= 0.0) {
			while((list.next != null) && list.next.dist_sq < dist_sq) {
				list = list.next;
			}
		}
		rnode.next = list.next;
		list.next = rnode;
		return 0;
	}
	
	public void clear_results(kdres rset)
	{
		res_node tmp;
		res_node node = rset.rlist.next;

		while(node != null) {
			tmp = node;
			node = node.next;
			free_resnode(tmp);
		}

		rset.rlist.next = null;
	}

	
}
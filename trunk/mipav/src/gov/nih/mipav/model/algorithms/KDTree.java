package gov.nih.mipav.model.algorithms;


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

 */

/* single nearest neighbor search written by Tamas Nepusz <tamas@cs.rhul.ac.uk> */

public class KDTree {
	
	class kdhyperrect {
		int dim;
		/* minimum/maximum coords */
		double min[];
		double max[];              
	};
	
	class kdnode {
		double pos[];
		int dir;
		//void *data;
		double data[];
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
	};
	
	public kdtree kd_create(int k) {
	    kdtree tree	= new kdtree();
	    tree.dim = k;
		tree.root = null;
		//tree.destr = null;
		tree.rect = null;

		return tree;
	}
	
	public void kd_free(kdtree tree)
	{
		if(tree != null) {
			kd_clear(tree);
			tree = null;
		}
	}
	
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
	
	public static void hyperrect_free(kdhyperrect rect)
	{
		rect.min = null;
		rect.max = null;
		rect = null;
	}
	
	public int insert_rec(kdnode nptr, double pos[], double data[], int dir, int dim)
	{ 
		int i;
		int new_dir;
		kdnode node;

		if(nptr == null) {
			node = new kdnode();
			node.pos = new double[dim];
			for (i = 0; i < dim; i++) {
				node.pos[i] = pos[i];
			}
			node.data = data;
			node.dir = dir;
			node.left = node.right = null;
			nptr = node;
			return 0;
		}

		node = nptr;
		new_dir = (node.dir + 1) % dim;
		if(pos[node.dir] < node.pos[node.dir]) {
			return insert_rec(nptr.left, pos, data, new_dir, dim);
		}
		return insert_rec(nptr.right, pos, data, new_dir, dim);
	}
	
}
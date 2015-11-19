package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry.*;

public class DivergenceFunction extends NodeAdjacencyFunction {

	public Point3D normal = new Point3D();
	public Octree ot;
	int[] index = new int[Octree.DIMENSION];
	// int[][] scratch = new int[Octree.DIMENSION][1];
	int[] scratch0 = new int[1];
	int[] scratch1 = new int[1];
	int[] scratch2 = new int[1];

	public void Function(OctNode node1, final OctNode node2) {
	    Point3D n = normal;
	    Point3D nn = new Point3D();
	    
	    nn.coords[0] = n.coords[0];
	    nn.coords[1] = n.coords[1];
	    nn.coords[2] = n.coords[2];
	    
	    // System.err.println("n.coords[0] = " + n.coords[0] + " n.coords[1] = " + n.coords[1] + " n.coords[2] = " + n.coords[2]); 
	    if (FunctionData.SymmetricIndex(index[0], (int)(node1.off[0]), scratch0)) {
			nn.coords[0] = -n.coords[0];
		}
		if (FunctionData.SymmetricIndex(index[1],  (int)(node1.off[1]), scratch1)) {
			nn.coords[1] = -n.coords[1];
		}
		if (FunctionData.SymmetricIndex(index[2], (int)(node1.off[2]), scratch2)) {
			nn.coords[2] = -n.coords[2];
		}
	
		double dot = ot.fData.dotTable[scratch0[0]]
				* ot.fData.dotTable[scratch1[0]] * ot.fData.dotTable[scratch2[0]];
		
		node1.nodeData.value += (float) (
				dot * 
				(ot.fData.dDotTable[scratch0[0]] * nn.coords[0] + 
				 ot.fData.dDotTable[scratch1[0]] * nn.coords[1] + 
				 ot.fData.dDotTable[scratch2[0]] * nn.coords[2]));
	    /*
	    System.err.println("scratch[0] = " + scratch0[0] + " scratch[1] = " + scratch1[0] + " scratch[2] = " + scratch2[0]);
		System.err.println("nn.coords[0] = " + nn.coords[0] + " nn.coords[1] = " + nn.coords[1] + " nn.coords[2] = " + nn.coords[2]); 
		System.err.println("index[0] = " + index[0] + " index[1] = " + index[1] + " index[2] = " + index[2]);
		System.err.println("node1.off[0]  = " + (int)node1.off[0] + " node1.off[1] = " + (int)node1.off[1] + " node1.off[2] = " + (int)node1.off[2]);
		System.err.println("ot.fData.dDotTable[scratch0[0]] = "+ ot.fData.dDotTable[scratch0[0]] + 
				           " ot.fData.dDotTable[scratch1[0]] = " + ot.fData.dDotTable[scratch1[0]] + 
				           " ot.fData.dDotTable[scratch2[0]] = " + ot.fData.dDotTable[scratch2[0]]);
		System.err.println("DivergenceFunction: node1.nodeData.value = " + node1.nodeData.value);
		
		Octree.pause();
	    */
	}

}
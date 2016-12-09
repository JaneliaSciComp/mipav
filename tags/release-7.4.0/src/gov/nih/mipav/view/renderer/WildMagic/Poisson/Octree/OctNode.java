package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes.*;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry.*;
import java.util.*;

public class OctNode implements Comparator {
	
	private static int UseAlloc = 0;

	private Geometry geo = new Geometry();
	public static int DIMENSION = 3;
	
	public static Allocator<OctNode> Allocator = new Allocator<OctNode>();
	
	public OctNode parent;
	public OctNode[] children;
	public short[] d = new short[1];
	public short[] off = new short[3];
	public TreeNodeData nodeData = new TreeNodeData();

	public static final int DepthShift=5;
	public static final int OffsetShift=19;
	public static final int DepthMask=(1<<DepthShift)-1;
	public static final int OffsetMask=(1<<OffsetShift)-1;
	public static final int OffsetShift1=DepthShift;
	public static final int OffsetShift2=OffsetShift1+OffsetShift;
	public static final int OffsetShift3=OffsetShift2+OffsetShift;
	
	public OctNode copy() {
		OctNode node = new OctNode();
		node.parent = this.parent;
		node.children = this.children;
		node.d = this.d;
		node.off = this.off;
		node.nodeData = this.nodeData;
		return node;
	}
	
	public static void SetAllocator(int blockSize)
	{
		if(blockSize>0)
		{
			UseAlloc=1;
			Allocator.set(blockSize);
		}
		else{UseAlloc=0;}
	}
	
	public static int UseAllocator(){return UseAlloc;}

	public OctNode(){
		parent=null;
		children=null;
		d[0]=off[0]=off[1]=off[2]= (short)0;
	}
	
	public void dispose(){
		if(UseAlloc == 0){if(children != null){children = null;}}
		parent=null;
		children=null;
	}
	
	public void setFullDepth(final int maxDepth){
		if(maxDepth != 0){
			if(children == null){initChildren();}
			for(int i=0;i<8;i++){children[i].setFullDepth(maxDepth-1);}
		}
	}
	
	public int initChildren(){
		int i,j,k;

		if(UseAlloc == 1 ){
			// children=(OctNode[])Allocator.newElements(8);
			children = new OctNode[8];
			for ( i = 0; i < 8; i++ ) {
				children[i] = new OctNode();	
			}
		} else{
			if(children != null )
			children= null;
			children=new OctNode[Cube.CORNERS];
			for ( i = 0; i < Cube.CORNERS; i++ ) {
				children[i] = new OctNode();
			}
		}
		if(children == null ){
			System.err.println("Failed to initialize children in OctNode::initChildren\n");
			System.exit(0);
			return 0;
		}
		int[] d = new int[1];
		int[] offf = new int[3];
		depthAndOffset(d,offf);
		for(i=0;i<2;i++){
			for(j=0;j<2;j++){
				for(k=0;k<2;k++){
					int idx=Cube.CornerIndex(i,j,k);
					children[idx].parent=this;
					children[idx].children=null;
					int[] off2 = new int[3];
					off2[0]=(offf[0]<<1)+i;
					off2[1]=(offf[1]<<1)+j;
					off2[2]=(offf[2]<<1)+k;
					Index(d[0]+1,off2,children[idx].d,children[idx].off);
				}
			}
		}
		return 1;
	}

	public static final void Index(final int depth,final int offset[], final short[] d, short off[]){
		d[0]= (short)depth;
		off[0]= (short)((1<<depth)+offset[0]-1);
		off[1]= (short)((1<<depth)+offset[1]-1);
		off[2]= (short)((1<<depth)+offset[2]-1);
		// System.err.println(" d[0] = " + (int)d[0] + " off[0] = " + (int)off[0] + " off[1] = " + (int)off[1] + " off[2] = " + (int)off[2]);
		// Octree.pause();
	}
	
	public final void depthAndOffset(int[] depth,int offset[]) {
		depth[0]= (int)(d[0]);
		offset[0]=((int)(off[0])+1)&(~(1<<depth[0]));
		offset[1]=((int)(off[1])+1)&(~(1<<depth[0]));
		offset[2]=((int)(off[2])+1)&(~(1<<depth[0]));
	}
	
	public final int depth() {return new Short(d[0]).intValue();}
	
	public static final void DepthAndOffset(final long index,int[] depth,int offset[]){
		depth[0]=(int)(index&DepthMask);
		offset[0]=((int)((index>>OffsetShift1)&OffsetMask)+1)&(~(1<<depth[0]));
		offset[1]=((int)((index>>OffsetShift2)&OffsetMask)+1)&(~(1<<depth[0]));
		offset[2]=((int)((index>>OffsetShift3)&OffsetMask)+1)&(~(1<<depth[0]));
	}
	
	public static final int Depth(final long index){
		return (int)(index&DepthMask);
	}
	
	public final void centerAndWidth(Point3D center,float[] width) {
		int depth;
		int[] offset = new int[3];
		depth=(int)(d[0]);
		offset[0]=((int)(off[0])+1)&(~(1<<depth));
		offset[1]=((int)(off[1])+1)&(~(1<<depth));
		offset[2]=((int)(off[2])+1)&(~(1<<depth));
		width[0]=(float)(1.0f/(1<<depth));
		for(int dim=0;dim<DIMENSION;dim++){
			center.coords[dim]=(float)(0.5f+offset[dim])*width[0];}
	}

	public static final void CenterAndWidth(final long index,Point3D center, float[] width){
		int depth;
		int[] offset = new int[3];
		depth= (int)(index&DepthMask);
		offset[0]=((int)((index>>OffsetShift1)&OffsetMask)+1)&(~(1<<depth));
		offset[1]=((int)((index>>OffsetShift2)&OffsetMask)+1)&(~(1<<depth));
		offset[2]=((int)((index>>OffsetShift3)&OffsetMask)+1)&(~(1<<depth));
		width[0]=(float)(1.0f/(1<<depth));
		for(int dim=0;dim<DIMENSION;dim++){center.coords[dim]=(float)(0.5f+offset[dim])*width[0];}
	}
	
	public final int maxDepth() {
		if( children == null){return 0;}
		else{
			int c=0,d=0;
			for(int i=0;i<Cube.CORNERS;i++){
				d=children[i].maxDepth();
				if( i == 0 || (d>c) ){c=d;}
			}
			return c+1;
		}
	}
	
	public final int nodes() {
		if( children == null ){return 1;}
		else{
			int c=0;
			for(int i=0;i<Cube.CORNERS;i++){c+=children[i].nodes();}
			return c+1;
		}
	}
	
	public final int leaves() {
		if(children == null){return 1;}
		else{
			int c=0;
			for(int i=0;i<Cube.CORNERS;i++){c+=children[i].leaves();}
			return c;
		}
	}

	public final int maxDepthLeaves(final int maxDepth) {
		if(depth()>maxDepth){return 0;}
		if( children == null ){return 1;}
		else{
			int c=0;
			for(int i=0;i<Cube.CORNERS;i++){c+=children[i].maxDepthLeaves(maxDepth);}
			return c;
		}
	}
	
	public final OctNode root() {
		OctNode temp=this;
		while(temp.parent != null ){temp=temp.parent;}
		return temp;
	}
	
	public final OctNode nextBranch(final OctNode current) {
		if(current.parent == null || current==this){return null;}
		int pIndex = 0;
		for ( int j = 0; j < current.parent.children.length; j++ ) {
			if ( current == current.parent.children[j]) {
				pIndex = j;
				break;
			}
		}
		// if(current-current.parent.children==Cube.CORNERS-1){return nextBranch(current.parent);}
		// else{return current+1;}
		if(pIndex==Cube.CORNERS-1){return nextBranch(current.parent);}
		else{return current.parent.children[pIndex+1];}
	}
	
	public final OctNode nextLeaf(final OctNode current) {
		if( current == null){
			OctNode temp=this;
			while(temp.children != null){temp= temp.children[0];}
			return temp;
		}
		if(current.children != null){return (OctNode)current.nextLeaf(null);}
		   OctNode temp=nextBranch(current);
		if( temp == null ){return null;}
		else{return temp.nextLeaf(null);}
	}
	
	public OctNode nextNode(final OctNode current){
		if( current == null ){return this;}
		else if(current.children != null ){return current.children[0];}
		else{return nextBranch(current);}
	}
	
	public void printRange() {
		Point3D center = new Point3D();
		float[] width = new float[1];
		centerAndWidth(center,width);
		for(int dim=0;dim<DIMENSION;dim++){
			System.err.println("%[" + (center.coords[dim]-width[0]/2) + (center.coords[dim]+width[0]/2) + "]");
			if(dim<DIMENSION-1){System.err.print("x");}
			else System.err.println();
		}
	}

	
	public void processNodeNodes(OctNode node, NodeAdjacencyFunction F, final int processCurrent){
		if(processCurrent != 0){F.Function(this,node);}
		if(children != null ){__processNodeNodes(node,F);}
	}
	
	public void processNodeFaces(OctNode node,NodeAdjacencyFunction F,final int fIndex, final int processCurrent){
		if(processCurrent != 0 ){F.Function(this,node);}
		if(children != null ){
			int[] c1 = new int[1];
			int[] c2 = new int[1];
			int[] c3 = new int[1];
			int[] c4 = new int[1];
			Cube.FaceCorners(fIndex,c1,c2,c3,c4);
			__processNodeFaces(node,F,c1,c2,c3,c4);
		}
	}
	
	public void processNodeEdges(OctNode node,NodeAdjacencyFunction F, final int eIndex, final int processCurrent){
		if(processCurrent != 0 ){F.Function(this,node);}
		if(children != null ){
			int[] c1 = new int[1];
			int[] c2 = new int[1];
			Cube.EdgeCorners(eIndex,c1,c2);
			__processNodeEdges(node,F,c1,c2);
		}
	}
	
	public void processNodeCorners(OctNode node,NodeAdjacencyFunction F, final int cIndex, final int processCurrent){
		if(processCurrent != 0 ){F.Function(this,node);}
		OctNode temp=this;
		while(temp.children != null){
			temp= temp.children[cIndex];
			F.Function(temp,node);
		}
	}
	
	public void __processNodeNodes(OctNode node,NodeAdjacencyFunction F){
		F.Function(children[0],node);
		F.Function(children[1],node);
		F.Function(children[2],node);
		F.Function(children[3],node);
		F.Function(children[4],node);
		F.Function(children[5],node);
		F.Function(children[6],node);
		F.Function(children[7],node);
		if(children[0].children != null ){children[0].__processNodeNodes(node,F);}
		if(children[1].children	!= null ){children[1].__processNodeNodes(node,F);}
		if(children[2].children != null ){children[2].__processNodeNodes(node,F);}
		if(children[3].children != null ){children[3].__processNodeNodes(node,F);}
		if(children[4].children != null ){children[4].__processNodeNodes(node,F);}
		if(children[5].children != null ){children[5].__processNodeNodes(node,F);}
		if(children[6].children != null ){children[6].__processNodeNodes(node,F);}
		if(children[7].children != null ){children[7].__processNodeNodes(node,F);}
	}
	
	public void __processNodeEdges(OctNode node,NodeAdjacencyFunction F,final int[] cIndex1, final int[] cIndex2){
		F.Function(children[cIndex1[0]],node);
		F.Function(children[cIndex2[0]],node);
		if(children[cIndex1[0]].children != null ){children[cIndex1[0]].__processNodeEdges(node,F,cIndex1,cIndex2);}
		if(children[cIndex2[0]].children != null ){children[cIndex2[0]].__processNodeEdges(node,F,cIndex1,cIndex2);}
	}
	
	public void __processNodeFaces(OctNode node,NodeAdjacencyFunction F, final int[] cIndex1, final int[] cIndex2, final int[] cIndex3, final int[] cIndex4){
		F.Function(children[cIndex1[0]],node);
		F.Function(children[cIndex2[0]],node);
		F.Function(children[cIndex3[0]],node);
		F.Function(children[cIndex4[0]],node);
		if(children[cIndex1[0]].children != null){children[cIndex1[0]].__processNodeFaces(node,F,cIndex1,cIndex2,cIndex3,cIndex4);}
		if(children[cIndex2[0]].children != null){children[cIndex2[0]].__processNodeFaces(node,F,cIndex1,cIndex2,cIndex3,cIndex4);}
		if(children[cIndex3[0]].children != null){children[cIndex3[0]].__processNodeFaces(node,F,cIndex1,cIndex2,cIndex3,cIndex4);}
		if(children[cIndex4[0]].children != null){children[cIndex4[0]].__processNodeFaces(node,F,cIndex1,cIndex2,cIndex3,cIndex4);}
	}
	
	public static void ProcessNodeAdjacentNodes(final int maxDepth,OctNode node1, final int width1,OctNode node2, final int width2,NodeAdjacencyFunction F, final int processCurrent){
		int[] c1 = new int[3];
		int[] c2 = new int[3];
		int w1,w2;
		node1.centerIndex(maxDepth+1,c1);
		node2.centerIndex(maxDepth+1,c2);
		w1=node1.width(maxDepth+1);
		w2=node2.width(maxDepth+1);

		ProcessNodeAdjacentNodes(c1[0]-c2[0],c1[1]-c2[1],c1[2]-c2[2],node1,(width1*w1)>>1,node2,(width2*w2)>>1,w2,F,processCurrent);
	}
	
	public static void ProcessNodeAdjacentNodes(final int dx,final int dy, final int dz,
			  OctNode node1, final int radius1,
			  OctNode node2, final int radius2, final int width2,
			  NodeAdjacencyFunction F, final int processCurrent){
		if(!Overlap(dx,dy,dz,radius1+radius2)){return;}
		if(processCurrent != 0){F.Function(node2,node1);}
		if(node2.children == null ){return;}
		__ProcessNodeAdjacentNodes(-dx,-dy,-dz,node1,radius1,node2,radius2,width2/2,F);
	}
	
	public static void ProcessTerminatingNodeAdjacentNodes(final int maxDepth,OctNode node1, final int width1,OctNode node2, final int width2,TerminatingNodeAdjacencyFunction F, final int processCurrent){
		int[] c1 = new int[3];
		int[] c2 = new int[3];
		int w1,w2;
		node1.centerIndex(maxDepth+1,c1);
		node2.centerIndex(maxDepth+1,c2);
		w1=node1.width(maxDepth+1);
		w2=node2.width(maxDepth+1);

		ProcessTerminatingNodeAdjacentNodes(c1[0]-c2[0],c1[1]-c2[1],c1[2]-c2[2],node1,(width1*w1)>>1,node2,(width2*w2)>>1,w2,F,processCurrent);
	}
	
	public static void ProcessTerminatingNodeAdjacentNodes(final int dx, final int dy, final int dz,
			 OctNode node1, final int radius1,
			 OctNode node2, final int radius2, final int width2,
			 TerminatingNodeAdjacencyFunction F, final int processCurrent)
	{
		if(!Overlap(dx,dy,dz,radius1+radius2)){return;}
		if(processCurrent != 0){F.Function(node2,node1);}
		if( node2.children == null ){return;}
		__ProcessTerminatingNodeAdjacentNodes(-dx,-dy,-dz,node1,radius1,node2,radius2,width2/2,F);
	}
	
	public static void ProcessPointAdjacentNodes(final int maxDepth,final int c1[],OctNode node2, final int width2,PointAdjacencyFunction F, final int processCurrent){
		int[] c2 = new int[3];
		int w2;
		node2.centerIndex(maxDepth+1,c2);
		w2=node2.width(maxDepth+1);
		ProcessPointAdjacentNodes(c1[0]-c2[0],c1[1]-c2[1],c1[2]-c2[2],node2,(width2*w2)>>1,w2,F,processCurrent);
	}
	
	public static void ProcessPointAdjacentNodes(final int dx, final int dy, final int dz,
			   OctNode node2, final int radius2, final int width2,
			   PointAdjacencyFunction F, final int processCurrent)
	{
		if(!Overlap(dx,dy,dz,radius2)){return;}
		if(processCurrent != 0){F.Function(node2);}
		if(node2.children == null){return;}
		__ProcessPointAdjacentNodes(-dx,-dy,-dz,node2,radius2,width2>>1,F);
	}
	
	public static void ProcessFixedDepthNodeAdjacentNodes(final int maxDepth,
			OctNode node1, final int width1,
			OctNode node2, final int width2,
			final int depth,NodeAdjacencyFunction F, final int processCurrent)
	{
		int[] c1 = new int[3];
		int[] c2 = new int[3];
		int w1,w2;
		node1.centerIndex(maxDepth+1,c1);
		node2.centerIndex(maxDepth+1,c2);
		w1=node1.width(maxDepth+1);
		w2=node2.width(maxDepth+1);
		
		ProcessFixedDepthNodeAdjacentNodes(c1[0]-c2[0],c1[1]-c2[1],c1[2]-c2[2],node1,(width1*w1)>>1,node2,(width2*w2)>>1,w2,depth,F,processCurrent);
	}
	
	
	public static void ProcessFixedDepthNodeAdjacentNodes(final int dx, final int dy, final int dz,
			OctNode node1, final int radius1,
			OctNode node2, final int radius2, final int width2,
			final int depth,NodeAdjacencyFunction F, final int processCurrent)
	{
		int d=node2.depth();
		if(d>depth){return;}
		if(!Overlap(dx,dy,dz,radius1+radius2)){return;}
		if(d==depth){if(processCurrent == 1){F.Function(node2,node1);}}
		else{
		if(node2.children == null ){return;}
		__ProcessFixedDepthNodeAdjacentNodes(-dx,-dy,-dz,node1,radius1,node2,radius2,width2/2,depth-1,F);
		}
	}
	
	public static void ProcessMaxDepthNodeAdjacentNodes(final int maxDepth,
			  OctNode node1,final int width1,
			  OctNode node2,final int width2,
			  final int depth,NodeAdjacencyFunction F, final int processCurrent)
	{
		int[] c1 = new int[3];
		int[] c2 = new int[3];
		int w1,w2;
		node1.centerIndex(maxDepth+1,c1);
		node2.centerIndex(maxDepth+1,c2);
		w1=node1.width(maxDepth+1);
		w2=node2.width(maxDepth+1);
		ProcessMaxDepthNodeAdjacentNodes(c1[0]-c2[0],c1[1]-c2[1],c1[2]-c2[2],node1,(width1*w1)>>1,node2,(width2*w2)>>1,w2,depth,F,processCurrent);
	}
	
	public static void ProcessMaxDepthNodeAdjacentNodes(final int dx, final int dy, final int dz,
			  OctNode node1, final int radius1,
			  OctNode node2, final int radius2, final int width2,
			  final int depth,NodeAdjacencyFunction F, final int processCurrent)
	{
		int d=node2.depth();
		if(d>depth){return;}
		if(!Overlap(dx,dy,dz,radius1+radius2)){return;}
		if(processCurrent != 0){F.Function(node2,node1);}
		if(d<depth && node2.children != null){__ProcessMaxDepthNodeAdjacentNodes(-dx,-dy,-dz,node1,radius1,node2,radius2,width2>>1,depth-1,F);}
	}
	
	public static void __ProcessNodeAdjacentNodes(final int dx, final int dy, final int dz,
			OctNode node1,final int radius1, OctNode node2, final int radius2,
			final int cWidth2, NodeAdjacencyFunction F) {
		int cWidth = cWidth2 >> 1;
		int radius = radius2 >> 1;
		int o = ChildOverlap(dx, dy, dz, radius1 + radius, cWidth);
		if (o != 0) {
			int dx1 = dx - cWidth;
			int dx2 = dx + cWidth;
			int dy1 = dy - cWidth;
			int dy2 = dy + cWidth;
			int dz1 = dz - cWidth;
			int dz2 = dz + cWidth;
			if ((o & 1) != 0) {
				F.Function(node2.children[0], node1);
				if (node2.children[0].children != null) {
					__ProcessNodeAdjacentNodes(dx1, dy1, dz1, node1, radius1, node2.children[0], radius, cWidth, F);
				}
			}
			if ((o & 2) != 0) {
				F.Function(node2.children[1], node1);
				if (node2.children[1].children != null) {
					__ProcessNodeAdjacentNodes(dx2, dy1, dz1, node1, radius1, node2.children[1], radius, cWidth, F);
				}
			}
			if ((o & 4) != 0) {
				F.Function(node2.children[2], node1);
				if (node2.children[2].children != null) {
					__ProcessNodeAdjacentNodes(dx1, dy2, dz1, node1, radius1, node2.children[2], radius, cWidth, F);
				}
			}
			if ((o & 8) != 0) {
				F.Function(node2.children[3], node1);
				if (node2.children[3].children != null) {
					__ProcessNodeAdjacentNodes(dx2, dy2, dz1, node1, radius1, node2.children[3], radius, cWidth, F);
				}
			}
			if ((o & 16) != 0) {
				F.Function(node2.children[4], node1);
				if (node2.children[4].children != null) {
					__ProcessNodeAdjacentNodes(dx1, dy1, dz2, node1, radius1, node2.children[4], radius, cWidth, F);
				}
			}
			if ((o & 32) != 0) {
				F.Function(node2.children[5], node1);
				if (node2.children[5].children != null) {
					__ProcessNodeAdjacentNodes(dx2, dy1, dz2, node1, radius1, node2.children[5], radius, cWidth, F);
				}
			}
			if ((o & 64) != 0) {
				F.Function(node2.children[6], node1);
				if (node2.children[6].children != null) {
					__ProcessNodeAdjacentNodes(dx1, dy2, dz2, node1, radius1, node2.children[6], radius, cWidth, F);
				}
			}
			if ((o & 128) != 0) {
				F.Function(node2.children[7], node1);
				if (node2.children[7].children != null) {
					__ProcessNodeAdjacentNodes(dx2, dy2, dz2, node1, radius1, node2.children[7], radius, cWidth, F);
				}
			}
		}
	}
	
	
	public static void __ProcessTerminatingNodeAdjacentNodes(final int dx, final int dy, final int dz,
			   OctNode node1, final int radius1,
			   OctNode node2, final int radius2, final int cWidth2,
			   TerminatingNodeAdjacencyFunction F)
	{
		int cWidth=cWidth2>>1;
		int radius=radius2>>1;
		int o=ChildOverlap(dx,dy,dz,radius1+radius,cWidth);
		if(o != 0){
		int dx1=dx-cWidth;
		int dx2=dx+cWidth;
		int dy1=dy-cWidth;
		int dy2=dy+cWidth;
		int dz1=dz-cWidth;
		int dz2=dz+cWidth;
		if((o&  1) != 0){if(1 == F.Function(node2.children[0],node1) && node2.children[0].children != null){__ProcessTerminatingNodeAdjacentNodes(dx1,dy1,dz1,node1,radius1,node2.children[0],radius,cWidth,F);}}
		if((o&  2) != 0){if(1 == F.Function(node2.children[1],node1) && node2.children[1].children != null){__ProcessTerminatingNodeAdjacentNodes(dx2,dy1,dz1,node1,radius1,node2.children[1],radius,cWidth,F);}}
		if((o&  4) != 0){if(1 == F.Function(node2.children[2],node1) && node2.children[2].children != null){__ProcessTerminatingNodeAdjacentNodes(dx1,dy2,dz1,node1,radius1,node2.children[2],radius,cWidth,F);}}
		if((o&  8) != 0){if(1 == F.Function(node2.children[3],node1) && node2.children[3].children != null){__ProcessTerminatingNodeAdjacentNodes(dx2,dy2,dz1,node1,radius1,node2.children[3],radius,cWidth,F);}}
		if((o& 16) != 0){if(1 == F.Function(node2.children[4],node1) && node2.children[4].children != null){__ProcessTerminatingNodeAdjacentNodes(dx1,dy1,dz2,node1,radius1,node2.children[4],radius,cWidth,F);}}
		if((o& 32) != 0){if(1 == F.Function(node2.children[5],node1) && node2.children[5].children != null){__ProcessTerminatingNodeAdjacentNodes(dx2,dy1,dz2,node1,radius1,node2.children[5],radius,cWidth,F);}}
		if((o& 64) != 0){if(1 == F.Function(node2.children[6],node1) && node2.children[6].children != null){__ProcessTerminatingNodeAdjacentNodes(dx1,dy2,dz2,node1,radius1,node2.children[6],radius,cWidth,F);}}
		if((o&128) != 0){if(1 == F.Function(node2.children[7],node1) && node2.children[7].children != null){__ProcessTerminatingNodeAdjacentNodes(dx2,dy2,dz2,node1,radius1,node2.children[7],radius,cWidth,F);}}
		}
	}
	
	public static void __ProcessPointAdjacentNodes(final int dx, final int dy, final int dz,
			 OctNode node2, final int radius2, final int cWidth2,
			 PointAdjacencyFunction F)
	{
		int cWidth=cWidth2>>1;
		int radius=radius2>>1;
		int o=ChildOverlap(dx,dy,dz,radius,cWidth);
		if(o != 0){
		int dx1=dx-cWidth;
		int dx2=dx+cWidth;
		int dy1=dy-cWidth;
		int dy2=dy+cWidth;
		int dz1=dz-cWidth;
		int dz2=dz+cWidth;
		if((o&  1) != 0){F.Function(node2.children[0]);if(node2.children[0].children != null){__ProcessPointAdjacentNodes(dx1,dy1,dz1,node2.children[0],radius,cWidth,F);}}
		if((o&  2) != 0){F.Function(node2.children[1]);if(node2.children[1].children != null){__ProcessPointAdjacentNodes(dx2,dy1,dz1,node2.children[1],radius,cWidth,F);}}
		if((o&  4) != 0){F.Function(node2.children[2]);if(node2.children[2].children != null){__ProcessPointAdjacentNodes(dx1,dy2,dz1,node2.children[2],radius,cWidth,F);}}
		if((o&  8) != 0){F.Function(node2.children[3]);if(node2.children[3].children != null){__ProcessPointAdjacentNodes(dx2,dy2,dz1,node2.children[3],radius,cWidth,F);}}
		if((o& 16) != 0){F.Function(node2.children[4]);if(node2.children[4].children != null){__ProcessPointAdjacentNodes(dx1,dy1,dz2,node2.children[4],radius,cWidth,F);}}
		if((o& 32) != 0){F.Function(node2.children[5]);if(node2.children[5].children != null){__ProcessPointAdjacentNodes(dx2,dy1,dz2,node2.children[5],radius,cWidth,F);}}
		if((o& 64) != 0){F.Function(node2.children[6]);if(node2.children[6].children != null){__ProcessPointAdjacentNodes(dx1,dy2,dz2,node2.children[6],radius,cWidth,F);}}
		if((o&128) != 0){F.Function(node2.children[7]);if(node2.children[7].children != null){__ProcessPointAdjacentNodes(dx2,dy2,dz2,node2.children[7],radius,cWidth,F);}}
		}
	}
	
	public static void __ProcessFixedDepthNodeAdjacentNodes(final int dx, final int dy, final int dz,
			  OctNode node1, final int radius1,
			  OctNode node2, final int radius2, final int cWidth2,
			  final int depth,NodeAdjacencyFunction F)
	{
		int cWidth=cWidth2>>1;
		int radius=radius2>>1;
		int o=ChildOverlap(dx,dy,dz,radius1+radius,cWidth);
		if(o != 0){
			int dx1=dx-cWidth;
			int dx2=dx+cWidth;
			int dy1=dy-cWidth;
			int dy2=dy+cWidth;
			int dz1=dz-cWidth;
			int dz2=dz+cWidth;
			if(node2.depth()==depth){
				if((o&  1) != 0){F.Function(node2.children[0],node1);}
				if((o&  2) != 0){F.Function(node2.children[1],node1);}
				if((o&  4) != 0){F.Function(node2.children[2],node1);}
				if((o&  8) != 0){F.Function(node2.children[3],node1);}
				if((o& 16) != 0){F.Function(node2.children[4],node1);}
				if((o& 32) != 0){F.Function(node2.children[5],node1);}
				if((o& 64) != 0){F.Function(node2.children[6],node1);}
				if((o&128) != 0){F.Function(node2.children[7],node1);}
			} else{
				if((o&  1) != 0){if(node2.children[0].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx1,dy1,dz1,node1,radius1,node2.children[0],radius,cWidth,depth,F);}}
				if((o&  2) != 0){if(node2.children[1].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx2,dy1,dz1,node1,radius1,node2.children[1],radius,cWidth,depth,F);}}
				if((o&  4) != 0){if(node2.children[2].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx1,dy2,dz1,node1,radius1,node2.children[2],radius,cWidth,depth,F);}}
				if((o&  8) != 0){if(node2.children[3].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx2,dy2,dz1,node1,radius1,node2.children[3],radius,cWidth,depth,F);}}
				if((o& 16) != 0){if(node2.children[4].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx1,dy1,dz2,node1,radius1,node2.children[4],radius,cWidth,depth,F);}}
				if((o& 32) != 0){if(node2.children[5].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx2,dy1,dz2,node1,radius1,node2.children[5],radius,cWidth,depth,F);}}
				if((o& 64) != 0){if(node2.children[6].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx1,dy2,dz2,node1,radius1,node2.children[6],radius,cWidth,depth,F);}}
				if((o&128) != 0){if(node2.children[7].children != null){__ProcessFixedDepthNodeAdjacentNodes(dx2,dy2,dz2,node1,radius1,node2.children[7],radius,cWidth,depth,F);}}
			}
		}
	}
	
	public static void __ProcessMaxDepthNodeAdjacentNodes(final int dx, final int dy, final int dz,
			OctNode node1, final int radius1,
			OctNode node2, final int radius2, final int cWidth2,
			final int depth,NodeAdjacencyFunction F)
	{
		int cWidth=cWidth2>>1;
		int radius=radius2>>1;  
		int o=ChildOverlap(dx,dy,dz,radius1+radius,cWidth);
		if(o != 0){
			int dx1=dx-cWidth;
			int dx2=dx+cWidth;
			int dy1=dy-cWidth;
			int dy2=dy+cWidth;
			int dz1=dz-cWidth;
			int dz2=dz+cWidth;
			if(node2.depth()<=depth){
				if((o&  1)!= 0){F.Function(node2.children[0],node1);}
				if((o&  2)!= 0){F.Function(node2.children[1],node1);}
				if((o&  4)!= 0){F.Function(node2.children[2],node1);}
				if((o&  8)!= 0){F.Function(node2.children[3],node1);}
				if((o& 16)!= 0){F.Function(node2.children[4],node1);}
				if((o& 32)!= 0){F.Function(node2.children[5],node1);}
				if((o& 64)!= 0){F.Function(node2.children[6],node1);}
				if((o&128)!= 0){F.Function(node2.children[7],node1);}
			}
			if(node2.depth()<depth){
				if((o&  1)!= 0){if(node2.children[0].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx1,dy1,dz1,node1,radius1,node2.children[0],radius,cWidth,depth,F);}}
				if((o&  2)!= 0){if(node2.children[1].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx2,dy1,dz1,node1,radius1,node2.children[1],radius,cWidth,depth,F);}}
				if((o&  4)!= 0){if(node2.children[2].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx1,dy2,dz1,node1,radius1,node2.children[2],radius,cWidth,depth,F);}}
				if((o&  8)!= 0){if(node2.children[3].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx2,dy2,dz1,node1,radius1,node2.children[3],radius,cWidth,depth,F);}}
				if((o& 16)!= 0){if(node2.children[4].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx1,dy1,dz2,node1,radius1,node2.children[4],radius,cWidth,depth,F);}}
				if((o& 32)!= 0){if(node2.children[5].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx2,dy1,dz2,node1,radius1,node2.children[5],radius,cWidth,depth,F);}}
				if((o& 64)!= 0){if(node2.children[6].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx1,dy2,dz2,node1,radius1,node2.children[6],radius,cWidth,depth,F);}}
				if((o&128)!= 0){if(node2.children[7].children!= null){__ProcessMaxDepthNodeAdjacentNodes(dx2,dy2,dz2,node1,radius1,node2.children[7],radius,cWidth,depth,F);}}
			}
		}
	}
		
	public static final int ChildOverlap(final int dx, final int dy, final int dz, final int d, final int cRadius2)
	{
		int w1=d-cRadius2;
		int w2=d+cRadius2;
		int overlap=0;

		int test=0,test1=0;
		if(dx<w2 && dx>-w1){test =1;}
		if(dx<w1 && dx>-w2){test|=2;}

		if(test == 0){return 0;}
		if(dz<w2 && dz>-w1){test1 =test;}
		if(dz<w1 && dz>-w2){test1|=test<<4;}

		if(test1 == 0){return 0;}
		if(dy<w2 && dy>-w1){overlap =test1;}
		if(dy<w1 && dy>-w2){overlap|=test1<<2;}
		return overlap;
	}
	
	public OctNode getNearestLeaf(final Point3D p){
		Point3D center = new Point3D();
		float[] width = new float[1];
		OctNode temp;
		int cIndex;
		if(children == null){return this;}
		centerAndWidth(center,width);
		temp=this;
		while(temp.children != null){
			cIndex=CornerIndex(center,p);
			temp=temp.children[cIndex];
			width[0]/=2;
			if( ( cIndex&1 ) != 0){center.coords[0]+=width[0]/2;}
			else		{center.coords[0]-=width[0]/2;}
			if( ( cIndex&2 ) != 0){center.coords[1]+=width[0]/2;}
			else		{center.coords[1]-=width[0]/2;}
			if( ( cIndex&4 ) != 0){center.coords[2]+=width[0]/2;}
			else		{center.coords[2]-=width[0]/2;}
		}
		return temp;
	}
	
	/*
	
	public OctNode getNearestLeaf(Point3D p) {
		int nearest;
		float temp,dist2;
		if( children == null){return this;}
		for(int i=0;i<Cube.CORNERS;i++){
			temp=geo.SquareDistance(children[i].center,p);
			if( i == 0 || temp<dist2){
				dist2=temp;
				nearest=i;
			}
		}
		return children[nearest].getNearestLeaf(p);
	}
	*/
	
	public static int CommonEdge(final OctNode node1, final int eIndex1, final OctNode node2, final int eIndex2){
		int[] o1 = new int[1];
		int[] o2 = new int[1];
		int[] i1 = new int[1];
		int[] i2 = new int[1];
		int[] j1 = new int[1];
		int[] j2 = new int[1];
	
		Cube.FactorEdgeIndex(eIndex1,o1,i1,j1);
		Cube.FactorEdgeIndex(eIndex2,o2,i2,j2);
		if(o1[0]!=o2[0]){return 0;}
	
		int[] dir = new int[2];
		int[] idx1 = new int[2];
		int[] idx2 = new int[2];
		switch(o1[0]){
			case 0:	dir[0]=1;	dir[1]=2;	break;
			case 1:	dir[0]=0;	dir[1]=2;	break;
			case 2:	dir[0]=0;	dir[1]=1;	break;
		};
		int[] d1 = new int[1];
		int[] d2 = new int[1];
		int[] off1 = new int[3];
		int[] off2 = new int[3];
		node1.depthAndOffset(d1,off1);
		node2.depthAndOffset(d2,off2);
		idx1[0]=off1[dir[0]]+(1<<d1[0])+i1[0];
		idx1[1]=off1[dir[1]]+(1<<d1[0])+j1[0];
		idx2[0]=off2[dir[0]]+(1<<d2[0])+i2[0];
		idx2[1]=off2[dir[1]]+(1<<d2[0])+j2[0];
		if(d1[0]>d2[0]){
			idx2[0]<<=(d1[0]-d2[0]);
			idx2[1]<<=(d1[0]-d2[0]);
		}
		else{
			idx1[0]<<=(d2[0]-d1[0]);
			idx1[1]<<=(d2[0]-d1[0]);
		}
		if(idx1[0]==idx2[0] && idx1[1]==idx2[1]){return 1;}
		else									{return 0;}
	}
	
	public static int CornerIndex(final Point3D center, final Point3D p){
		int cIndex=0;
		if(p.coords[0]>center.coords[0]){cIndex|=1;}
		if(p.coords[1]>center.coords[1]){cIndex|=2;}
		if(p.coords[2]>center.coords[2]){cIndex|=4;}
		return cIndex;
	}
	
	// operator =
	public OctNode set(final OctNode node){
		int i;
		if(children != null ){ children = null;}
		
		// depth=node.depth;
		d[0] = (short)node.depth();
		// for(i=0;i<DIMENSION;i++){this.off[i] = node.offset[i];}
		for(i=0;i<DIMENSION;i++){this.off[i] = (short)(node.off[i]);}
		if(node.children != null){
			initChildren();
			for(i=0;i<Cube.CORNERS;i++){children[i] = node.children[i];}
		}
		return this;
	}
	
	
	public static int CompareForwardDepths(final Object v1, final Object v2){
		return ((OctNode)v1).depth()-((OctNode)v2).depth();
	}
	
	public static int CompareForwardPointerDepths(final Object v1, final Object v2){
	     OctNode n1,n2;
		n1=((OctNode)v1);
		n2=((OctNode)v2);
		if(n1.d[0]!=n2.d[0]){return new Short(n1.d[0]).intValue()-new Short(n2.d[0]).intValue();}
		while(n1.parent != n2.parent){
			n1=n1.parent;
			n2=n2.parent;
		}
		if(n1.off[0]!=n2.off[0]){return new Short(n1.off[0]).intValue()- new Short(n2.off[0]).intValue();}
		if(n1.off[1]!=n2.off[1]){return new Short(n1.off[1]).intValue()- new Short(n2.off[1]).intValue();}
		return new Short(n1.off[2]).intValue()- new Short(n2.off[2]).intValue();
	}
	
	public int compare(final Object v1, final Object v2){
	     OctNode n1,n2;
		n1=((OctNode)v1);
		n2=((OctNode)v2);
		if(n1.d[0]!=n2.d[0]){return (int)(n1.d[0])- (int)(n2.d[0]);}
		while(n1.parent != n2.parent){
			n1=n1.parent;
			n2=n2.parent;
		}
		if(n1.off[0]!=n2.off[0]){return (int)(n1.off[0])- (int)(n2.off[0]);}
		if(n1.off[1]!=n2.off[1]){return (int)(n1.off[1])- (int)(n2.off[1]);}
		// System.err.println("(int)(n1.off[2])-(int)(n2.off[2]) = " + ((int)(n1.off[2])-(int)(n2.off[2])));
		// Octree.pause();
		return (int)(n1.off[2])- (int)(n2.off[2]);
	}
	
	public static int CompareBackwardDepths(final Object v1, final Object v2){
		return ((OctNode)v2).depth()-(( OctNode)v1).depth();
	}
	
	public static int CompareBackwardPointerDepths(final Object v1, final Object v2){
		return (( OctNode)v2).depth()-((OctNode)v1).depth();
	}
	
	public static final boolean Overlap2(final int depth1, final int offSet1[], final float multiplier1, final int depth2, 
			                             final int offSet2[], final float multiplier2){
		int d=depth2-depth1;
		float w=multiplier2+multiplier1*(1<<d);
		float w2=(float)((1<<(d-1))-0.5f);
		if(
			(float)Math.abs((float)(offSet2[0]-(offSet1[0]<<d))-w2)>=w ||
			(float)Math.abs((float)(offSet2[1]-(offSet1[1]<<d))-w2)>=w ||
			(float)Math.abs((float)(offSet2[2]-(offSet1[2]<<d))-w2)>=w
			){return false;}
		return true;
	}
	
	public static final boolean Overlap(final int c1, final int c2, final int c3, final int dWidth){
		if(c1>=dWidth || c1<=-dWidth || c2>=dWidth || c2<=-dWidth || c3>=dWidth || c3<=-dWidth){return false;}
		else{return true;}
	}
	
	
	public OctNode faceNeighbor(final int faceIndex) {return __faceNeighbor(faceIndex>>1,faceIndex&1);}
	
	public OctNode faceNeighbor(final int faceIndex, final int forceChildren) {
		// System.err.println("faceIndex = " + faceIndex);
		return __faceNeighbor(faceIndex>>1,faceIndex&1, forceChildren);
	}
	
	public OctNode __faceNeighbor(final int dir, final int off, final int forceChildren){
		if(parent == null ){return null;}
		// int pIndex=(int)(this-parent.children);
		int pIndex = 0;
		for ( int i = 0; i < parent.children.length; i++ ) {
			if ( this == parent.children[i]) {
				pIndex = i;
				break;
			}
		}
		// System.err.println("pIndex = " + pIndex + " dir = " + dir + " off = " + off);
		pIndex^=(1<<dir);
		// Octree.pause();
		if((pIndex & (1<<dir))==(off<<dir)){return parent.children[pIndex];}
//		if(!(((pIndex>>dir)^off)&1)){return &parent->children[pIndex];}
		else{
			OctNode temp=parent.__faceNeighbor(dir,off,forceChildren);
			if(temp == null){return null;}
			if(temp.children == null ){
				if(forceChildren != 0 ){temp.initChildren();}
				else{return temp;}
			}
			
			return temp.children[pIndex];
		}
	}
	
	
	public final OctNode __faceNeighbor(final int dir, final int off) {
		if(parent == null){return null;}
		// int pIndex=(int)(this-parent.children);
		int pIndex = 0;
		for ( int i = 0; i < parent.children.length; i++ ) {
			if ( this == parent.children[i]) {
				pIndex = i;
				break;
			}
		}
		pIndex^=(1<<dir);
		if((pIndex & (1<<dir))==(off<<dir)){return parent.children[pIndex];}
//		if(!(((pIndex>>dir)^off)&1)){return &parent->children[pIndex];}
		else{
			final OctNode temp=parent.__faceNeighbor(dir,off);
			if(temp == null || temp.children == null ){return temp;}
			else{return temp.children[pIndex];}
		}
	}
	
	
	public OctNode edgeNeighbor(final int edgeIndex, final int forceChildren){
		int[] idx = new int[2];
		int[] i = new int[2];
		int[] i0 = new int[1];
		int[] i1 = new int[1];
		int[] o = new int[1];
		Cube.FactorEdgeIndex(edgeIndex,o,i0,i1);
		i[0] = i0[0]; i[1] = i1[0];
		switch(o[0]){
			case 0:	idx[0]=1;	idx[1]=2;	break;
			case 1:	idx[0]=0;	idx[1]=2;	break;
			case 2:	idx[0]=0;	idx[1]=1;	break;
		};
		return __edgeNeighbor(o,i,idx,forceChildren);
	}
	
	public final OctNode edgeNeighbor(final int edgeIndex) {
		int[] idx = new int[2];
		int[] o = new int[1];
		int[] i = new int[2];
		int[] i0 = new int[1];
		int[] i1 = new int[1];
		Cube.FactorEdgeIndex(edgeIndex,o,i0,i1);
		i[0] = i0[0]; i[1] = i1[0];
		switch(o[0]){
			case 0:	idx[0]=1;	idx[1]=2;	break;
			case 1:	idx[0]=0;	idx[1]=2;	break;
			case 2:	idx[0]=0;	idx[1]=1;	break;
		};
		return __edgeNeighbor(o,i,idx);
	}
	
	public final OctNode __edgeNeighbor(final int[] o, final int i[], final int idx[]){
		if( parent == null){return null;}
		// int pIndex=(int)(this-parent.children);
		int pIndex = 0;
		for ( int j = 0; j < parent.children.length; j++ ) {
			if ( this == parent.children[j]) {
				pIndex = j;
				break;
			}
		}
		int aIndex;
		int[] x = new int[DIMENSION];
        int[] x0 = new int[1];
        int[] x1 = new int[1];
        int[] x2 = new int[1];
		
		Cube.FactorCornerIndex(pIndex,x0,x1,x2);
		x[0] = x0[0]; x[1] = x1[0]; x[2] = x2[0];
		aIndex=(~((i[0] ^ x[idx[0]]) | ((i[1] ^ x[idx[1]])<<1))) & 3;
		pIndex^=(7 ^ (1<<o[0]));
		if(aIndex==1)	{	// I can get the neighbor from the parent's face adjacent neighbor
			final OctNode temp=parent.__faceNeighbor(idx[0],i[0]);
			if(temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==2)	{	// I can get the neighbor from the parent's face adjacent neighbor
			final OctNode temp=parent.__faceNeighbor(idx[1],i[1]);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==0)	{	// I can get the neighbor from the parent
			return parent.children[pIndex];
		}
		else if(aIndex==3)	{	// I can get the neighbor from the parent's edge adjacent neighbor
			final OctNode temp=parent.__edgeNeighbor(o,i,idx);
			if( temp == null || temp.children == null ){return temp;}
			else{return temp.children[pIndex];}
		}
		else{return null;}
	}
	
	public OctNode __edgeNeighbor(final int[] o, final int i[], final int idx[], final int forceChildren){
		if( parent == null ){return null;}
		// int pIndex=(int)(this-parent.children);
		int pIndex = 0;
		for ( int j = 0; j < parent.children.length; j++ ) {
			if ( this == parent.children[j]) {
				pIndex = j;
				break;
			}
		}
		int aIndex;
		int[] x = new int[DIMENSION];
        int[] x0 = new int[1];
        int[] x1 = new int[1];
        int[] x2 = new int[1];

		Cube.FactorCornerIndex(pIndex,x0,x1,x2);
		x[0] = x0[0]; x[1] = x1[0]; x[2] = x2[0];
		aIndex=(~((i[0] ^ x[idx[0]]) | ((i[1] ^ x[idx[1]])<<1))) & 3;
		pIndex^=(7 ^ (1<<o[0]));
		if(aIndex==1)	{	// I can get the neighbor from the parent's face adjacent neighbor
			OctNode temp=parent.__faceNeighbor(idx[0],i[0],0);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==2)	{	// I can get the neighbor from the parent's face adjacent neighbor
			OctNode temp=parent.__faceNeighbor(idx[1],i[1],0);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==0)	{	// I can get the neighbor from the parent
			return parent.children[pIndex];
		}
		else if(aIndex==3)	{	// I can get the neighbor from the parent's edge adjacent neighbor
			OctNode temp=parent.__edgeNeighbor(o,i,idx,forceChildren);
			if(temp == null ){return null;}
			if(temp.children == null ){
				if(forceChildren != 0 ){temp.initChildren();}
				else{return temp;}
			}
			return temp.children[pIndex];
		}
		else{return null;}
	}
	
	public final OctNode cornerNeighbor(final int cornerIndex) {
		int pIndex,aIndex=0;
		if( parent == null ){return null;}

		// pIndex=(int)(this-parent.children);
		pIndex = 0;
		for ( int i = 0; i < parent.children.length; i++ ) {
			if ( this == parent.children[i]) {
				pIndex = i;
				break;
			}
		}
		aIndex=(cornerIndex ^ pIndex);	// The disagreement bits
		pIndex=(~pIndex)&7;				// The antipodal point
		if(aIndex==7){					// Agree on no bits
			return parent.children[pIndex];
		}
		else if(aIndex==0){				// Agree on all bits
			final OctNode temp=((OctNode)parent).cornerNeighbor(cornerIndex);
			if( temp == null || temp.children == null ){return temp;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==6){				// Agree on face 0
			final OctNode temp=((OctNode)parent).__faceNeighbor(0,cornerIndex & 1);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==5){				// Agree on face 1
			final OctNode temp =((OctNode)parent).__faceNeighbor(1,(cornerIndex & 2)>>1);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==3){				// Agree on face 2
			final OctNode temp=((OctNode)parent).__faceNeighbor(2,(cornerIndex & 4)>>2);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==4){				// Agree on edge 2
			 final OctNode temp=((OctNode)parent).edgeNeighbor(8 | (cornerIndex & 1) | (cornerIndex & 2) );
			if(temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==2){				// Agree on edge 1
			final OctNode temp=((OctNode)parent).edgeNeighbor(4 | (cornerIndex & 1) | ((cornerIndex & 4)>>1) );
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==1){				// Agree on edge 0
			final OctNode temp=((OctNode)parent).edgeNeighbor(((cornerIndex & 2) | (cornerIndex & 4))>>1 );
			if(temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else{return null;}
	}
	
	public OctNode cornerNeighbor(final int cornerIndex, final int forceChildren){
		int pIndex,aIndex=0;
		if( parent == null ){return null;}

		// pIndex=(int)(this-parent.children);
		pIndex = 0;
		for ( int i = 0; i < parent.children.length; i++ ) {
			if ( this == parent.children[i]) {
				pIndex = i;
				break;
			}
		}
		aIndex=(cornerIndex ^ pIndex);	// The disagreement bits
		pIndex=(~pIndex)&7;				// The antipodal point
		if(aIndex==7){					// Agree on no bits
			return parent.children[pIndex];
		}
		else if(aIndex==0){				// Agree on all bits
			OctNode temp=((OctNode)parent).cornerNeighbor(cornerIndex,forceChildren);
			if(temp == null ){return null;}
			if(temp.children == null ){
				if(forceChildren != 0){temp.initChildren();}
				else{return temp;}
			}
			return temp.children[pIndex];
		}
		else if(aIndex==6){				// Agree on face 0
			OctNode temp=((OctNode)parent).__faceNeighbor(0,cornerIndex & 1,0);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==5){				// Agree on face 1
			OctNode temp=((OctNode)parent).__faceNeighbor(1,(cornerIndex & 2)>>1,0);
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==3){				// Agree on face 2
			OctNode temp=((OctNode)parent).__faceNeighbor(2,(cornerIndex & 4)>>2,0);
			if(temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==4){				// Agree on edge 2
			OctNode temp=((OctNode)parent).edgeNeighbor(8 | (cornerIndex & 1) | (cornerIndex & 2) );
			if( temp == null ||  temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==2){				// Agree on edge 1
			OctNode temp=((OctNode)parent).edgeNeighbor(4 | (cornerIndex & 1) | ((cornerIndex & 4)>>1) );
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else if(aIndex==1){				// Agree on edge 0
			OctNode temp=((OctNode)parent).edgeNeighbor(((cornerIndex & 2) | (cornerIndex & 4))>>1 );
			if( temp == null || temp.children == null ){return null;}
			else{return temp.children[pIndex];}
		}
		else{return null;}
	}
	
	
	
	
	public final int width(final int maxDepth) {
		int d=depth();
		return 1<<(maxDepth-d); 
	}
	
	public final void centerIndex(final int maxDepth,int index[]) {
		int[] d = new int[1];
		int[] o = new int[3];
		depthAndOffset(d,o);
		for(int i=0;i<DIMENSION;i++){index[i]=BinaryNode.CornerIndex(maxDepth,d[0]+1,o[i]<<1,1);}
	}
	
	
	
	
}
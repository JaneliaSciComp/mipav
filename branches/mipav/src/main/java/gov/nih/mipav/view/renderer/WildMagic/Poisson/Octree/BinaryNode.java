package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;


public class BinaryNode {
	
	public static final int CenterCount(int depth){return 1<<depth;}
	public static final int CumulativeCenterCount(int maxDepth){return (1<<(maxDepth+1))-1;}
	public static final int Index(int depth, int offSet){return (1<<depth)+offSet-1;}
	public static final int CornerIndex(int maxDepth,int depth,int offSet,int forwardCorner)
	  {return (offSet+forwardCorner)<<(maxDepth-depth);}
	
	public static final float CornerIndexPosition(int index,int maxDepth)
	  {return (float)(index)/(1<<maxDepth);}
	public static final float Width(int depth)
	  {return (float)(1.0f/(1<<depth));}
	public static final void CenterAndWidth(int depth,int offset,float[] center,float[] width)
	  {
	    width[0]=(float)(1.0f/(1<<depth));
	    center[0]=(float)((0.5f+offset)*width[0]);
	  }
	public static final void CenterAndWidth(int idx,float[] center,float[] width)
	  {
	    int[] depth = new int[1];
	    int[] offset = new int[1];
	    DepthAndOffset(idx,depth,offset);
	    CenterAndWidth(depth[0],offset[0],center,width);
	  }
	
	
	public static final void DepthAndOffset(int idx, int[] depth,int[] offset)
	  {
	    int i=idx+1;
	    depth[0]=-1;
	    while(i != 0){
	      i>>=1;
	      depth[0]++;
	    }
	    offset[0]=(idx+1)-(1<<depth[0]);
	  }
}
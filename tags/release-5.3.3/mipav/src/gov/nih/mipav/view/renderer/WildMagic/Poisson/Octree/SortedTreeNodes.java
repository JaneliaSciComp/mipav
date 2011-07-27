package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class SortedTreeNodes {
	public OctNode[] treeNodes;
	public OctNode[] treeNodesTemp;
	public int[] nodeCount;
	public int maxDepth;
	
	public SortedTreeNodes(){
		nodeCount=null;
		treeNodes=null;
		maxDepth=0;
	}
	
	public void dispose(){
		nodeCount=null;
		treeNodes=null;
	}
	
	public void set(OctNode root, final int setIndex){
		if(nodeCount != null ) {
			nodeCount = null;
		}
		if( treeNodes != null ){
			treeNodes = null;
		}
		
		maxDepth=root.maxDepth()+1;
		nodeCount=new int[maxDepth+1];
		treeNodes=new OctNode[root.nodes()];

		OctNode temp=root.nextNode(null);
		int i,cnt=0;
		while(temp != null ){
			treeNodes[cnt++]= temp;
			temp=root.nextNode(temp);
		}
		/*
		treeNodesTemp = new OctNode[cnt];
		System.arraycopy(treeNodes, 0, treeNodesTemp, 0, cnt);
	     
		for ( i = 0; i < cnt; i++ ) {
			treeNodesTemp[i] = treeNodes[i];
		}
	    
		treeNodes = new OctNode[cnt];
		System.arraycopy(treeNodesTemp, 0, treeNodes, 0, cnt);
        */		
		// qsort(treeNodes,cnt,sizeof(const OctNode*),OctNode::CompareForwardPointerDepths);
		OctNode t = new OctNode();
		QuickSort qsort = new QuickSort(t);
		qsort.sort(treeNodes);
		// SortUtil.qsort(treeNodes, t );
		for(i=0;i<=maxDepth;i++){
			nodeCount[i]=0;
		}
		for(i=0;i<cnt;i++){
			if(setIndex == 1){
				treeNodes[i].nodeData.nodeIndex=i;
			}
			nodeCount[treeNodes[i].depth()+1]++;
		}
		for(i=1;i<=maxDepth;i++){
			nodeCount[i]+=nodeCount[i-1];
		}
	}
}
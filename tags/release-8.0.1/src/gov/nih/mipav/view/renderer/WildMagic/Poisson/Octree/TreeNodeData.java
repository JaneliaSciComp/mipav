package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class TreeNodeData {
	
	public static int UseIndex = 1;
	
	public int mcIndex;;
	
	public int nodeIndex;
	
	public float centerWeightContribution;
	
	public float value;
	
	public TreeNodeData(){
		if(UseIndex == 1 ){
			nodeIndex=-1;
			centerWeightContribution=0;
		}
		else{mcIndex=0;}
		value=0;
	}
}
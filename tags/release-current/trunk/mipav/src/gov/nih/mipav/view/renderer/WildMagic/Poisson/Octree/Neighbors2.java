package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class Neighbors2 {
	
	public OctNode[][][] neighbors = new OctNode[3][3][3];
	
	public Neighbors2(){
		clear();
	}
	
	public void dispose(){
		if(neighbors != null ) { 
			neighbors = null;
		}
	}
	
	public void clear(){
		for(int i=0;i<3;i++) {
			for(int j=0;j<3;j++) {
				for(int k=0;k<3;k++) {
					neighbors[i][j][k]=null;
				}
			}
		}
	}
	
}
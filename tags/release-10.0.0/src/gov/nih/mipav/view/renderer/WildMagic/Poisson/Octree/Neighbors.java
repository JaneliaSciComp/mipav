package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class Neighbors {
	
	OctNode[][][] neighbors = new OctNode[3][3][3];
	
	public Neighbors(){
		clear();
	}
	
	public void clear(){
		for(int i=0;i<3;i++){
			for(int j=0;j<3;j++){
				for(int k=0;k<3;k++){
					neighbors[i][j][k]=null;
				}
			}
		}
	}
	
}

package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes.*;

public class NeighborKey2 {
	
	public Neighbors2[] neighbors;
	
	
	public void set(final int d){
		if(neighbors != null ){ neighbors = null;}
	
		if(d<0){return;}
		neighbors=new Neighbors2[d+1];
		for ( int i = 0; i < d+1; i++ ) {
			neighbors[i] = new Neighbors2();
		}
	}
	
	public Neighbors2 getNeighbors(final OctNode node){
		int d=node.depth();
		int pIndex = 0;
		if(node!=neighbors[d].neighbors[1][1][1]){
			neighbors[d].clear();

			if(node.parent == null ){neighbors[d].neighbors[1][1][1]=node;}
			else{
				int i,j,k;
				int[] x1 = new int[1];
				int[] y1 = new int[1];
				int[] z1 = new int[1];
				int[] x2 = new int[1];
				int[] y2 = new int[1];
				int[] z2 = new int[1];
				for ( int id =0; id < node.parent.children.length; id++ ) {
					if ( node == node.parent.children[id] ) {
						pIndex = id;
						break;
					}
				}
				// int idx=(int)(node-node.parent.children);
				int idx= pIndex;
				// System.err.println("idx = "+ idx);
				// Octree.pause();
				
				Cube.FactorCornerIndex(  idx   ,x1,y1,z1);
				Cube.FactorCornerIndex((~idx)&7,x2,y2,z2);
				for(i=0;i<2;i++){
					for(j=0;j<2;j++){
						for(k=0;k<2;k++){
							neighbors[d].neighbors[x2[0]+i][y2[0]+j][z2[0]+k]= node.parent.children[Cube.CornerIndex(i,j,k)];
							// System.err.println("node.parent.children[Cube.CornerIndex(i,j,k)] " + node.parent.children[Cube.CornerIndex(i,j,k)].nodeData.value);;
						}
					}
				}
				
				Neighbors2 temp=getNeighbors(node.parent);

				// Set the neighbors from across the faces
				i=x1[0]<<1;
				if(temp.neighbors[i][1][1] != null && temp.neighbors[i][1][1].children != null ){
					for(j=0;j<2;j++){for(k=0;k<2;k++){neighbors[d].neighbors[i][y2[0]+j][z2[0]+k]= temp.neighbors[i][1][1].children[Cube.CornerIndex(x2[0],j,k)];}}
				}
				j=y1[0]<<1;
				if(temp.neighbors[1][j][1] != null && temp.neighbors[1][j][1].children != null){
					for(i=0;i<2;i++){for(k=0;k<2;k++){neighbors[d].neighbors[x2[0]+i][j][z2[0]+k]= temp.neighbors[1][j][1].children[Cube.CornerIndex(i,y2[0],k)];}}
				}
				k=z1[0]<<1;
				if(temp.neighbors[1][1][k] != null && temp.neighbors[1][1][k].children != null){
					for(i=0;i<2;i++){for(j=0;j<2;j++){neighbors[d].neighbors[x2[0]+i][y2[0]+j][k]= temp.neighbors[1][1][k].children[Cube.CornerIndex(i,j,z2[0])];}}
				}

				// Set the neighbors from across the edges
				i=x1[0]<<1;	j=y1[0]<<1;
				if(temp.neighbors[i][j][1] != null && temp.neighbors[i][j][1].children != null){
					for(k=0;k<2;k++){neighbors[d].neighbors[i][j][z2[0]+k]= temp.neighbors[i][j][1].children[Cube.CornerIndex(x2[0],y2[0],k)];}
				}
				i=x1[0]<<1;	k=z1[0]<<1;
				if(temp.neighbors[i][1][k] != null && temp.neighbors[i][1][k].children != null){
					for(j=0;j<2;j++){neighbors[d].neighbors[i][y2[0]+j][k]= temp.neighbors[i][1][k].children[Cube.CornerIndex(x2[0],j,z2[0])];}
				}
				j=y1[0]<<1;	k=z1[0]<<1;
				if(temp.neighbors[1][j][k] != null && temp.neighbors[1][j][k].children != null){
					for(i=0;i<2;i++){neighbors[d].neighbors[x2[0]+i][j][k]= temp.neighbors[1][j][k].children[Cube.CornerIndex(i,y2[0],z2[0])];}
				}

				// Set the neighbor from across the corner
				i=x1[0]<<1;	j=y1[0]<<1;	k=z1[0]<<1;
				if(temp.neighbors[i][j][k] != null && temp.neighbors[i][j][k].children != null){
					neighbors[d].neighbors[i][j][k]= temp.neighbors[i][j][k].children[Cube.CornerIndex(x2[0],y2[0],z2[0])];
				}
			
			}
		}
		return neighbors[node.depth()];
	}
	
}

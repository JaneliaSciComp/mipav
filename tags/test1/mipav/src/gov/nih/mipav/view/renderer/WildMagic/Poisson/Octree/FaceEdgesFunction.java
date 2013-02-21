package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import java.util.*;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes.*;

public class FaceEdgesFunction  extends NodeAdjacencyFunction {

	public int fIndex, maxDepth;

	public Vector<Pair<Long, Long>> edges;

	public HashMap<Long, Pair<RootInfo, Integer>> vertexCount;

	public void Function(final OctNode node1, final OctNode node2) {
		if (node1.children == null
				&& MarchingCubes.HasRoots(node1.nodeData.mcIndex) != 0) {
			RootInfo ri1 = new RootInfo();
			RootInfo ri2 = new RootInfo();
			// HashMap<Long,Pair<RootInfo,Integer> >.iterator iter;

			Pair<RootInfo, Integer> iter;

			int[] isoTri = new int[Octree.DIMENSION * MarchingCubes.MAX_TRIANGLES];
			int count = MarchingCubes.AddTriangleIndices(node1.nodeData.mcIndex, isoTri);

			for (int j = 0; j < count; j++) {
				for (int k = 0; k < 3; k++) {
					if (fIndex == Cube.FaceAdjacentToEdges(isoTri[j * 3 + k], isoTri[j * 3 + ((k + 1) % 3)])) {
						if (Octree.GetRootIndex(node1, isoTri[j * 3 + k], maxDepth, ri1)
								&& Octree.GetRootIndex(node1, isoTri[j * 3+ ((k + 1) % 3)], maxDepth, ri2)) {
							edges.add(new Pair<Long, Long>(ri2.key, ri1.key));


							iter = vertexCount.get(ri1.key);
							if (iter == null) {
								vertexCount.put(ri1.key, new Pair(ri1, 0));
								// vertexCount.get(ri1.key).first = ri1;
								// vertexCount.get(ri1.key).second = 0;
							}
							iter = vertexCount.get(ri2.key);
							if (iter == null) {
								vertexCount.put(ri2.key, new Pair(ri2, 0));
								// vertexCount.get(ri2.key).first = ri2;
								// vertexCount.get(ri2.key).second = 0;
							}
							vertexCount.get(ri1.key).second--;
							vertexCount.get(ri2.key).second++;
						} else {
							System.err.println("FaceEdgeFunction:  Bad Edge 1: " + ri1.key + " , " + ri2.key);
						}
				  }
				}
			}
		}
	}

}
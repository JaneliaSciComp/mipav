package gov.nih.mipav.view.renderer.WildMagic.Knees;

public class CornerEdgePattern extends PatternDetection {
	
	public int findCornerEdge(int map[][], int startX, int startY, int xDim, int yDim, int range) {

		int x1[] = new int[1];
		int y1[] = new int[1];

		int x2[] = new int[1];
		int y2[] = new int[1];

		int findRightComponent = 0;
		int findLeftComponent = 0;

		int xLeftMostPtOnRight = -1, xLeftMostPtOnLeft = -1;
		int yLeftMostPtOnRight = -1, yLeftMostPtOnLeft = -1;
		int xRightMostPtOnRight = -1, xRightMostPtOnLeft = -1;
		int yRightMostPtOnRight = -1, yRightMostPtOnLeft = -1;

		float dist;

		for (int i = 0; i < yDim; i++) {
		 	if (map[i][startX] == 1)
		 		return 0;
		}
		
			// search on the right component in the graph map
			findRightComponent = searchOnRight(map, startX, startY, xDim, yDim, x1, x2, y1, y2);
			if ( findRightComponent == 1 ) {
				xLeftMostPtOnRight = x1[0];
				yLeftMostPtOnRight = y1[0];

				xRightMostPtOnRight = x2[0];
				yRightMostPtOnRight = y2[0];
			    // System.err.println("xLeftMostPtOnRight = " + xLeftMostPtOnRight + "  yLeftMostPtOnRight = " + yLeftMostPtOnRight);
				// System.err.println("xRightMostPtOnRight = " + xRightMostPtOnRight + "  yRightMostPtOnRight = " + yRightMostPtOnRight);
			} else {
				return 0;
			}
		
			
			// search on the left component in the graph map
			findLeftComponent = searchOnLeft(map, startX, startY, xDim, yDim, x1, x2, y1, y2);
			if ( findLeftComponent == 1 ) {
				xLeftMostPtOnLeft = x1[0];
				yLeftMostPtOnLeft = y1[0];

				xRightMostPtOnLeft = x2[0];
				yRightMostPtOnLeft = y2[0];
				// System.err.println("xLeftMostPtOnLeft = " + xLeftMostPtOnLeft + "  yLeftMostPtOnLeft = " + yLeftMostPtOnLeft);
				// System.err.println("xRightMostPtOnLeft = " + xRightMostPtOnLeft + "  yRightMostPtOnLeft = " + yRightMostPtOnLeft);
			} else {
				return 0;
			}
					
			// check distance to find the weak connected edge
			if (findRightComponent == 1 && findLeftComponent == 1) {
				dist = (float)Math.sqrt((xLeftMostPtOnRight - xRightMostPtOnLeft) * (xLeftMostPtOnRight - xRightMostPtOnLeft)
						+ (yLeftMostPtOnRight - yRightMostPtOnLeft) * (yLeftMostPtOnRight - yRightMostPtOnLeft));
				// System.err.println("dist = " + dist);
				
			
				float edgeLen = 2 * range; 
				float diagLen = (float)Math.sqrt(edgeLen*edgeLen+edgeLen*edgeLen);
				if (dist <= diagLen) {
					return 1;
				} else {
					return 0;
				}
				
				
				/*
				if (dist <= 10) {
					System.err.println("found");
					return 1;
				} else {
					System.err.println("not found");
					return 0;
				}
				*/
				
			}
	
		
		return 0;
	}
	

	protected int searchOnLeft(int map[][], int x, int y, int xDim, int yDim, int x1[], int x2[], int[] y1, int[] y2) {

		int count = 0;
		boolean[][] visited = new boolean[yDim][xDim];
		int xLeftMost = xDim;
		int yLeftMost = yDim;
		int xRightMost = 0;
		int yRightMost = 0;

		int i, j;
		
		for (i = 0; i < yDim; i++ ) {
			for ( j = 0; j < xDim; j++ ) {
				visited[i][j] = false;
			}
		}
		  
		for (i = 0; i < yDim; i++) {
		 	for (j = 0; j < x; j++) {
				if (map[i][j] == 1 && !visited[i][j]) {
                   
					x1[0] = xDim;
					y1[0] = yDim;

					x2[0] = 0;
					y2[0] = 0;

					DFS_searchOnLeft(map, j, i, xDim, yDim, x1, x2, y1, y2, visited, x);

					if (x1[0] < xLeftMost) {
						xLeftMost = x1[0];
						yLeftMost = y1[0];
					}

					if (x2[0] > xRightMost) {
						xRightMost = x2[0];
						yRightMost = y2[0];
					}

					count++;
				}
		  }
		}

		if (count >= 1) {
			// System.err.println(" count left = " + count);
			// printVisitedMapOnLeft(visited, x, y, xDim, yDim);
			// System.err.println(" xRightMost = " + xRightMost + "  yRightMost = " + yRightMost);
			x1[0] = xLeftMost;
			y1[0] = yLeftMost;

			x2[0] = xRightMost;
			y2[0] = yRightMost;

			return 1;
		} else {
			return 0;
		}

	}

	protected void DFS_searchOnLeft(int map[][], int x, int y, int xDim, int yDim, int[] x1, int x2[], int[] y1, int[] y2, boolean visited[][], int xDelimiter) {
	
	   visited[y][x] = true;

	   if ( map[y][x] == 1 ) {
			if (x < xDelimiter && x < x1[0]) {
				x1[0] = x;
				y1[0] = y;
			}
			if (x < xDelimiter && x > x2[0]) {
				x2[0] = x;
				y2[0] = y;
			}
		}

		for (int k = 0; k < 8; k++) {
			if (isSafe_searchOnLeft(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, visited, xDelimiter)) {
				DFS_searchOnLeft(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, x1, x2, y1, y2, visited, xDelimiter);
			}
		}
	}
	
	protected boolean isSafe_searchOnLeft(int map[][], int x, int y, int xDim, int yDim, boolean visited[][], int xDelimiter) {
		return (x >= 0) && (x < xDelimiter) && (y >= 0) && (y < yDim) && (map[y][x] == 1 && !visited[y][x]);
	}
	
	protected void printMapOnLeft(int map[][], int x, int y, int xDim, int yDim) {
		int i, j;
		for (i = 0; i < yDim; i++) {
		 	for (j = 0; j < x; j++) {
		 		System.err.print(map[i][j]);
		 	}
		 	System.err.println();
		 }
		
	}
	
	protected void printMapOnRight(int map[][], int x, int y, int xDim, int yDim) {
		int i, j;
		for (i = 0; i < yDim; i++) {
		 	for (j = x; j < xDim; j++) {
		 		System.err.print(map[i][j]);
		 	}
		 	System.err.println();
		 }
	}
	
	protected void printVisitedMapOnLeft(boolean visited[][], int x, int y, int xDim, int yDim) {
		System.err.println("visited map on left");
		int i, j;
		for (i = 0; i < yDim; i++) {
		 	for (j = 0; j < x; j++) {
		 		if ( visited[i][j] ) {
		 			System.err.print("1");
		 		} else {
		 			System.err.print("0");
		 		}
		 	}
		 	System.err.println();
		 }
		
	}
	
	protected void printVisitedMapOnRight(boolean visited[][], int x, int y, int xDim, int yDim) {
		System.err.println("visited map on right");
		int i, j;
		for (i = 0; i < yDim; i++) {
		 	for (j = x; j < xDim; j++) {
		 		if ( visited[i][j] ) {
		 			System.err.print("1");
		 		} else {
		 			System.err.print("0");
		 		}
		 	}
		 	System.err.println();
		 }
	}
	
	protected int searchOnRight(int map[][], int x, int y, int xDim, int yDim, int x1[], int x2[], int[] y1, int[] y2) {

		int count = 0;
		boolean[][] visited = new boolean[yDim][xDim];
		int xLeftMost = xDim;
		int yLeftMost = yDim;
		int xRightMost = 0;
		int yRightMost = 0;

		int i, j;
		
		for (i = 0; i < yDim; i++ ) {
			for ( j = 0; j < xDim; j++ ) {
				visited[i][j] = false;
			}
		}
		
		// printMapOnRight(map, x, y, xDim, yDim);
		
		for (i = 0; i < yDim; i++) {
		 	for (j = x; j < xDim; j++) {
				if (map[i][j] == 1 && !visited[i][j]) {
                   
					x1[0] = xDim;
					y1[0] = yDim;

					x2[0] = 0;
					y2[0] = 0;

					DFS_searchOnRight(map, j, i, xDim, yDim, x1, x2, y1, y2, visited, x);

					if (x1[0] < xLeftMost) {
						xLeftMost = x1[0];
						yLeftMost = y1[0];
					}

					if (x2[0] > xRightMost) {
						xRightMost = x2[0];
						yRightMost = y2[0];
					}

					count++;
				}
		  }
		}

		if (count >= 1) {
			// System.err.println(" count right = " + count);
			// printVisitedMapOnRight(visited, x, y, xDim, yDim);
			// System.err.println(" xLeftMost = " + xLeftMost + "  yLeftMost = " + yLeftMost);
			x1[0] = xLeftMost;
			y1[0] = yLeftMost;

			x2[0] = xRightMost;
			y2[0] = yRightMost;

			return 1;
		} else {
			return 0;
		}

	}

	protected void DFS_searchOnRight(int map[][], int x, int y, int xDim, int yDim, int[] x1, int x2[], int[] y1, int[] y2, boolean visited[][], int xDelimiter) {
		visited[y][x] = true;

	   if ( map[y][x] == 1 ) {
			if (x >= xDelimiter && (x < xDim) && x < x1[0]) {
				x1[0] = x;
				y1[0] = y;
			}
			if (x >= xDelimiter && (x < xDim) && x > x2[0]) {
				x2[0] = x;
				y2[0] = y;
			}
		}

		for (int k = 0; k < 8; k++) {
			if (isSafe_searchOnRight(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, visited, xDelimiter)) {
				DFS_searchOnRight(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, x1, x2, y1, y2, visited, xDelimiter);
			}
		}
	}
	
	protected boolean isSafe_searchOnRight(int map[][], int x, int y, int xDim, int yDim, boolean visited[][], int xDelimiter) {
		return (x >= xDelimiter) && (x < xDim) && (y >= 0) && (y < yDim) && (map[y][x] == 1 && !visited[y][x]);
	}
	
	

	public int test() {
		int M[][] = {
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				{0, 0, 1, 0, 0, 0, 0, 0, 0, 0},
				{0, 1, 1, 0, 0, 0, 0, 0, 0, 0},
				{0, 1, 1, 1, 0, 0, 0, 0, 0, 0},
				{0, 1, 1, 0, 0, 0, 0, 0, 1, 0},
				{0, 1, 1, 0, 0, 0, 0, 1, 1, 0},
				{0, 1, 0, 0, 0, 0, 0, 0, 1, 0},
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		};
		
		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;
		
		findCornerEdge(M, 5, 0, 10, 10, 5);
		return 0;
	}
	
	public int test1() {
		int M[][] = {
				{1, 1, 0, 0, 0, 0, 0, 0, 1, 1},
				{1, 1, 1, 0, 0, 0, 0, 0, 1, 1},
				{1, 1, 1, 0, 0, 0, 0, 1, 1, 1},
				{1, 1, 1, 1, 0, 0, 0, 1, 1, 0},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 0},
				{1, 1, 1, 0, 0, 0, 0, 1, 1, 0},
				{1, 1, 0, 0, 0, 0, 0, 1, 1, 0},
				{0, 0, 0, 0, 0, 0, 0, 1, 1, 0},
				{0, 0, 0, 0, 0, 0, 0, 1, 1, 0},
				{0, 0, 0, 0, 0, 0, 0, 1, 1, 0},
		};
		
		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;
		
		// System.err.println("number of islands is " + countIsLands(M));
		findCornerEdge(M, 5, 0, 10, 10, 0);
		return 0;
	}
	
	public int test2() {
		int M[][] = {
				{0, 0, 0, 0, 0, 0, 1, 1, 1, 1},
				{0, 0, 1, 0, 0, 0, 0, 0, 1, 1},
				{0, 1, 1, 0, 0, 0, 0, 1, 1, 1},
				{0, 1, 1, 1, 0, 0, 0, 1, 1, 1},
				{0, 1, 1, 0, 0, 0, 0, 1, 1, 1},
				{0, 1, 1, 0, 0, 0, 0, 1, 1, 1},
				{0, 1, 0, 0, 0, 0, 0, 0, 1, 1},
				{0, 0, 0, 0, 0, 0, 0, 1, 1, 1},
				{0, 0, 0, 0, 0, 0, 0, 1, 1, 1},
				{0, 0, 0, 0, 0, 0, 0, 1, 1, 1},
		};
		
		findCornerEdge(M, 5, 0, 10, 10, 0);
		return 0;
	}
	
	
}
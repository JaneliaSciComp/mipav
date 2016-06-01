package gov.nih.mipav.view.renderer.WildMagic.Knees;

public class WeakConnectedEdgePatternHorizontal extends CornerEdgePattern {
	public int findWeakEdge(int map[][], int startX, int startY, int xDim, int yDim, int range, boolean searchOnRight, int[] xResult) {

		int x1[] = new int[1];
		int y1[] = new int[1];

		int x2[] = new int[1];
		int y2[] = new int[1];

		int x3[] = new int[1];
		int y3[] = new int[1];

		int x4[] = new int[1];
		int y4[] = new int[1];

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

        		
		if (searchOnRight) {
			int searchIslandOnRight = searchSingleIsLandOnRight(map, startX, startY, xDim, yDim, x1, y1, x2, y2, x3, y3, x4, y4);
			int searchIslandOnLeft = searchSingleIsLandOnLeft(map, startX, startY, xDim, yDim, x1, y1, x2, y2, x3, y3, x4, y4);
			if (searchIslandOnRight == 1 || searchIslandOnLeft == 1) {
				return 0;
			} else if ( searchIslandOnRight >= 2 || searchIslandOnRight >= 2 ){
				return 0;
			}
		}
	     

		// search on the right component in the graph map
		findRightComponent = searchOnRight(map, startX, startY, xDim, yDim, x1, x2, y1, y2);
		if (findRightComponent == 1) {
			xLeftMostPtOnRight = x1[0];
			yLeftMostPtOnRight = y1[0];

			xRightMostPtOnRight = x2[0];
			yRightMostPtOnRight = y2[0];
			// System.err.println("xLeftMostPtOnRight = " + xLeftMostPtOnRight +
			// "  yLeftMostPtOnRight = " + yLeftMostPtOnRight);
			// System.err.println("xRightMostPtOnRight = " + xRightMostPtOnRight
			// + "  yRightMostPtOnRight = " + yRightMostPtOnRight);
		} else {
			return 0;
		}

		// search on the left component in the graph map
		findLeftComponent = searchOnLeft(map, startX, startY, xDim, yDim, x1, x2, y1, y2);
		if (findLeftComponent == 1) {
			xLeftMostPtOnLeft = x1[0];
			yLeftMostPtOnLeft = y1[0];

			xRightMostPtOnLeft = x2[0];
			yRightMostPtOnLeft = y2[0];
			// System.err.println("xLeftMostPtOnLeft = " + xLeftMostPtOnLeft +
			// "  yLeftMostPtOnLeft = " + yLeftMostPtOnLeft);
			// System.err.println("xRightMostPtOnLeft = " + xRightMostPtOnLeft +
			// "  yRightMostPtOnLeft = " + yRightMostPtOnLeft);
		} else {
			return 0;
		}

		// check distance to find the weak connected edge
		if (findRightComponent == 1 && findLeftComponent == 1) {
			dist = (float) Math.sqrt((xLeftMostPtOnRight - xRightMostPtOnLeft) * (xLeftMostPtOnRight - xRightMostPtOnLeft)
					+ (yLeftMostPtOnRight - yRightMostPtOnLeft) * (yLeftMostPtOnRight - yRightMostPtOnLeft));

			/**************** ??????????????????????? **********************/
		
			
			xResult[0] = xLeftMostPtOnRight; 
			float edgeLen = 2 * range; 
			float
			diagLen = (float)Math.sqrt(edgeLen*edgeLen+edgeLen*edgeLen); 
			if (dist <= diagLen) { return 1; } 
			else { return 0; }
			

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

	int searchSingleIsLandOnLeft(int map[][], int x, int y, int xDim, int yDim, int x1[], int[] y1, int x2[], int[] y2, int x3[], int[] y3, int x4[], int[] y4) {

		int count = 0;
		boolean[][] visited = new boolean[yDim][xDim];
		int xLeftMost = xDim;
		int yLeftMost = yDim;
		int xRightMost = 0;
		int yRightMost = 0;
		int xHighest = 0;
		int yHighest = 0;
		int xLowest = xDim;
		int yLowest = yDim;

		int i, j;

		for (i = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				visited[i][j] = false;
			}
		}

		for (i = 0; i < yDim; i++) {
			for (j = 0; j < x; j++) {
				if (map[i][j] == 1 && !visited[i][j]) {

					x1[0] = xDim;
					x1[0] = yDim;

					x2[0] = 0;
					y2[0] = 0;

					x3[0] = xDim;
					y3[0] = yDim;

					x4[0] = 0;
					y4[0] = 0;

					DFS_searchSingleIslandOnLeft(map, j, i, xDim, yDim, x1, y1, x2, y2, x3, y3, x4, y4, visited, x);

					// consider Y and X
					if (x1[0] < xLeftMost) {
						xLeftMost = x1[0];
						yLeftMost = y1[0];
					}

					if (x2[0] > xRightMost) {
						xRightMost = x2[0];
						yRightMost = y2[0];
					}

					if (y3[0] < yLowest) {
						yLowest = y3[0];
						xLowest = x3[0];
					}

					if (y4[0] > yHighest) {
						yHighest = y4[0];
						xHighest = x4[0];
					}

					count++;
				}
			}
		}

		// printVisitedMapOnLeft(visited, x, y, xDim, yDim);
		
		if ( count >= 2 ) {
			// System.err.println("count >= 2 on left side");
			return 2;
		} else if (count == 1) {
			if (xLeftMost >= 1 && xRightMost < x-1 && yLowest >= 1 && yHighest < yDim - 1) {
				// System.err.println("count == 1 on left side");
				return 1;
			} else
				return 0;
		} else {
			return 0;
		}

	}
	
	void DFS_searchSingleIslandOnLeft(int map[][], int x, int y, int xDim, int yDim, int[] x1, int[] y1, int x2[], int[] y2, int x3[], int y3[], int x4[],
			int y4[], boolean visited[][], int xDelimiter) {

		visited[y][x] = true;

		if (map[y][x] == 1) {
			if (x >= 0 && x < xDelimiter && x < xDim && x < x1[0]) {
				x1[0] = x;
				y1[0] = y;
			}
			if (x >= 0 && x < xDelimiter && x < xDim && x > x2[0]) {
				x2[0] = x;
				y2[0] = y;
			}

			if (y > 0 && y < yDim && y < y3[0]) {
				x3[0] = x;
				y3[0] = y;
			}

			if (y > 0 && y < yDim && y > y4[0]) {
				x4[0] = x;
				y4[0] = y;
			}
		}

		for (int k = 0; k < 8; k++) {
			if (isSafe_searchOnLeft(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, visited, xDelimiter)) {
				DFS_searchSingleIslandOnLeft(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, x1, y1, x2, y2, x3, y3, x4, y4, visited, xDelimiter);
			}
		}
	}
	
	int searchSingleIsLandOnRight(int map[][], int x, int y, int xDim, int yDim, int x1[], int[] y1, int x2[], int[] y2, int x3[], int[] y3, int x4[], int[] y4) {

		int count = 0;
		boolean[][] visited = new boolean[yDim][xDim];
		int xLeftMost = xDim;
		int yLeftMost = yDim;
		int xRightMost = 0;
		int yRightMost = 0;
		int xHighest = 0;
		int yHighest = 0;
		int xLowest = xDim;
		int yLowest = yDim;

		int i, j;

		for (i = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				visited[i][j] = false;
			}
		}

		for (i = 0; i < yDim; i++) {
			for (j = x; j < xDim; j++) {
				if (map[i][j] == 1 && !visited[i][j]) {

					x1[0] = xDim;
					x1[0] = yDim;

					x2[0] = 0;
					y2[0] = 0;

					x3[0] = xDim;
					y3[0] = yDim;

					x4[0] = 0;
					y4[0] = 0;

					DFS_searchSingleIslandOnRight(map, j, i, xDim, yDim, x1, y1, x2, y2, x3, y3, x4, y4, visited, x);

					// consider Y and X
					if (x1[0] < xLeftMost) {
						xLeftMost = x1[0];
						yLeftMost = y1[0];
					}

					if (x2[0] > xRightMost) {
						xRightMost = x2[0];
						yRightMost = y2[0];
					}

					if (y3[0] < yLowest) {
						yLowest = y3[0];
						xLowest = x3[0];
					}

					if (y4[0] > yHighest) {
						yHighest = y4[0];
						xHighest = x4[0];
					}

					count++;
				}
			}
		}

		// printVisitedMapOnRight(visited, x, y, xDim, yDim);

		if ( count >= 2 ) {
			// System.err.println("count >= 2 on right side");
			return 2;
		} else if (count == 1) {
			if (xLeftMost >= x + 1 && xRightMost < xDim - 1 && yLowest >= 1 && yHighest < yDim - 1) {
				// System.err.println("count == 1 on right side");
				return 1;
			} else
				return 0;
		} else {
			return 0;
		}

	}

	void DFS_searchSingleIslandOnRight(int map[][], int x, int y, int xDim, int yDim, int[] x1, int[] y1, int x2[], int[] y2, int x3[], int y3[], int x4[],
			int y4[], boolean visited[][], int xDelimiter) {

		visited[y][x] = true;

		if (map[y][x] == 1) {
			if (x >= xDelimiter && x < xDim && x < x1[0]) {
				x1[0] = x;
				y1[0] = y;
			}
			if (x >= xDelimiter && x < xDim && x > x2[0]) {
				x2[0] = x;
				y2[0] = y;
			}

			if (y > 0 && y < yDim && y < y3[0]) {
				x3[0] = x;
				y3[0] = y;
			}

			if (y > 0 && y < yDim && y > y4[0]) {
				x4[0] = x;
				y4[0] = y;
			}
		}

		for (int k = 0; k < 8; k++) {
			if (isSafe_searchOnRight(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, visited, xDelimiter)) {
				DFS_searchSingleIslandOnRight(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, x1, y1, x2, y2, x3, y3, x4, y4, visited, xDelimiter);
			}
		}
	}

	public int test() {
		int M[][] = { 
				{ 1, 1, 0, 0, 0, 0, 0, 0, 1, 1 }, 
				{ 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 }, 
				{ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 1, 0, 0, 0, 1, 0, 0 }, 
				{ 0, 0, 1, 1, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;
		int[] result = new int[1];
		// System.err.println("number of islands is " + countIsLands(M));
		findWeakEdge(M, 5, 0, 10, 10, 0, true, result);
		return 0;
	}
	
	public int test1() {
		int M[][] = { 
				{ 1, 1, 0, 0, 0, 0, 0, 0, 1, 1 }, 
				{ 1, 1, 1, 0, 0, 0, 0, 0, 0, 1 }, 
				{ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } };

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		x1[0] = Integer.MAX_VALUE;
		x2[0] = Integer.MIN_VALUE;

		y1[0] = Integer.MAX_VALUE;
		y2[0] = Integer.MIN_VALUE;
		int[] result = new int[1];
		// System.err.println("number of islands is " + countIsLands(M));
		findWeakEdge(M, 5, 0, 10, 10, 0, true, result);
		return 0;
	}
}
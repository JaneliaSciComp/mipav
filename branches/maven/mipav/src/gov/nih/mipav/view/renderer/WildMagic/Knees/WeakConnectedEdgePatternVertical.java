package gov.nih.mipav.view.renderer.WildMagic.Knees;

public class WeakConnectedEdgePatternVertical extends PatternDetection {

	public int findWeakEdge(int map[][], int startX, int startY, int xDim, int yDim, int range) {

		int x1[] = new int[1];
		int y1[] = new int[1];

		int x2[] = new int[1];
		int y2[] = new int[1];

		int findTopComponent = 0;
		int findBottomComponent = 0;

		int xLowestPtOnTop = -1, xHighestPtOnTop = -1;
		int yLowestPtOnTop = -1, yHighestPtOnTop = -1;

		int xLowestPtOnBottom = -1, xHighestPtOnBottom = -1;
		int yLowestPtOnBottom = -1, yHighestPtOnBottom = -1;

		float dist;

		for (int i = 0; i < xDim; i++) {
			if (map[startY][i] == 1)
				return 0;
		}

		// search on the top component in the graph map
		findTopComponent = searchOnTop(map, startX, startY, xDim, yDim, x1, x2, y1, y2);
		if (findTopComponent == 1) {
			xLowestPtOnTop = x1[0];
			yLowestPtOnTop = y1[0];

			xHighestPtOnTop = x2[0];
			yHighestPtOnTop = y2[0];
			// System.err.println("xLowestPtOnTop = " + xLowestPtOnTop + "  yLowestPtOnTop = " + yLowestPtOnTop);
			// System.err.println("xHighestPtOnTop = " + xHighestPtOnTop + "  yHighestPtOnTop = " + yHighestPtOnTop);
			
		} else {
			return 0;
		}

		// search on the left component in the graph map
		findBottomComponent = searchOnBottom(map, startX, startY, xDim, yDim, x1, x2, y1, y2);
		if (findBottomComponent == 1) {
			xLowestPtOnBottom = x1[0];
			yLowestPtOnBottom = y1[0];

			xHighestPtOnBottom = x2[0];
			yHighestPtOnBottom = y2[0];
			// System.err.println("xLowestPtOnBottom = " + xLowestPtOnBottom + "  yLowestPtOnBottom = " + yLowestPtOnBottom);
			// System.err.println("xHighestPtOnBottom = " + xHighestPtOnBottom + "  yHighestPtOnBottom = " + yHighestPtOnBottom);
		} else {
			return 0;
		}

		// check distance to find the weak connected edge
		if (findTopComponent == 1 && findBottomComponent == 1) {
			dist = (float) Math.sqrt((xLowestPtOnBottom - xHighestPtOnTop) * (xLowestPtOnBottom - xHighestPtOnTop) + (yLowestPtOnBottom - yHighestPtOnTop) * (yLowestPtOnBottom - yHighestPtOnTop));
           
			// System.err.println("dist = " + dist);
			float edgeLen = 2 * range;
			float diagLen = (float) Math.sqrt(edgeLen * edgeLen + edgeLen * edgeLen);
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

	int searchOnBottom(int map[][], int x, int y, int xDim, int yDim, int x1[], int x2[], int[] y1, int[] y2) {

		int count = 0;
		boolean[][] visited = new boolean[yDim][xDim];
		int xLowest = xDim;
		int yLowest = yDim;
		int xHighest = 0;
		int yHighest = 0;

		int i, j;

		for (i = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				visited[i][j] = false;
			}
		}

		for (i = y; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				if (map[i][j] == 1 && !visited[i][j]) {

					x1[0] = xDim;
					y1[0] = yDim;

					x2[0] = 0;
					y2[0] = 0;

					DFS_searchOnBottom(map, j, i, xDim, yDim, x1, x2, y1, y2, visited, y);

					if (y1[0] < yLowest) {
						xLowest = x1[0];
						yLowest = y1[0];
					}

					if (y2[0] > yHighest) {
						xHighest = x2[0];
						yHighest = y2[0];
					}

					count++;
				}
			}
		}

		if (count >= 1) {
			// printVisitedMapOnBottom(visited, x, y, xDim, yDim);
			// System.err.println(" count bottom = " + count);

			x1[0] = xLowest;
			y1[0] = yLowest;

			x2[0] = xHighest;
			y2[0] = yHighest;

			return 1;
		} else {
			return 0;
		}

	}

	void DFS_searchOnBottom(int map[][], int x, int y, int xDim, int yDim, int[] x1, int x2[], int[] y1, int[] y2, boolean visited[][], int yDelimiter) {
		visited[y][x] = true;

		if (map[y][x] == 1) {
			if (y >= yDelimiter && y < yDim && y < y1[0]) {
				x1[0] = x;
				y1[0] = y;
			}
			if (y >= yDelimiter && y < yDim && y > y2[0]) {
				x2[0] = x;
				y2[0] = y;
			}
		}

		for (int k = 0; k < 8; k++) {
			if (isSafe_searchOnBottom(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, visited, yDelimiter)) {
				DFS_searchOnBottom(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, x1, x2, y1, y2, visited, yDelimiter);
			}
		}
	}

	boolean isSafe_searchOnBottom(int map[][], int x, int y, int xDim, int yDim, boolean visited[][], int yDelimiter) {
		return (y >= yDelimiter) && (y < yDim) && (x >= 0) && (x < xDim) && (map[y][x] == 1 && !visited[y][x]);
	}

	int searchOnTop(int map[][], int x, int y, int xDim, int yDim, int x1[], int x2[], int[] y1, int[] y2) {

		int count = 0;
		boolean[][] visited = new boolean[yDim][xDim];
		int xLowest = xDim;
		int yLowest = yDim;
		int xHighest = 0;
		int yHighest = 0;

		int i, j;

		for (i = 0; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				visited[i][j] = false;
			}
		}

		// printMapOnTop(map, x, y, xDim, yDim);

		for (i = 0; i < y; i++) {
			for (j = 0; j < xDim; j++) {
				if (map[i][j] == 1 && !visited[i][j]) {

					x1[0] = xDim;
					y1[0] = yDim;

					x2[0] = 0;
					y2[0] = 0;

					DFS_searchOnTop(map, j, i, xDim, yDim, x1, x2, y1, y2, visited, y);

					if (y1[0] < yLowest) {
						xLowest = x1[0];
						yLowest = y1[0];
					}

					if (y2[0] > yHighest) {
						xHighest = x2[0];
						yHighest = y2[0];
					}

					count++;
				}
			}
		}

		if (count >= 1) {
			// System.err.println(" count top = " + count);
			// printVisitedMapOnTop(visited, x, y, xDim, yDim);
			x1[0] = xLowest;
			y1[0] = yLowest;

			x2[0] = xHighest;
			y2[0] = yHighest;

			return 1;
		} else {
			return 0;
		}

	}

	void DFS_searchOnTop(int map[][], int x, int y, int xDim, int yDim, int[] x1, int x2[], int[] y1, int[] y2, boolean visited[][], int yDelimiter) {
		visited[y][x] = true;

		if (map[y][x] == 1) {
			if (y >= 0 && y < yDelimiter && y < y1[0]) {
				x1[0] = x;
				y1[0] = y;
			}
			if (y >= 0 && y < yDelimiter && y > y2[0]) {
				x2[0] = x;
				y2[0] = y;
			}
		}

		for (int k = 0; k < 8; k++) {
			if (isSafe_searchOnTop(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, visited, yDelimiter)) {
				DFS_searchOnTop(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, x1, x2, y1, y2, visited, yDelimiter);
			}
		}
	}

	boolean isSafe_searchOnTop(int map[][], int x, int y, int xDim, int yDim, boolean visited[][], int yDelimiter) {
		return (y >= 0) && (y < yDelimiter) && (x >= 0) && (x < xDim) && (map[y][x] == 1 && !visited[y][x]);
	}

	public void printMapOnTop(int [][]map, int x, int y, int xDim, int yDim) {
		System.err.println("print top map");
		int i, j;
		
		for (i = 0; i < y; i++) {
			for (j = 0; j < xDim; j++) {
				System.err.print(map[i][j]);
			}
			System.err.println();
		}
	}
	
	public void printMapOnBottom(int [][]map, int x, int y, int xDim, int yDim) {

		System.err.println("print bottom map");
		int i, j;
		for (i = y; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				System.err.print(map[i][j]);
			}
			System.err.println();
		}
	}
	
	public void printVisitedMapOnTop(boolean [][]map, int x, int y, int xDim, int yDim) {
		System.err.println("print top visited map");
		int i, j;
		
		for (i = 0; i < y; i++) {
			for (j = 0; j < xDim; j++) {
				if ( map[i][j] ) {
					System.err.print("1");
				} else {
					System.err.print("0");
				}
			}
			System.err.println();
		}
	}
	
	public void printVisitedMapOnBottom(boolean [][]map, int x, int y, int xDim, int yDim) {

		System.err.println("print bottom visited map");
		int i, j;
		for (i = y; i < yDim; i++) {
			for (j = 0; j < xDim; j++) {
				if ( map[i][j] ) {
					System.err.print("1");
				} else {
					System.err.print("0");
				}
			}
			System.err.println();
		}
	}
	
	public int test() {
		int M[][] = { 
				{ 0, 0, 0, 0, 0, 0, 0, 1, 1, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 1, 1, 1, 1 }, 
				{ 0, 0, 0, 0, 0, 0, 1, 1, 1, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 }, 
				{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 },
				{ 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 }, 
				{ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 } 
		};

		// System.err.println("number of islands is " + countIsLands(M));
		findWeakEdge(M, 5, 5, 10, 10, 0);
	
		return 0;
	}

}
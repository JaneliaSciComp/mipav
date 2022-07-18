package gov.nih.mipav.view.renderer.WildMagic.Knees;

public class PatternDetection {
	
	protected static int rowNumber[] = {-1, -1, -1, 0, 0, 1, 1, 1};
	protected static int colNumber[] = {-1, 0, 1, -1, 1, -1, 0, 1};
	
    public void DFS(int map[][], int x, int y, int xDim, int yDim, int[] x1, int x2[], int[] y1, int[] y2, boolean visited[][]) {
		
    	visited[y][x] = true;

		if ( map[y][x] == 1 ) {
			if (x < x1[0])
				x1[0] = x;
			if (x > x2[0])
				x2[0] = x;
			if (y < y1[0])
				y1[0] = y;
			if (y > y2[0])
				y2[0] = y;
		}

		for (int k = 0; k < 8; k++) {
			if (isSafe(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, visited)) {
				DFS(map, x + colNumber[k], y + rowNumber[k], xDim, yDim, x1, x2, y1, y2, visited);
			}
		}
	}
	
    public boolean isSafe(int map[][], int x, int y, int xDim, int yDim, boolean visited[][]) {
		return (x >= 0) && (x < xDim) && (y >= 0) && (y < yDim) && (map[y][x] == 1 && !visited[y][x]);
	}

    
    public void printMap(int map[][], int xDim, int yDim) {
        int i, j;
		for (i = 0; i < yDim; i++) {
		 	for (j = 0; j < xDim; j++) {
		 		System.err.print(map[i][j]);
		 	}
		 	System.err.println();
		}	
		System.err.println();
	}
    
    public void printVisitedMap(boolean visited[][], int xDim, int yDim) {
        int i, j;
		for (i = 0; i < yDim; i++) {
		 	for (j = 0; j < xDim; j++) {
		 		if( visited[i][j] ) {
		 			System.err.print("1");
		 		} else {
		 			System.err.print("0");
		 		}
		 	}
		 	System.err.println();
		}	
		System.err.println();
	}
}
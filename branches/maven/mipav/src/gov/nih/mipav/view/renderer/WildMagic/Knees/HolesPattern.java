package gov.nih.mipav.view.renderer.WildMagic.Knees;

public class HolesPattern extends PatternDetection {
	
	public int findHoles(int map[][], int startX, int startY, int xDim, int yDim) {
		boolean[][] visited = new boolean[yDim][xDim];
        int i, j;
		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		for ( i = 0; i < yDim; i++ ) {
			for ( j = 0; j < xDim; j++ ) {
				visited[i][j] = false;
			}
		}

		int minX = Integer.MAX_VALUE;
		int maxX = Integer.MIN_VALUE;

		int minY = Integer.MAX_VALUE;
		int maxY = Integer.MIN_VALUE;

		if (map[startY][startX] == 1 && !visited[startY][startX]) {

			x1[0] = Integer.MAX_VALUE;
			x2[0] = Integer.MIN_VALUE;

			y1[0] = Integer.MAX_VALUE;
			y2[0] = Integer.MIN_VALUE;

			DFS(map, startX, startY, xDim, yDim, x1, x2, y1, y2, visited);

			minX = x1[0];
			maxX = x2[0];
			minY = y1[0];
			maxY = y2[0];
		}

		if ( minX == Integer.MAX_VALUE && maxX == Integer.MIN_VALUE && 
				 minY == Integer.MAX_VALUE && maxY == Integer.MIN_VALUE ) 
			return 0;
		
		if (minX > 2 && minY > 2 && maxX < (xDim - 2) && maxY < (yDim - 2) ) {
			// printVisitedMap(visited, xDim, yDim);
			return 1;
		} else {
			return 0;
		}
	}
	
	public int detectHoles(int map[][], int startX, int startY, int xDim, int yDim) {
		boolean[][] visited = new boolean[yDim][xDim];
		
		int i, j;

		// printMap(map);
		
		for ( i = 0; i < yDim; i++ ) {
			for ( j = 0; j < xDim; j++ ) {
				visited[i][j] = false;
			}
		}

		int x1[] = new int[1];
		int x2[] = new int[1];
		int y1[] = new int[1];
		int y2[] = new int[1];

		int minX = Integer.MAX_VALUE;
		int maxX = Integer.MIN_VALUE;

		int minY = Integer.MAX_VALUE;
		int maxY = Integer.MIN_VALUE;
		

		if (map[startY][startX] == 1 && !visited[startY][startX]) {

			x1[0] = Integer.MAX_VALUE;
			x2[0] = Integer.MIN_VALUE;

			y1[0] = Integer.MAX_VALUE;
			y2[0] = Integer.MIN_VALUE;

			DFS(map, startX, startY, xDim, yDim, x1, x2, y1, y2, visited);

			minX = x1[0];
			maxX = x2[0];
			minY = y1[0];
			maxY = y2[0];

			if (minX > 0 && minY > 0 && maxX < xDim-1  && maxY < yDim-1) {
				// printVisitedMap(visited, xDim, yDim);
				return 1;
			} else {
				return 0;
			}
		}
	
		return 0;
	}

	int test() {
		int M[][] = {
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				{0, 0, 0, 0, 1, 1, 1, 0, 0, 0},
				{0, 0, 0, 1, 1, 1, 0, 0, 0, 0},
				{0, 0, 0, 1, 1, 1, 0, 0, 0, 0},
				{0, 0, 1, 1, 1, 1, 0, 0, 0, 0},
				{0, 0, 1, 1, 1, 0, 0, 0, 0, 0},
				{0, 0, 0, 1, 0, 0, 0, 0, 0, 0},
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
				{0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
		};
		
		if ( findHoles(M, 2, 9, 10, 10) == 1 ) {
			System.err.println("detect holes");
		}
		return 0;
	}
	
	int test1() {
		int M[][] = {
				{0, 0, 0, 0, 0, 0, 0, 1, 0, 0},
				{0, 0, 0, 0, 0, 0, 1, 1, 0, 0},
				{0, 0, 0, 0, 1, 1, 1, 0, 0, 0},
				{0, 0, 0, 0, 1, 1, 0, 0, 0, 0},
				{0, 0, 0, 1, 1, 0, 0, 0, 0, 0},
				{0, 0, 1, 1, 0, 0, 0, 0, 0, 0},
				{0, 0, 1, 1, 0, 0, 0, 0, 0, 0},
				{0, 0, 1, 0, 0, 0, 0, 0, 0, 0},
				{0, 1, 1, 0, 0, 0, 0, 0, 0, 0},
				{0, 1, 0, 0, 0, 0, 0, 0, 0, 0},
		};
		
		if ( findHoles(M, 6, 2, 10, 10) == 1 ) {
			System.err.println("detect holes");
		}
		return 0;
	}
	
	int test3() {
		int M[][] = {
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		};
		
		if ( findHoles(M, 6, 2, 10, 10) == 1 ) {
			System.err.println("detect holes");
		}
		return 0;
	}
	
	
}
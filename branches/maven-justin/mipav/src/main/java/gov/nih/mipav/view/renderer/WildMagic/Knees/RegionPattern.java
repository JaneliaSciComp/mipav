package gov.nih.mipav.view.renderer.WildMagic.Knees;

public class RegionPattern {
	
	public static int FuzzyC = 0;
	public static int Class1 = 1;
	public static int Class2 = 2;
	public static int Class3 = 3;
	public static int GRE = 5;
	public static int Class1_weak = 6;
	public static int Class1_lowInten = 7;
	public static int Class1_corner = 8;
	public static int FuzzyC_class1 = 0;
	public static int GRE_HIGH_INTEN = 9;
	public static int CLASS3_HIGH_INTEN = 10;
	public static int GRE_GREY = 11;
	public static int Class3_LowInten = 11;
	public static int FuzzyC_GRE_LOW = 12;
	public static int GRE_BLACK = 13;
	public static int GRE_FUZZY_CLASS3 = 14;
	public static int GRE_FUZZY_CLASS1 = 15;
	
	public int searchRegion(int map[][], int startX, int startY, int xDim, int yDim, int range, int type) {
        int i, j;
        int sum = xDim * yDim;
        int count = 0;
        float percent;
		
		for ( i = 0; i < yDim; i++ ) {
			for ( j = 0; j < xDim; j++ ) {
				if ( map[i][j] == 1 ) {
					count++;
				}
			}
		}
		
	    percent = (float)(count / sum);
	    // System.err.println("percent = " + percent);
	    if ( type == GRE_BLACK ) { 
	    	if ( percent >= 0.2f ) {
				return 1;
			} else return 0;
	    } else if ( type == GRE ) { 
	    	if ( percent >= 0.7f ) {
				return 1;
			} else return 0;
	    } else if (type == GRE_GREY){
	    	if ( percent >= 0.8f ) {
				return 1;
			} else return 0;
	    } else if (type == FuzzyC){
	    	if ( percent >= 0.95f ) {
				return 1;
			} else return 0;
	    } else if (type == GRE_FUZZY_CLASS3){
	    	if ( percent >= 0.95f ) {
				return 1;
			} else return 0;
	    } else if (type == GRE_FUZZY_CLASS1){
	    	if ( percent >= 0.95f ) {
				return 1;
			} else return 0;
	    }else {
		    if ( percent >= 0.6f ) {
				return 1;
			} else return 0;
	    }
	}

	
	
	public int test() {
		int M[][] = {
				{1, 0, 1, 1, 0, 0, 0, 0, 1, 1},
				{0, 0, 1, 1, 0, 0, 1, 1, 1, 1},
				{0, 1, 0, 1, 0, 1, 1, 1, 1, 0},
				{1, 0, 1, 0, 1, 1, 0, 1, 1, 1},
				{1, 1, 1, 1, 1, 0, 1, 1, 1, 1},
				{0, 1, 1, 1, 1, 1, 1, 0, 1, 1},
				{0, 1, 1, 1, 1, 0, 1, 1, 1, 1},
				{1, 1, 1, 1, 1, 0, 1, 1, 1, 1},
				{0, 1, 0, 1, 0, 0, 1, 0, 1, 1},
				{1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		};
		
		if ( searchRegion(M, 0, 1, 10, 10, 5, 1) == 1 ) {
		 	System.err.println("find region");
		}
		return 0;
	}
	
	public int test1() {
		int M[][] = {
				{0, 0, 0, 0, 0, 0, 0, 1, 0, 0},
				{0, 0, 0, 0, 0, 0, 1, 1, 0, 0},
				{0, 0, 0, 0, 0, 1, 1, 0, 0, 0},
				{0, 0, 0, 0, 1, 1, 0, 0, 0, 0},
				{0, 0, 0, 1, 1, 0, 0, 0, 0, 0},
				{0, 0, 1, 1, 0, 0, 0, 0, 0, 0},
				{0, 0, 1, 1, 0, 0, 0, 0, 0, 0},
				{0, 0, 1, 0, 0, 0, 0, 0, 0, 0},
				{0, 1, 1, 0, 0, 0, 0, 0, 0, 0},
				{0, 1, 0, 0, 0, 0, 0, 0, 0, 0},
		};
		
		if ( searchRegion(M, 6, 1, 10, 10, 5, 1) == 1 ) {
		 	System.err.println("find region");
		}
		return 0;
	}
	
}
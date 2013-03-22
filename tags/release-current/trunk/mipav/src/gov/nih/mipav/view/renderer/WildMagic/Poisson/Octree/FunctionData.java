package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

public class FunctionData {

	public static final int DOT_FLAG = 1;
	public static final int D_DOT_FLAG = 2;
	public static final int D2_DOT_FLAG = 4;
	public static final int VALUE_FLAG = 1;
	public static final int D_VALUE_FLAG = 2;

	public int Degree;
	public int depth, res, res2;
	public float[] dotTable, dDotTable, d2DotTable;
	public float[] valueTables, dValueTables;
	public PPolynomial baseFunction;
	public PPolynomial dBaseFunction;
	public PPolynomial[] baseFunctions;

	public int useDotRatios;
	public int normalize;

	public FunctionData(int _Degree) {
		Degree = _Degree;
		baseFunction = new PPolynomial(Degree);
		dBaseFunction = new PPolynomial(Degree-1);
		
		dotTable = dDotTable = d2DotTable = null;
		valueTables = dValueTables = null;
		res = 0;
	}

	public void dispose() {
		dotTable = dDotTable = d2DotTable = null;
		valueTables = dValueTables = null;
		res = 0;
	}

	public void set(final int maxDepth, final PPolynomial F, final int normalize, final int useDotRatios) {
		int i;
		this.normalize = normalize;
		this.useDotRatios = useDotRatios;

		depth = maxDepth;
		res = BinaryNode.CumulativeCenterCount(depth);
		res2 = (1 << (depth + 1)) + 1;
		baseFunctions = new PPolynomial[res];
		for (i = 0; i < res; i++) {
			baseFunctions[i] = new PPolynomial(Degree + 1);
		}
		// Scale the function so that it has:
		// 0] Value 1 at 0
		// 1] Integral equal to 1
		// 2] Square integral equal to 1
		switch (normalize) {
		case 2:
			baseFunction = F.div(Math.sqrt((F.mul(F)).integral(
					F.polys[0].start, F.polys[F.polyCount - 1].start)));
			break;
		case 1:
			baseFunction = F.div(F.integral(F.polys[0].start,
					F.polys[F.polyCount - 1].start));
			break;
		default:
			baseFunction = F.div(F.evaluate(0));
		}
		dBaseFunction = baseFunction.derivative();
		float[] c1 = new float[1];
		float[] w1 = new float[1];
		for (i = 0; i < res; i++) {
			BinaryNode.CenterAndWidth(i, c1, w1);
			baseFunctions[i] = baseFunction.scale(w1[0]).shift(c1[0]);
			// Scale the function so that it has L2-norm equal to one
			switch (normalize) {
			case 2:
				baseFunctions[i].div_into(Math.sqrt(w1[0]));
				break;
			case 1:
				baseFunctions[i].div_into(w1[0]);
				break;
			}
		}
	}
	
	
	public void setDotTables(final int flags){
		clearDotTables(flags);
		int size;
		size=(res*res+res)>>1;
		if( ( flags & DOT_FLAG ) != 0){
			dotTable=new float[size];
			// memset(dotTable,0,sizeof(Real)*size);
		}
		if( ( flags & D_DOT_FLAG ) != 0){
			dDotTable=new float[size];
			// memset(dDotTable,0,sizeof(Real)*size);
		}
		if( ( flags & D2_DOT_FLAG ) != 0 ){
			d2DotTable=new float[size];
			// memset(d2DotTable,0,sizeof(Real)*size);
		}
		double t1,t2;
		t1=baseFunction.polys[0].start;
		t2=baseFunction.polys[baseFunction.polyCount-1].start;
		System.err.println("t1 = " + t1 + " t2 = " + t2);
		for(int i=0;i<res;i++){
			float[] c1 = new float[1];
			float[] c2 = new float[1];
			float[] w1 = new float[1];
			float[] w2 = new float[1];
			BinaryNode.CenterAndWidth(i,c1,w1);
			double start1	=t1*w1[0]+c1[0];
			double end1		=t2*w1[0]+c1[0];
			for(int j=0;j<=i;j++){
				BinaryNode.CenterAndWidth(j,c2,w2);
				int idx=SymmetricIndex(i,j);

				double start	=t1*w2[0]+c2[0];
				double end		=t2*w2[0]+c2[0];

				if(start<start1){start=start1;}
				if(end>end1)	{end=end1;}
				if(start>=end){continue;}

				BinaryNode.CenterAndWidth(j,c2,w2);
				float dot=dotProduct(c1[0],w1[0],c2[0],w2[0]);
				if((float)Math.abs(dot)<1e-15){continue;}
				if( ( flags & DOT_FLAG) != 0 ){dotTable[idx]=dot;}
				if(useDotRatios != 0){
					if( ( flags & D_DOT_FLAG ) != 0){
						dDotTable[idx]=-dDotProduct(c1[0],w1[0],c2[0],w2[0])/dot;
						// System.err.println("dDotTable[" + idx + "] = " + dDotTable[idx]);
					}
					if( ( flags & D2_DOT_FLAG ) != 0 ){
						d2DotTable[idx]=d2DotProduct(c1[0],w1[0],c2[0],w2[0])/dot;
						// System.err.println("d2DotTable[" + idx + "] = " + d2DotTable[idx]);
					}
				}
				else{
					if( ( flags & D_DOT_FLAG ) != 0 ){
						dDotTable[idx]= dDotProduct(c1[0],w1[0],c2[0],w2[0]);
						// System.err.println("dDotTable[" + idx + "] = " + dDotTable[idx]);
					}
					if( ( flags & D2_DOT_FLAG ) != 0 ){
						d2DotTable[idx]=d2DotProduct(c1[0],w1[0],c2[0],w2[0]);
						// System.err.println("d2DotTable[" + idx + "] = " + d2DotTable[idx]);
					}
				}
				// Octree.pause();
			}
		}
	}

	public void clearDotTables(final int flags){
		if((flags & DOT_FLAG) != 0 && dotTable != null ){
			// delete[] dotTable;
			dotTable=null;
		}
		if((flags & D_DOT_FLAG) != 0 && dDotTable != null ){
			// delete[] dDotTable;
			dDotTable= null;
		}
		if((flags & D2_DOT_FLAG) != 0 && d2DotTable != null ){
			// delete[] d2DotTable;
			d2DotTable=null;
		}
	}
	
	public void setValueTables(final int flags, final double smooth){
		clearValueTables();
		if( ( flags &   VALUE_FLAG ) != 0 ){ valueTables=new float[res*res2];}
		if( ( flags & D_VALUE_FLAG ) != 0 ){dValueTables=new float[res*res2];}
		PPolynomial function = new PPolynomial(Degree+1);
		PPolynomial dFunction = new PPolynomial(Degree);
		for(int i=0;i<res;i++){
			if(smooth>0){
				function=baseFunctions[i].MovingAverage(smooth);
				dFunction=baseFunctions[i].derivative().MovingAverage(smooth);
			}
			else{
				function = (baseFunctions[i]);
				dFunction=baseFunctions[i].derivative();
			}
			for(int j=0;j<res2;j++){
				double x=(double)(j)/(res2-1);
				if( ( flags &   VALUE_FLAG ) != 0 ){ valueTables[j*res+i]=(float)( function.evaluate(x));}
				if( ( flags & D_VALUE_FLAG ) != 0 ){dValueTables[j*res+i]=(float)(dFunction.evaluate(x));}
			}
		}
	}
	
	
	public void setValueTables(final int flags, final double valueSmooth, final double normalSmooth){
		clearValueTables();
		if( ( flags &   VALUE_FLAG ) != 0 ){ valueTables=new float[res*res2];}
		if( ( flags & D_VALUE_FLAG ) != 0 ){dValueTables=new float[res*res2];}
		PPolynomial function = new PPolynomial(Degree+1);
		PPolynomial  dFunction = new PPolynomial(Degree);
		for(int i=0;i<res;i++){
			if(valueSmooth>0)	{ function=baseFunctions[i].MovingAverage(valueSmooth);}
			else				{ function = (baseFunctions[i]);}
			if(normalSmooth>0)	{dFunction=baseFunctions[i].derivative().MovingAverage(normalSmooth);}
			else				{dFunction=baseFunctions[i].derivative();}

			for(int j=0;j<res2;j++){
				double x=(double)(j)/(res2-1);
				if(( flags &   VALUE_FLAG) != 0){ valueTables[j*res+i]=(float)( function.evaluate(x));}
				if(( flags & D_VALUE_FLAG) != 0 ){dValueTables[j*res+i]=(float)(dFunction.evaluate(x));}
			}
		}
	}
	
	public void clearValueTables(){
		valueTables=dValueTables=null;
	}
	
	
	public final float dotProduct(final double center1, final double width1, final double center2, final double width2) {
		double r=(float)Math.abs(baseFunction.polys[0].start);
		switch(normalize){
			case 2:
				return (float)((baseFunction.mul(baseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)*width1/Math.sqrt(width1*width2)));
			case 1:
				return (float)((baseFunction.mul(baseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)*width1/(width1*width2)));
			default:
				return (float)((baseFunction.mul(baseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)*width1));
		}
	}
	
	public final float dDotProduct(final double center1, final double width1, final double center2, final double width2) {
		double r=Math.abs(baseFunction.polys[0].start);
		switch(normalize){
			case 2:
				return (float)((dBaseFunction.mul(baseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)/Math.sqrt(width1*width2)));
			case 1:
				return (float)((dBaseFunction.mul(baseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)/(width1*width2)));
			default:
				return (float)((dBaseFunction.mul(baseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)));
		}
	}
	
	public final float d2DotProduct(final double center1, final double width1, final double center2, final double width2) {
		double r=Math.abs(baseFunction.polys[0].start);
		switch(normalize){
			case 2:
				return (float)((dBaseFunction.mul(dBaseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)/width2/Math.sqrt(width1*width2)));
			case 1:
				return (float)((dBaseFunction.mul(dBaseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)/width2/(width1*width2)));
			default:
				return (float)((dBaseFunction.mul(dBaseFunction.scale(width2/width1).shift((center2-center1)/width1)).integral(-2*r,2*r)/width2));
		}
	}

	
	public static final int SymmetricIndex(final int i1, final int i2){
		if(i1>i2)	{return ((i1*i1+i1)>>1)+i2;}
		else		{return ((i2*i2+i2)>>1)+i1;}
	}
	
	public static final boolean SymmetricIndex(final int i1, final int i2,int[] index){
		if(i1<i2){
			index[0]=((i2*i2+i2)>>1)+i1;
			return true;
		}
		else{
			index[0]=((i1*i1+i1)>>1)+i2;
			return false;
		}
	}
	
	
	
}
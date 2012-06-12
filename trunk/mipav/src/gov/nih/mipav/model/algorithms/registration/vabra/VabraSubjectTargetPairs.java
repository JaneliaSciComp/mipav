package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.IOException;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfOneVariable;
import de.jtem.numericalMethods.calculus.minimizing.Brent;


public class VabraSubjectTargetPairs implements RealFunctionOfOneVariable {

	/**
     *    Robust maximum estimation
     *    @param 	ratio	float fraction in [0,1]: the minimum number of points above or equal to the maximum over the total volume
	 *    @param 	scales	int: the number of times the scale is refined for finding the robust maximum
	 *	  @param	nx,ny,nz	image dimensions
	 *	  @return 			the robust maximum value	
     */
	private static final float robustMaximum(float[] image, float ratio, int scales, int nx, int ny, int nz ) {
		float Imin,Imax,Rmax;
		int Nbins = 10;
		float[] bins = new float[Nbins];
		float count;
		int n;
		int index;
		int slice = nx * ny;
		
		// ratio: global value
		ratio = ratio*nx*ny*nz;
		
		// find first min, max
		Imin = image[0];
		Imax = image[0];
		for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) for (int z=0;z<nz;z++) {
			//float val = image.getFloat(x,y,z);
			index = z *slice + y * nx + x;
			float val = image[index];
			if (val>Imax) Imax = val;
			if (val<Imin) Imin = val;
		}
		Rmax = Imax;
		
		for (int t=0;t<scales;t++) {
			
			Rmax = Imax;
			
			// compute coarse histogram
			for (n=0;n<Nbins;n++) bins[n] = 0;
			
			for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) for (int z=0;z<nz;z++) {
				//float val = image.getFloat(x,y,z);
				index = z *slice + y * nx + x;
				float val = image[index];
				// first one include both boundaries 
				if (  (val >= Imin )
					&&(val <= Imin + 1.0f/Nbins*(Imax-Imin) ) ) bins[0]++;
				for (n=1;n<Nbins;n++) {
					if (  (val >  Imin + (float)n/(float)Nbins*(Imax-Imin) )
						&&(val <= Imin + (float)(n+1)/(float)Nbins*(Imax-Imin) ) ) bins[n]++;	
				}
			}
			// find the value corresponding to the ratio
			count = 0;
			n=Nbins;
			while ( (count < ratio) && (n>0) ) {
				n=n-1;
				count +=bins[n];
			}
			Rmax = Imin + (n+0.5f)/Nbins*(Imax-Imin);	

			// new boundaries
			float I0 = Imin + (float)n/(float)Nbins*(Imax-Imin);
			float I1 = Imin + (float)(n+1)/(float)Nbins*(Imax-Imin);
			
			Imin = I0;
			Imax = I1;
			
			// new ratio
			ratio = ratio - (count-bins[n]);			
		}
		
		return Rmax;
	}

	/**
     *    Robust maximum estimation
     *    @param 	ratio	float fraction in [0,1]: the minimum number of points below or equal to the minimum over the total volume
	 *    @param 	scales	int: the number of times the scale is refined for finding the robust minimum
	 *	  @return 			the robust minimum value	
     */
    private static final float robustMinimum(float[] image, float ratio, int scales, int nx, int ny, int nz ) {
		float Imin,Imax,Rmin;
		int Nbins = 10;
		float[] bins = new float[Nbins];
		float count;
		int n;
		int index;
		int slice = nx * ny;
		
		// ratio: global value
		ratio = ratio*nx*ny*nz;
		
		// find first min, max
		Imin = image[0];
		Imax = image[0];
		for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) for (int z=0;z<nz;z++) {
			//float val = image.getFloat(x,y,z);
			index = z *slice + y * nx + x;
			float val = image[index];
			if (val>Imax) Imax = val;
			if (val<Imin) Imin = val;
		}
		Rmin = Imin;
		
		for (int t=0;t<scales;t++) {
			
			Rmin = Imin;
		
			// compute coarse histogram
			for (n=0;n<Nbins;n++) bins[n] = 0;
			
			for (int x=0;x<nx;x++) for (int y=0;y<ny;y++) for (int z=0;z<nz;z++) {
				//float val = image.getFloat(x,y,z);
				index = z *slice + y * nx + x;
				float val = image[index];
				// first one include both boundaries 
				if (  (val >= Imin )
					&&(val <= Imin + 1.0f/Nbins*(Imax-Imin) ) ) bins[0]++;
				for (n=1;n<Nbins;n++) {
					if (  (val >  Imin + (float)n/(float)Nbins*(Imax-Imin) )
						&&(val <= Imin + (float)(n+1)/(float)Nbins*(Imax-Imin) ) ) bins[n]++;	
				}
			}
			
			// find the value corresponding to the ratio
			count = 0;
			n=0;
			while ( (count < ratio) && (n<Nbins) ) {
				count +=bins[n];
				n=n+1;
			}
			Rmin = Imin + (n-0.5f)/Nbins*(Imax-Imin);
		
			// new boundaries
			float I0 = Imin + (float)(n-1)/(float)Nbins*(Imax-Imin);
			float I1 = Imin + (float)(n)/(float)Nbins*(Imax-Imin);
			
			Imin = I0;
			Imax = I1;
			
			// new ratio
			ratio = ratio - (count-bins[n-1]);		
		}
		
		return Rmin;
	}
	
    private int maxDimensions;
    private VabraVolumeCollection subject;

    private VabraVolumeCollection deformedSubject;
    private VabraVolumeCollection target;

    private VabraVolumeCollection targetBinned;

    private VabraVolumeCollection deformedSubjectBinned;

    private double currentDownSampleFactor;
    private int boundingBox[] = new int[6];

    private float[] currentDeformField; //deformation field as an array for easy access
    private float[] totalDeformField; //deformation field 
	
    private ModelImage origSubjectList;
    private ModelImage origTargetList;
    private int chInterpType;
    private int numOfBins;
    private float RobustMinThresh, RobustMaxThresh;
	
    private int defFieldUpdateMode;

	private int curX, curY, curZ;

	
	private VabraVolumeCollection deformedSubjectCopy;

	private ModelImage[]  totalDeformFieldSplit;
	
	
	public VabraSubjectTargetPairs(ModelImage subjectVols, ModelImage targetVols, 
			float robustMaxT, float robustMinT, int numBins, int interpType,boolean useMNMI, double[] directionsOptmizationWeight, int defFieldUpdateMode) {
		this.numOfBins = numBins;
		this.origSubjectList=subjectVols;
		this.origTargetList=targetVols;
		this.chInterpType = interpType;
		this.subject=new VabraVolumeCollection(subjectVols, chInterpType, numOfBins, true);
		deformedSubject=this.subject.clone();
		this.target=new VabraVolumeCollection(targetVols, chInterpType, numOfBins, true);
		this.defFieldUpdateMode = defFieldUpdateMode;

		curX = subjectVols.getExtents().length > 0 ? subjectVols.getExtents()[0] : 1;
		curY = subjectVols.getExtents().length > 1 ? subjectVols.getExtents()[1] : 1;
		curZ = subjectVols.getExtents().length > 2 ? subjectVols.getExtents()[2] : 1;

		calculateBoundingBox();

		RobustMinThresh = robustMinT;
		RobustMaxThresh = robustMaxT;


		threshAtRobustMaxAndMin(subject);
		threshAtRobustMaxAndMin(target);
		threshAtRobustMaxAndMin(deformedSubject);
		
		targetBinned = target.clone();
		targetBinned.rescaleToBins();
		
		totalDeformFieldSplit = new ModelImage[3];

		
		
		// Optimizer
		this.directionsOptmizationWeight = directionsOptmizationWeight;
		this.defFieldUpdateMode = defFieldUpdateMode;

		localROI = new int[6];
		rbf = new VabraRBF();
		localCoarseGradient = new double[3];
		coarseLocalRegionCenter = new int[3];
		
		
		
		
		
		
		
		
		// Histogram 
		defSxPlus = new int[numOfBins];
		defSyPlus = new int[numOfBins];
		defSzPlus = new int[numOfBins];
		
		defSxMinus = new int[numOfBins];
		defSyMinus = new int[numOfBins];
		defSzMinus = new int[numOfBins];
		origDeformedSubject = new int[numOfBins];
		origTarget = new int[numOfBins];
		
		currentDeformedSubject = new int[numOfBins];
		
		//allocated joint histograms
		defSTxPlus = new int[numOfBins*numOfBins];
		defSTyPlus = new int[numOfBins*numOfBins];
		defSTzPlus = new int[numOfBins*numOfBins];

		defSTxMinus = new int[numOfBins*numOfBins];
		defSTyMinus = new int[numOfBins*numOfBins];
		defSTzMinus = new int[numOfBins*numOfBins];
		
		origJointST = new int[numOfBins*numOfBins];
		
		currentJointST = new int[numOfBins*numOfBins];
	}

	public int coarseGradientParameters() {
		return 3; 
	}

	public void dispose() {
		
		
		if ( totalDeformField != null )
		{
			totalDeformField = null;
		}
		if ( subject != null )
		{
			subject.disposeLocal();
			subject=null;
		}
		if ( target != null )
		{
			target.disposeLocal();
			target=null;
		}
		if ( deformedSubject != null )
		{
			deformedSubject.disposeLocal();
			deformedSubject=null;
		}
		if ( deformedSubjectCopy != null )
		{
			deformedSubjectCopy.disposeLocal();
			deformedSubjectCopy=null;
		}
		if ( deformedSubjectBinned != null )
		{
			deformedSubjectBinned.disposeLocal();
			deformedSubjectBinned=null;
		}
		if ( targetBinned != null )
		{
			targetBinned.disposeLocal();
			targetBinned=null;
		}
		for ( int i = 0; i < totalDeformFieldSplit.length; i++ )
		{
			if ( totalDeformFieldSplit[i] != null )
			{
				totalDeformFieldSplit[i].disposeLocal();
				totalDeformFieldSplit[i] = null;
			}
		}
		
		currentDeformField = null;
		
		origSubjectList=null;
		origTargetList=null;
		targetBinned = null;
		deformedSubjectBinned = null;
		
		
		
//Optimizer
		rbf = null;
		localROI = null;
		localCoarseGradient = null;
		coarseLocalRegionCenter = null;
		
//Histogram

		origDeformedSubject = null;
		origTarget = null;
		origJointST = null;

		// used in optimization -- perhaps move to child class
		currentDeformedSubject = null;
		currentJointST = null;
	}
	
	private int levelCount = 0;
	public int[] prepareForNextLevel() {
		System.err.println( "prepareForNextLevel " + levelCount++ );

		
		if ( deformedSubject != null )
		{
			deformedSubject.disposeLocal();
		}
		deformedSubject = subject.returnDeformedCopy(totalDeformField);
		threshAtRobustMaxAndMin(subject);
		threshAtRobustMaxAndMin(target);
		threshAtRobustMaxAndMin(deformedSubject);
		
		if ( defFieldUpdateMode == 1 )
		{			
			if ( deformedSubjectCopy != null )
			{
				deformedSubjectCopy.disposeLocal();
			}
			deformedSubjectCopy = deformedSubject.clone();
		}
		
		// find binned copies of target and deformated subject
		if ( targetBinned != null )
		{
			targetBinned.disposeLocal();
		}
		targetBinned = target.clone();
		targetBinned.rescaleToBins();

		if ( deformedSubjectBinned != null )
		{
			deformedSubjectBinned.disposeLocal();
		}
		deformedSubjectBinned = deformedSubject.clone();
		deformedSubjectBinned.rescaleToBins();
		calculateBoundingBox();
		updateHistograms(targetBinned,deformedSubjectBinned,boundingBox);
		return boundingBox;
	}
	
	private int resCount = 0;
	public void setResolution(float downSampleFactor) {

		int newX, newY, newZ;
		int oldX, oldY, oldZ;
		double sigma = 1.0;

		if (currentDownSampleFactor == downSampleFactor)
			return;

		oldX = subject.getXN();
		oldY = subject.getYN();
		oldZ = subject.getZN();

		currentDownSampleFactor = downSampleFactor;
		double sMin = subject.getMin();
		double sMax = subject.getMax();
		double sIntervals = subject.getIntervals();
		
		double tMin = target.getMin();
		double tMax = target.getMax();
		double tIntervals = target.getIntervals();
		if ( subject != null )
		{
			subject.disposeLocal();
		}
		if ( target != null )
		{
			target.disposeLocal();
		}
		subject=new VabraVolumeCollection(origSubjectList, chInterpType, numOfBins, false);
		target=new VabraVolumeCollection(origTargetList, chInterpType, numOfBins, false);
		subject.setMinMax( sMin, sMax, sIntervals );
		target.setMinMax( tMin, tMax, tIntervals );
		
		

		if (currentDownSampleFactor != 1.0) {
			newX = (int) (subject.getXN() / currentDownSampleFactor);
			newY = (int) (subject.getYN() / currentDownSampleFactor);
			newZ = (int) (subject.getZN() / currentDownSampleFactor);
		} else {
			newX = (subject.getXN());
			newY = (subject.getYN());
			newZ = (subject.getZN());
		}
		subject.downSample(newX, newY, newZ, sigma);
		target.downSample(newX, newY, newZ, sigma);
		if (totalDeformField == null) {
			// setup deformation fields for first time
			totalDeformField = new float[ subject.getXN() * subject.getYN() * subject.getZN() * 3 ];
		} else {
			// resample deformation fields
			float[] totalDeformFieldTemp = new float[ subject.getXN() * subject.getYN() * subject.getZN() * 3 ];
			RegistrationUtilities.DeformationFieldResample3DM(totalDeformField, totalDeformFieldTemp, oldX, oldY, oldZ, newX, newY, newZ);
			totalDeformField = null;
			totalDeformField = totalDeformFieldTemp;
		}
		curX = subject.getXN();
		curY = subject.getYN();
		curZ = subject.getZN();
		System.err.println( "setResolution " + resCount++ );
		/*
		if ( defFieldUpdateMode == 1 )
		{
			if ( currentDeformField != null )
			{
				currentDeformField.disposeLocal(false);
			}
			currentDeformField = new ModelImage( ModelStorageBase.FLOAT, new int[]{subject.getXN(), subject.getYN(), subject.getZN(), 3}, "currentDeformField" );

			for(int c = 0; c < 3; c++){
				if ( totalDeformFieldSplit[c] != null )
				{
					totalDeformFieldSplit[c].disposeLocal(false);
				}
				totalDeformFieldSplit[c] = new ModelImage( ModelStorageBase.FLOAT, new int[]{subject.getXN(), subject.getYN(), subject.getZN()}, "totalDeformFieldSplit" );
				for(int i = 0; i < subject.getXN(); i++)
					for(int j = 0; j < subject.getYN(); j++)
						for(int k = 0; k < subject.getZN(); k++){
							totalDeformFieldSplit[c].set(i, j, k, totalDeformField.getDouble(i, j, k, c));
						}
			}
		}
		*/
	}

	private void threshAtRobustMaxAndMin(VabraVolumeCollection imgVec) {

		float robustMax, robustMin;
		int XN, YN, ZN, i, j, k;

		XN = imgVec.XN;
		YN = imgVec.YN;
		ZN = imgVec.ZN;

		int index;
		int slice = XN * YN;
		if (RobustMaxThresh > 0.00001f) {
			robustMax = robustMaximum(imgVec.data, RobustMaxThresh, 2, XN, YN, ZN);
			for (i = 0; i < XN; i++) for (j = 0; j < YN; j++) for (k = 0; k < ZN; k++) {
				index = k * slice + j * XN + i;
				float val = imgVec.data[index];
				if (val > robustMax) {
					imgVec.data[index] = robustMax;
				}
			}
		}

		if (RobustMinThresh > 0.00001f) {
			robustMin = robustMinimum(imgVec.data, RobustMinThresh, 2, XN, YN, ZN);
			for (i = 0; i < XN; i++) for (j = 0; j < YN; j++) for (k = 0; k < ZN; k++) {
				index = k * slice + j * XN + i;
				float val = imgVec.data[index];
				if (val < robustMin) {
					imgVec.data[index] = robustMin;
				}
			}
		}

		imgVec.calculateMaxAndMinVals();
	}


	private void updateDefField(int[] regionToUpdate){

		double newVec, oldVec, x, y, z;
		int XN = curX;
		int YN = curY;
		int ZN = curZ;
		int components = 3;
		int slice = XN * YN;
		int size = ZN * slice;
		int index;

		for(int i = regionToUpdate[0]; i <= regionToUpdate[1]; i++)
			for(int j = regionToUpdate[2]; j <= regionToUpdate[3]; j++)
				for(int k = regionToUpdate[4]; k <= regionToUpdate[5]; k++){
					index = k * slice + j * XN + i;
					if(currentDeformField[index] != 0 || currentDeformField[size + index] != 0 || currentDeformField[size * 2 + index] != 0 ){
						x = i + currentDeformField[index];
						y = j + currentDeformField[size + index];
						z = k + currentDeformField[size * 2 + index];

						for(int c = 0; c < 3; c++){
							index = c * size + k * slice + j * XN + i;
							oldVec = RegistrationUtilities.Interpolation(totalDeformFieldSplit[c], XN, YN, ZN, x, y, z, 0);
							newVec = currentDeformField[index] + oldVec;
							index = c * size + k * slice + j * XN + i;
							totalDeformField[index] = (float)newVec;
							currentDeformField[index] = 0;
						}
					}
				}

		for(int c = 0; c < components; c++)
		{
			for(int i = regionToUpdate[0]; i <= regionToUpdate[1]; i++)
				for(int j = regionToUpdate[2]; j <= regionToUpdate[3]; j++)
					for(int k = regionToUpdate[4]; k <= regionToUpdate[5]; k++){
						index = c * size + k * slice + j * XN + i;
						//totalDeformFieldSplit[c].set(i, j, k, totalDeformField.getDouble(i, j, k, c));
						totalDeformFieldSplit[c].set(i, j, k, totalDeformField[index]);
					}
		}

	}

	void calculateBoundingBox() {
		int k, j, i, ch;
		int XN, YN, ZN, ext;
		XN = subject.getXN();
		YN = subject.getYN();
		ZN = subject.getZN();
		ext = 5;

		boundingBox[1] = 0;
		boundingBox[0] = subject.getXN();
		boundingBox[3] = 0;
		boundingBox[2] = subject.getYN();
		boundingBox[5] = 0;
		boundingBox[4] = subject.getZN();

		int slice = XN * YN;
		int index;
		float[] sub=deformedSubject.data;
		float[] tar=target.data;
		for (i = 0; i < XN; i++) for (j = 0; j < YN; j++) for (k = 0; k < ZN; k++){
			index = k * slice + j * XN + i;
			if ((Math.abs(sub[index]) > 0.0000001) || (Math.abs(tar[index]) > 0.0000001))
			{
				if (i < boundingBox[0]) boundingBox[0] = i;
				if (i > boundingBox[1]) boundingBox[1] = i;
				if (j < boundingBox[2]) boundingBox[2] = j;
				if (j > boundingBox[3]) boundingBox[3] = j;
				if (k < boundingBox[4]) boundingBox[4] = k;
				if (k > boundingBox[5]) boundingBox[5] = k;
			}
		}

		boundingBox[0]=Math.max(0, boundingBox[0]-ext); 			
		boundingBox[1]=Math.min(XN-1, boundingBox[1]+ext);
		boundingBox[2]=Math.max(0, boundingBox[2]-ext); 			
		boundingBox[3]=Math.min(YN-1, boundingBox[3]+ext);
		boundingBox[4]=Math.max(0, boundingBox[4]-ext); 			
		boundingBox[5]=Math.min(ZN-1, boundingBox[5]+ext);

		maxDimensions = Math.max(Math.max(boundingBox[1]-boundingBox[0], boundingBox[3]-boundingBox[2]), boundingBox[5]-boundingBox[4]);
	}
	
	ModelImage getDeformationField(){
		ModelImage defField = new ModelImage( ModelStorageBase.FLOAT, new int[]{curX, curY, curZ, 3}, origSubjectList.getImageName()+"_def_field");
		try {
			defField.importData( 0, totalDeformField, true );
		} catch (IOException e) {
			e.printStackTrace();
		}
		return defField;
	}

	ModelImage getDeformedSubject(){
		ModelImage origSub= origSubjectList;

		int rows = origSub.getExtents().length > 0 ? origSub.getExtents()[0] : 1;
		int cols = origSub.getExtents().length > 1 ? origSub.getExtents()[1] : 1;
		int slices = origSub.getExtents().length > 2 ? origSub.getExtents()[2] : 1;

		ModelImage defSub = new ModelImage( ModelStorageBase.FLOAT, new int[]{rows,cols,slices}, "d" );

		RegistrationUtilities.DeformImage3D(origSub, defSub, totalDeformField, rows,
				cols, slices, chInterpType); 
		defSub.setImageName(origSubjectList.getImageName() + "_reg");
		defSub.calcMinMax();
		return defSub;
	}
    
    
    
// Optimizer
    private VabraRBF rbf;
    private static double lambda = .350;;//jacobian threshold
    private int[] localROI;
    private double[] localCoarseGradient; // 3 = gradient dimensions
    private int[] coarseLocalRegionCenter;
    private double[] directionsOptmizationWeight;//Directional Constraints
	
    private double domainMax,domainMin,tolerance;


	public double getDomainMax() {
		return domainMax;
	}

	public double getDomainMin() {
		return domainMin;
	}

	public double getDomainTolerance() {
		return tolerance;
	}

	public double getValue(double c) {
		return coarseCostFunction(c);
	}		

	public void coarseGradient(int[] regionCenter, double[] results) {
		double x, y, z;

		int tx_valRBFoff, ty_valRBFoff, tz_valRBFoff;
		int supportRBF[] = new int[6];

		//double nmiOrig;

		int targetBins;
		int subjectBins;

		//Step Size
		double defX, defY, defZ;
		double deltaC[] = new double[3];
		deltaC[0] = 0.1;
		deltaC[1] = 0.1;
		deltaC[2] = 0.1;

		//int counter memflag;
		for ( int i = 0; i < origJointST.length; i++ )
		{
			if ( i < origDeformedSubject.length )
			{
				defSxPlus[i] = origDeformedSubject[i];
				defSxMinus[i] = origDeformedSubject[i];
				defSyPlus[i] = origDeformedSubject[i];
				defSyMinus[i] = origDeformedSubject[i];
				defSzPlus[i] = origDeformedSubject[i];
				defSzMinus[i] = origDeformedSubject[i];
			}
			defSTxPlus[i] = origJointST[i];
			defSTxMinus[i] = origJointST[i];
			defSTyPlus[i] = origJointST[i];
			defSTyMinus[i] = origJointST[i];
			defSTzPlus[i] = origJointST[i];
			defSTzMinus[i] = origJointST[i];
		}

		supportRBF[0] = Math.max(boundingBox[0], regionCenter[0] - rbf.getScale());
		supportRBF[1] = Math.min(boundingBox[1], regionCenter[0] + rbf.getScale());
		supportRBF[2] = Math.max(boundingBox[2], regionCenter[1] - rbf.getScale());
		supportRBF[3] = Math.min(boundingBox[3], regionCenter[1] + rbf.getScale());
		supportRBF[4] = Math.max(boundingBox[4], regionCenter[2] - rbf.getScale());
		supportRBF[5] = Math.min(boundingBox[5], regionCenter[2] + rbf.getScale());

		int XN = curX;
		int YN = curY;
		int ZN = curZ;
		int index;
		int slice = XN * YN;
		int size = XN * YN * ZN;
		
		double testValsD; 
		int testBins;

		int rbfOffset = rbf.getOffset();
		for (int i = supportRBF[0]; i <= supportRBF[1]; i++) 
			for (int j = supportRBF[2]; j <= supportRBF[3]; j++) 
				for (int k = supportRBF[4]; k <= supportRBF[5]; k++) {
					/* coordinates relative to region center */
					tx_valRBFoff = i - regionCenter[0] + rbfOffset;
					ty_valRBFoff = j - regionCenter[1] + rbfOffset;
					tz_valRBFoff = k - regionCenter[2] + rbfOffset;
					if (rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff] != 0) {

						index = k * slice + j * XN + i;
						
						//Calculate rbf at point multiplied by step size

						defX = deltaC[0]*rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff];
						defY = deltaC[1]*rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff];
						defZ = deltaC[2]*rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff];

						//Find values(normalized) of subject and target at the point 
						//subjectBins[ch] = imgSubTarPairs.deformedSubjectBinned.data.getUByte(i,j, k);
						subjectBins = ((short) ((int) deformedSubjectBinned.data[index] & 0xff));
						//System.out.format(subjectBins[ch]+"\n");

						//targetBins[ch] = imgSubTarPairs.targetBinned.data.getUByte(i, j, k);
						targetBins = ((short) ((int) targetBinned.data[index] & 0xff));
						//System.out.format(targetBins[ch]+"\n");

						//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
						if (defFieldUpdateMode == 0){
							x = i + totalDeformField[index];
							y = j + totalDeformField[size + index];
							z = k + totalDeformField[size * 2 + index];

							//Adjusts histograms bins in plus/minus x, y and z directions to find NMI gradient
							//adjustAllGradientBins(subject, x, y, z, defX, defY, defZ, targetBins, subjectBins);
							//adjustGradientBins( subject, x + defX, y, z, defSxPlus, defSTxPlus, targetBins, subjectBins);
							if ((x + defX) < XN && (x + defX) >= 0 && y < YN && y >= 0 && z < ZN && z >= 0) {
								testValsD = RegistrationUtilities.TrilinearInterpolation(subject.data, subject.XN, subject.YN,	subject.ZN, (x + defX), y, z);
								testBins = subject.calculateBin(testValsD);
							} else {
								testBins = 0;
							}
							defSxPlus[testBins] += 1;
							defSxPlus[subjectBins] -= 1;
							defSTxPlus[testBins*numOfBins + targetBins] += 1;
							defSTxPlus[subjectBins*numOfBins + targetBins] -= 1;
							
							
							//adjustGradientBins( subject, x - defX, y, z, defSxMinus, defSTxMinus, targetBins, subjectBins);
							if ((x - defX) < XN && (x - defX) >= 0 && y < YN && y >= 0 && z < ZN && z >= 0) {
								testValsD = RegistrationUtilities.TrilinearInterpolation(subject.data, subject.XN, subject.YN,	subject.ZN, (x - defX), y, z);
								testBins = subject.calculateBin(testValsD);
							} else {
								testBins = 0;
							}
							defSxMinus[testBins] += 1;
							defSxMinus[subjectBins] -= 1;
							defSTxMinus[testBins*numOfBins + targetBins] += 1;
							defSTxMinus[subjectBins*numOfBins + targetBins] -= 1;
							
												
							//adjustGradientBins( subject, x, y + defY, z, defSyPlus, defSTyPlus, targetBins, subjectBins);
							if (x < XN && x >= 0 && (y + defY) < YN && (y + defY) >= 0 && z < ZN && z >= 0) {
								testValsD = RegistrationUtilities.TrilinearInterpolation(subject.data, subject.XN, subject.YN,	subject.ZN, x, y + defY, z);
								testBins = subject.calculateBin(testValsD);
							} else {
								testBins = 0;
							}
							defSyPlus[testBins] += 1;
							defSyPlus[subjectBins] -= 1;
							defSTyPlus[testBins*numOfBins + targetBins] += 1;
							defSTyPlus[subjectBins*numOfBins + targetBins] -= 1;
							
							
							//adjustGradientBins( subject, x, y - defY, z, defSyMinus, defSTyMinus, targetBins, subjectBins);
							if (x < XN && x >= 0 && (y - defY) < YN && (y - defY) >= 0 && z < ZN && z >= 0) {
								testValsD = RegistrationUtilities.TrilinearInterpolation(subject.data, subject.XN, subject.YN,	subject.ZN, x, y - defY, z);
								testBins = subject.calculateBin(testValsD);
							} else {
								testBins = 0;
							}
							defSyMinus[testBins] += 1;
							defSyMinus[subjectBins] -= 1;
							defSTyMinus[testBins*numOfBins + targetBins] += 1;
							defSTyMinus[subjectBins*numOfBins + targetBins] -= 1;
							
							
							//adjustGradientBins( subject, x, y, z + defZ, defSzPlus, defSTzPlus, targetBins, subjectBins);
							if (x < XN && x >= 0 && y < YN && y >= 0 && (z + defZ) < ZN && (z + defZ) >= 0) {
								testValsD = RegistrationUtilities.TrilinearInterpolation(subject.data, subject.XN, subject.YN,	subject.ZN, x, y, (z + defZ));
								testBins = subject.calculateBin(testValsD);
							} else {
								testBins = 0;
							}
							defSzPlus[testBins] += 1;
							defSzPlus[subjectBins] -= 1;
							defSTzPlus[testBins*numOfBins + targetBins] += 1;
							defSTzPlus[subjectBins*numOfBins + targetBins] -= 1;
							
							
							//adjustGradientBins( subject, x, y, z - defZ, defSzMinus, defSTzMinus, targetBins, subjectBins);
							if (x < XN && x >= 0 && y < YN && y >= 0 && (z - defZ) < ZN && (z - defZ) >= 0) {
								testValsD = RegistrationUtilities.TrilinearInterpolation(subject.data, subject.XN, subject.YN,	subject.ZN, x, y, (z - defZ));
								testBins = subject.calculateBin(testValsD);
							} else {
								testBins = 0;
							}
							defSzMinus[testBins] += 1;
							defSzMinus[subjectBins] -= 1;
							defSTzMinus[testBins*numOfBins + targetBins] += 1;
							defSTzMinus[subjectBins*numOfBins + targetBins] -= 1;
						}else{
							x = i;
							y = j;
							z = k;

							//Adjusts histograms bins in plus/minus x, y and z directions to find NMI gradient
							//adjustAllGradientBins(deformedSubject, x, y, z, defX, defY, defZ, targetBins, subjectBins);
						}
					}
				}


		//Find Gradients
		getNMIGradients(results, deltaC);

		//clear out directions not being optimized
		for(int i = 0; i<3;i++)
		{
			results[i]=results[i]*directionsOptmizationWeight[i];
		}
	}
	void updateFromCoarseOptimization(double coeff) {
		//int channels = tarSubImagePairs.numOfCh
		/*double val;
		float x, y, z;
		int i, j, k, ch;
		int ox_valRBFoff, oy_valRBFoff, oz_valRBFoff;*/
		if (Math.abs(coeff) < 0.00001) return;


		double maxJacob = Math.abs(coeff)* rbf.getMaxValuesChange()
		* (Math.max(Math.abs(localCoarseGradient[0]), 
				Math.max(Math.abs(localCoarseGradient[1]), Math.abs(localCoarseGradient[2]))));
		if (maxJacob > lambda) return;

		coarseCostFunctionCommit(coeff, true);

	}

//	should return change in cost function due to optimization
	
	public double coarseOptimize(int[] regionCenter, double[] gradient) {
		int gradParams = coarseGradientParameters();

		localROI[0] = Math.max(boundingBox[0], regionCenter[0] - rbf.getScale());
		localROI[1] = Math.min(boundingBox[1], regionCenter[0] + rbf.getScale());
		localROI[2] = Math.max(boundingBox[2], regionCenter[1] - rbf.getScale());
		localROI[3] = Math.min(boundingBox[3], regionCenter[1] + rbf.getScale());
		localROI[4] = Math.max(boundingBox[4], regionCenter[2] - rbf.getScale());
		localROI[5] = Math.min(boundingBox[5], regionCenter[2] + rbf.getScale());

		// do I use the old gradient or calculate a new in since things may have
		// changed
		coarseGradient(regionCenter, localCoarseGradient);
		RegistrationUtilities.VectorNormalization(localCoarseGradient, gradParams);

		coarseLocalRegionCenter[0] = regionCenter[0];
		coarseLocalRegionCenter[1] = regionCenter[1];
		coarseLocalRegionCenter[2] = regionCenter[2];

		double originalNMI = coarseCostFunction(0);
		double delta = 0;
		double optimizedCoeff = 0;
		double optimizedNMI = 0;

		if(originalNMI != -2){ //skip if already perfectly registered

			double maxCoeff = lambda/(rbf.getMaxValuesChange() * (Math.max(Math.abs(localCoarseGradient[0]), 
					Math.max(Math.abs(localCoarseGradient[1]), Math.abs(localCoarseGradient[2])))));
			
			domainMax = maxCoeff;
			domainMin = -maxCoeff;
			tolerance = 5.0e-5;
			
			double[] X = new double[2];
			Brent.search( domainMin, domainMin, domainMax, X, this, tolerance );
			optimizedCoeff = X[0];
			optimizedNMI = X[1];
			//optimizedNMI = coarseCostFunction(optimizedCoeff);
			
			//Optimizer1DContinuous opt = new BrentMethod1D();
			//opt.initialize(this);		
			//opt.optimize(true);
			//optimizedCoeff = opt.getExtrema();
			//optimizedNMI = coarseCostFunction(optimizedCoeff);
			delta = (optimizedNMI) - originalNMI;
		}
		//System.out.format("**************************Coeff: "+optimizedCoeff[0]+"***************\n");
		if (delta < 0 && Math.abs(optimizedCoeff) >= 0.005) {
			//System.out.println(getClass().getCanonicalName()+"\t"+"UPDATE FROM COARSE OPTIMIZATION "+Math.abs(optimizedCoeff[0]));
			//System.out.format("At ("+coarseLocalRegionCenter[0]+","+coarseLocalRegionCenter[1]+","+coarseLocalRegionCenter[2]+") Coeff:"+optimizedCoeff+" NMI:"+ optimizedNMI +"\n");
			updateFromCoarseOptimization(optimizedCoeff);
			//System.out.format("New:" +coarseCostFunction(0,false)+"\n");
			// System.out.format("accepted\n");
		} else {
			//System.out.format("SHOULD NOT OPTIMIZE %f<0 && %f>=0.005\n",delta,Math.abs(optimizedCoeff[0]));
			delta = 0;
		}
		return delta;
	}


	private int commitCount = 0;
	private double coarseCostFunctionCommit(double coeff, boolean commitUpdate) {
		//System.err.println( "coarseCostFunctionCommit " + commitCount++ );
		
		double rbfVal;
		double coeff_x, coeff_y, coeff_z;

		int ox_valRBFoff, oy_valRBFoff, oz_valRBFoff;
		double defX, defY, defZ;
		double tlambda;
		int targetBins;
		int subjectBins;
		int testBin;
		double interpValsD;
		//int i, j, k, ch;
		double x, y, z;
		VabraVolumeCollection referenceSubject;


		//temp start
		int XN = curX;
		int YN = curY;
		int ZN = curZ;
		//temp end

		resetCurrentHistograms();
		if(coeff == 0) return -getCurrentNMI(); 

		tlambda = (Math.round((double) coeff * 10000.0)) / 10000.0;

		coeff_x = ((double) tlambda) * localCoarseGradient[0];
		coeff_y = ((double) tlambda) * localCoarseGradient[1];
		coeff_z = ((double) tlambda) * localCoarseGradient[2];

		int index;
		int slice = XN * YN;
		int size = XN * YN * ZN;

		int rbfOffset = rbf.getOffset();
		//optimized by putting outside and incrementing.
		for (int i = localROI[0]; i <= localROI[1]; i++) {
			for (int j = localROI[2]; j <= localROI[3]; j++) {
				for (int k = localROI[4]; k <= localROI[5]; k++) {

					//(ox, oy, oz) are coordinates of (i, j) relative to region center				
					ox_valRBFoff = i - coarseLocalRegionCenter[0] + rbfOffset;
					oy_valRBFoff = j - coarseLocalRegionCenter[1] + rbfOffset;
					oz_valRBFoff = k - coarseLocalRegionCenter[2] + rbfOffset;

					rbfVal = rbf.values[ox_valRBFoff][oy_valRBFoff][oz_valRBFoff];

					if (rbfVal != 0) {
						// steepest descent direction: negative of gradient of NMI wrt c.

						//Amount to adjust with RBF if not constrained
						defX = directionsOptmizationWeight[0]*coeff_x*rbfVal;
						defY = directionsOptmizationWeight[1]*coeff_y*rbfVal;
						defZ = directionsOptmizationWeight[2]*coeff_z*rbfVal;
						

						index = k * slice + j * XN + i;
						
						//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
						if (defFieldUpdateMode == 0){
							x = i + totalDeformField[index];
							y = j + totalDeformField[size + index];
							z = k + totalDeformField[size * 2 + index];
							referenceSubject = subject;	
						}else{
							x = i;
							y = j;
							z = k;
							referenceSubject = deformedSubjectCopy;
						}

						//subjectBins[ch] = imgSubTarPairs.deformedSubjectBinned.data.getUByte(i,j, k);
						subjectBins = ((short) ((int) deformedSubjectBinned.data[index] & 0xff));
						
						//interpValsD = RegistrationUtilities.Interpolation(referenceSubject.data, XN, YN, ZN, x + defX, y + defY,z + defZ, imgSubTarPairs.chInterpType);
						interpValsD = RegistrationUtilities.TrilinearInterpolation(referenceSubject.data, XN, YN, ZN, x + defX, y + defY,z + defZ);
						// TODO ditch this fcn call:
						testBin = referenceSubject.calculateBin(interpValsD);
						//if(coeff == 0 && subjectBins[ch] != testBin[ch]) System.out.format(subjectBins[ch]+" "+interpValsD[ch]+" "+ testBin[ch]+"\n");

						//targetBins[ch] = imgSubTarPairs.targetBinned.data.getUByte(i, j, k);
						targetBins = ((short) ((int) targetBinned.data[index] & 0xff));

						//System.out.format("interp"+interpValsD[0]+"submax"+referenceSubject.maxValsD[0]+"\n");
						//System.out.format("sub"+subjectBins[0]+"test"+testBin[0]+"tar"+targetBins[0]+"\n");
						currentDeformedSubject[testBin] += 1;
						currentDeformedSubject[subjectBins] -= 1;
						currentJointST[testBin*numOfBins + targetBins] += 1;
						currentJointST[subjectBins*numOfBins + targetBins] -= 1;
						//set as current deformation field if actually updating
						if(commitUpdate){			
							//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
							if (defFieldUpdateMode == 0){
								totalDeformField[index] += (float)defX;
								totalDeformField[size + index] += defY;
								totalDeformField[size * 2 + index] += (float)defZ;
							}else{
								currentDeformField[index] = (float)defX;
								currentDeformField[size + index] = (float)defY;
								currentDeformField[size * 2 + index] = (float)defZ;
							}

							//change images and real histograms if actually updating
							deformedSubject.data[index] = (float) interpValsD;
							deformedSubjectBinned.data[index] = testBin;
								
							adjustOrigBins(subjectBins, targetBins,testBin);

						}

					}
				}
			}
		}

		//Update copies and final deformation field if actually updating
		if(commitUpdate){
			//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
			//System.out.format("Before:"+ -imgSubTarPairs.hist.getMNMI(imgSubTarPairs.hist.currentDeformedSubject, imgSubTarPairs.hist.origTarget, imgSubTarPairs.hist.currentJointST)+"\n");
			//System.out.format("After:"+ -imgSubTarPairs.hist.getMNMI(imgSubTarPairs.hist.currentDeformedSubject, imgSubTarPairs.hist.origTarget, imgSubTarPairs.hist.currentJointST)+"\n");
			if (defFieldUpdateMode == 1){
				for (int i = localROI[0]; i <= localROI[1]; i++) 
					for (int j = localROI[2]; j <= localROI[3]; j++) 
						for (int k = localROI[4]; k <= localROI[5]; k++) {
							index = k * slice + j * XN + i;
							deformedSubjectCopy.data[index] = deformedSubject.data[index];
						}

				updateDefField(localROI);
			}

		}

	
		//for (int ch = 0; ch < imgSubTarPairs.numOfCh; ch++){
			//nmiValD -= imgSubTarPairs.chWeights[ch] * RegistrationUtilities.NMI(imgSubTarPairs.hist.currentDeformedSubject, imgSubTarPairs.hist.origTarget,
				//	imgSubTarPairs.hist.currentJointST, ch, imgSubTarPairs.numOfBins);
			//if(commitUpdate)System.out.format("Committing" + imgSubTarPairs.hist.getCurrentNMI() + "\n");
			//if(commitUpdate)System.out.format("Committed:" + imgSubTarPairs.hist.getOrigNMI() + "\n");
		//}
		//System.out.format("COARSE COST FUNC %f %f\n",lambda[0],nmiVal);
		return -getCurrentNMI();
	}


	//public float coarseCostFunction(float[] lambda) {
	private double coarseCostFunction(double coeff) {
		double rbfVal;
		double coeff_x, coeff_y, coeff_z;

		int ox_valRBFoff, oy_valRBFoff, oz_valRBFoff;
		double defX, defY, defZ;
		double tlambda;
		int targetBins;
		int subjectBins;
		int testBin;
		double interpValsD;
		//int i, j, k, ch;
		double x, y, z;
		VabraVolumeCollection referenceSubject;


		//temp start
		int XN = curX;
		int YN = curY;
		int ZN = curZ;
		//temp end

		if(coeff == 0) return -(RegistrationUtilities.NMI(origDeformedSubject, origTarget, origJointST, numOfBins)); 
		
		for ( int i = 0; i < origJointST.length; i++ )
		{
			if ( i < origDeformedSubject.length )
			{
				currentDeformedSubject[i] = origDeformedSubject[i];
			}
			currentJointST[i] = origJointST[i];
		}

		tlambda = (Math.round((double) coeff * 10000.0)) / 10000.0;

		coeff_x =  directionsOptmizationWeight[0]*((double) tlambda) * localCoarseGradient[0];
		coeff_y =  directionsOptmizationWeight[1]*((double) tlambda) * localCoarseGradient[1];
		coeff_z =  directionsOptmizationWeight[2]*((double) tlambda) * localCoarseGradient[2];

		int index;
		int slice = XN * YN;
		int size = XN * YN * ZN;
		int size2 = 2* XN * YN * ZN;
		
		int rbfOffset = rbf.getOffset();
		//optimized by putting outside and incrementing.
		for (int i = localROI[0]; i <= localROI[1]; i++) {
			for (int j = localROI[2]; j <= localROI[3]; j++) {
				for (int k = localROI[4]; k <= localROI[5]; k++) {

					//(ox, oy, oz) are coordinates of (i, j) relative to region center				
					ox_valRBFoff = i - coarseLocalRegionCenter[0] + rbfOffset;
					oy_valRBFoff = j - coarseLocalRegionCenter[1] + rbfOffset;
					oz_valRBFoff = k - coarseLocalRegionCenter[2] + rbfOffset;

					rbfVal = rbf.values[ox_valRBFoff][oy_valRBFoff][oz_valRBFoff];

					if (rbfVal != 0) {
						// steepest descent direction: negative of gradient of NMI wrt c.

						//Amount to adjust with RBF if not constrained
						defX = coeff_x*rbfVal;
						defY = coeff_y*rbfVal;
						defZ = coeff_z*rbfVal;
						

						index = k * slice + j * XN + i;
						
						//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
						if (defFieldUpdateMode == 0){
							x = i + totalDeformField[index];
							y = j + totalDeformField[size + index];
							z = k + totalDeformField[size2 + index];
							referenceSubject = subject;	
						}else{
							x = i;
							y = j;
							z = k;
							referenceSubject = deformedSubjectCopy;
						}

						//subjectBins[ch] = imgSubTarPairs.deformedSubjectBinned.data.getUByte(i,j, k);
						subjectBins = ((short) ((int) deformedSubjectBinned.data[index] & 0xff));
						
						//interpValsD = RegistrationUtilities.Interpolation(referenceSubject.data, XN, YN, ZN, x + defX, y + defY,z + defZ, imgSubTarPairs.chInterpType);
						interpValsD = RegistrationUtilities.TrilinearInterpolation(referenceSubject.data, XN, YN, ZN, x + defX, y + defY,z + defZ);
						// TODO ditch this fcn call:
						testBin = (int)((interpValsD - referenceSubject.minValsD) / referenceSubject.intervalsD);
						//testBin = referenceSubject.calculateBin(interpValsD);
						//if(coeff == 0 && subjectBins[ch] != testBin[ch]) System.out.format(subjectBins[ch]+" "+interpValsD[ch]+" "+ testBin[ch]+"\n");

						if ( testBin != subjectBins )
						{
							//targetBins[ch] = imgSubTarPairs.targetBinned.data.getUByte(i, j, k);
							targetBins = ((short) ((int) targetBinned.data[index] & 0xff));

							currentDeformedSubject[testBin] += 1;
							currentDeformedSubject[subjectBins] -= 1;
							currentJointST[testBin*numOfBins + targetBins] += 1;
							currentJointST[subjectBins*numOfBins + targetBins] -= 1;
						}
					}
				}
			}
		}
	
		return -getCurrentNMI();
	}

	public VabraRBF getRBF(){
		return rbf;
	}
	
	
	
	
	
	
	
//Histogram
	private int[] origJointST;
	private int[] currentJointST;
	private int[] defSTxPlus,defSTxMinus,defSTyPlus,defSTyMinus,defSTzPlus,defSTzMinus;
	private int[] defSxPlus, defSxMinus, defSyPlus, defSyMinus, defSzPlus, defSzMinus;
	private int[] origDeformedSubject, origTarget;
	private int[] currentDeformedSubject;
	


	private int[] copyHist(int[] in){
		
		int[] copy = new int[in.length];
		
		for(int i=0; i < in.length; i++){
			copy[i] = in[i];
			
		}
		return copy;
	}
	
	private int[][] copyHist(int[][] in){
		
		int[][] copy = new int[in.length][in[0].length];
		
		for(int i=0; i < in.length; i++){
			for(int j=0; j < in[0].length; j++){
				copy[i][j] = in[i][j];
			}
		}
		return copy;
	}
	
	private int[][][] copyHist(int[][][] vol) {
		int rows = vol.length;
		int cols = vol[0].length;
		int slices = vol[0][0].length;
		int[][][] copy = new int[rows][cols][slices];
		for (int i = 0; i < rows; i++) {
			for (int j = 0; j < cols; j++) {
				for (int k = 0; k < slices; k++) {
					copy[i][j][k] = vol[i][j][k];
				}
			}
		}
		return copy;
	}
	
	private void resetCurrentHistograms() {
		for ( int i = 0; i < origJointST.length; i++ )
		{
			if ( i < origDeformedSubject.length )
			{
				currentDeformedSubject[i] = origDeformedSubject[i];
			}
			currentJointST[i] = origJointST[i];
		}
	}

	private void updateHistograms(VabraVolumeCollection normedTarget, VabraVolumeCollection normedDeformedSubject, int[] boundingBox) {
	    // System.out.println(getClass().getCanonicalName()+"\t"+"UPDATE HISTOGRAMS ");
		if (origDeformedSubject == null) {
			System.out.format("null Histograms");
			// initializeHistograms();
		}
		RegistrationUtilities.Histogram3D(normedDeformedSubject.data, numOfBins, boundingBox, origDeformedSubject, normedDeformedSubject.XN, normedDeformedSubject.YN, normedDeformedSubject.ZN );
		RegistrationUtilities.Histogram3D(normedTarget.data, numOfBins, boundingBox, origTarget, normedTarget.XN, normedTarget.YN, normedTarget.ZN);
		RegistrationUtilities.JointHistogram3D(normedDeformedSubject.data,normedTarget.data, 
				numOfBins, boundingBox, origJointST, normedTarget.XN, normedTarget.YN, normedTarget.ZN);
	}
	
	private double getCurrentNMI(){

		return (RegistrationUtilities.NMI(currentDeformedSubject, origTarget, currentJointST, numOfBins)); 
	}
		
	private void adjustOrigBins(int subBin, int tarBin, int newBin){
		adjustBins(origDeformedSubject, origJointST, subBin, tarBin, newBin);
	}
	
	/*
	private void adjustAllGradientBins(VabraVolumeCollection subject, double origX, double origY, double origZ,  double defX, double defY, double defZ, int targetBins, int subjectBins){

		adjustGradientBins( subject, origX + defX, origY, origZ, defSxPlus, defSTxPlus, targetBins, subjectBins);
		adjustGradientBins( subject, origX - defX, origY, origZ, defSxMinus, defSTxMinus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY + defY, origZ, defSyPlus, defSTyPlus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY - defY, origZ, defSyMinus, defSTyMinus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY, origZ + defZ, defSzPlus, defSTzPlus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY, origZ - defZ, defSzMinus, defSTzMinus, targetBins, subjectBins);
		
	}*/
	
	private void getNMIGradients(double[] results, double[] deltaC){
		results[0] = (RegistrationUtilities.NMI(defSxPlus, origTarget,defSTxPlus, numOfBins)
					- RegistrationUtilities.NMI(defSxMinus,origTarget, defSTxMinus, numOfBins))
					/ (2.0f * deltaC[0]);
		results[1] = (RegistrationUtilities.NMI(defSyPlus, origTarget, defSTyPlus, numOfBins)
					- RegistrationUtilities.NMI(defSyMinus, origTarget, defSTyMinus, numOfBins))
					/ (2.0f * deltaC[1]);
		results[2] = (RegistrationUtilities.NMI(defSzPlus, origTarget, defSTzPlus, numOfBins)
					- RegistrationUtilities.NMI(defSzMinus, origTarget, defSTzMinus, numOfBins))
					/ (2.0f * deltaC[2]);
	}
	
	
	private void adjustGradientBins(VabraVolumeCollection subject,double x,double y,double z, int[] subjectHist,int[] jointHist, int targetBins, int subjectBins){
		
		double testValsD; 
		int testBins;
		
		if (x < subject.getXN() && x >= 0 && y < subject.getYN() && y >= 0 && z < subject.getZN() && z >= 0) {
			testValsD = subject.interpolate(x, y, z);
		} else {
			testValsD = subject.minValsD;
		}

		testBins = subject.calculateBin(testValsD);
		adjustBins(subjectHist, jointHist, subjectBins,targetBins,testBins);

	}
	

	private void adjustBins(int[] subjectHist, int[] jointHist, int subBin, int tarBin, int newBin){
		subjectHist[newBin] += 1;
		subjectHist[subBin] -= 1;
		jointHist[newBin*numOfBins + tarBin] += 1;
		jointHist[subBin*numOfBins + tarBin] -= 1;
	}

	@Override
	public double eval(double arg0) {
		return coarseCostFunction(arg0);
	}	
	
	
}

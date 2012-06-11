package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


public class VabraSubjectTargetPairs {

	protected VabraHistograms hist;

	protected int maxDimensions;
	
	protected VabraVolumeCollection subject;
	protected VabraVolumeCollection deformedSubject;
	protected VabraVolumeCollection target;

	protected VabraVolumeCollection targetBinned;
	protected VabraVolumeCollection deformedSubjectBinned;

	protected double currentDownSampleFactor;

	protected int boundingBox[] = new int[6];

	protected ModelImage currentDeformField; //deformation field 
	//protected float[][][][] currentDeformFieldM; //deformation field as an array for easy access
	protected float[] totalDeformField; //deformation field 
	//protected float[][][][] totalDeformFieldM; //deformation field as an array for easy access

	protected ModelImage origSubjectList;
	protected ModelImage origTargetList;
	
	protected int chInterpType;
	protected int numOfBins;
	protected int numOfSub;
	protected int numOfTar;
	protected float RobustMinThresh, RobustMaxThresh;
	protected int defFieldUpdateMode;
	public int curX, curY, curZ;
	
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
		if ( currentDeformField != null )
		{
			currentDeformField.disposeLocal();
			currentDeformField = null;
		}
		
		origSubjectList=null;
		origTargetList=null;
		targetBinned = null;
		deformedSubjectBinned = null;
		hist.dispose();
	}

	public VabraSubjectTargetPairs(ModelImage subjectVols, ModelImage targetVols, 
			float robustMaxT, float robustMinT, int numBins, int interpType,boolean useMNMI, int defFieldUpdateMode) {
		this.numOfBins = numBins;
		this.origSubjectList=subjectVols;
		this.origTargetList=targetVols;
		this.chInterpType = interpType;
		this.subject=new VabraVolumeCollection(subjectVols, chInterpType, numOfBins, true);
		deformedSubject=this.subject.clone();
		this.target=new VabraVolumeCollection(targetVols, chInterpType, numOfBins, true);
		this.defFieldUpdateMode = defFieldUpdateMode;

		numOfSub = 1;
		numOfTar = 1;


		curX = subjectVols.getExtents().length > 0 ? subjectVols.getExtents()[0] : 1;
		curY = subjectVols.getExtents().length > 1 ? subjectVols.getExtents()[1] : 1;
		curZ = subjectVols.getExtents().length > 2 ? subjectVols.getExtents()[2] : 1;

		calculateBoundingBox();

		RobustMinThresh = robustMinT;
		RobustMaxThresh = robustMaxT;



		setAlltoRobustHist();
		//normalizeImagesToMax();

		//incrementCompletedUnits();
		targetBinned = target.clone();
		targetBinned.rescaleToBins();
		
		hist = new VabraHistogramsMultCh(numOfSub, numOfTar, numOfBins);
		//if(useMNMI)hist = new VabraHistogramsMNMI(numOfSub, numOfTar, numOfBins);
		//else hist = new VabraHistogramsMultCh(numOfSub, numOfTar, numOfBins);
		
		//incrementCompletedUnits();

		totalDeformFieldSplit = new ModelImage[3];


		return;
	}

	//public void calculateMaxAndMinVals(){
		
	//	subject.calculateMaxAndMinVals();
	//	deformedSubject.calculateMaxAndMinVals();
	//	target.calculateMaxAndMinVals();

		
	//}
	
	public void threshAtRobustMaxAndMin(VabraVolumeCollection imgVec) {

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

	public void setAlltoRobustHist() {
		threshAtRobustMaxAndMin(subject);
		threshAtRobustMaxAndMin(target);
		threshAtRobustMaxAndMin(deformedSubject);
	}

	/*
	public void normalizeImagesToMax() {
		for (int ch = 0; ch < numOfSub; ch++) {
			ImageDataMath.normalizeToUnitIntensity(subject.data[ch]);
			ImageDataMath.scaleFloatValue(subject.data[ch],(float) maxValsD);

			ImageDataMath.normalizeToUnitIntensity(deformedSubject.data[ch]);
			ImageDataMath.scaleFloatValue(deformedSubject.data[ch], (float) maxValsD);
		}
		for (int ch = 0; ch < numOfTar; ch++) {
			ImageDataMath.normalizeToUnitIntensity(target.data[ch]);
			ImageDataMath.scaleFloatValue(target.data[ch], (float) maxValsD);
		}
	}
	 */

	public void prepareForNextLevel() {

		if ( deformedSubject != null )
		{
			deformedSubject.disposeLocal();
		}
		deformedSubject = subject.returnDeformedCopy(totalDeformField);
		setAlltoRobustHist();
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
		hist.updateHistograms(targetBinned,deformedSubjectBinned,boundingBox);
	}

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
		reintializeFromFile();

		if (currentDownSampleFactor != 1.0) {
			newX = (int) (subject.getXN() / currentDownSampleFactor);
			newY = (int) (subject.getYN() / currentDownSampleFactor);
			newZ = (int) (subject.getZN() / currentDownSampleFactor);
		} else {
			newX = (int) (subject.getXN());
			newY = (int) (subject.getYN());
			newZ = (int) (subject.getZN());
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

	VabraVolumeCollection deformedSubjectCopy;
	ModelImage[]  totalDeformFieldSplit;
	
	public void updateDefField(){
		int[] regionToUpdate = new int[6];
		regionToUpdate[0] = 0;
		regionToUpdate[1] = subject.getXN() -1;
		regionToUpdate[2] = 0;
		regionToUpdate[3] = subject.getYN() -1;
		regionToUpdate[4] = 0;
		regionToUpdate[5] = subject.getZN() -1;
		updateDefField(regionToUpdate);
	}
	
	public void updateDefField(int[] regionToUpdate){

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
					if(currentDeformField.getFloat(i,j,k,0) != 0 || currentDeformField.getFloat(i,j,k,1) != 0 || currentDeformField.getFloat(i,j,k,2) != 0 ){
						x = i + currentDeformField.getFloat(i,j,k,0);
						y = j + currentDeformField.getFloat(i,j,k,1);
						z = k + currentDeformField.getFloat(i,j,k,2);

						for(int c = 0; c < 3; c++){
							oldVec = RegistrationUtilities.Interpolation(totalDeformFieldSplit[c], XN, YN, ZN, x, y, z, 0);
							newVec = currentDeformField.getFloat(i,j,k,c) + oldVec;
							//totalDeformField.set(i, j, k, c, newVec);
							index = c * size + k * slice + j * XN + i;
							totalDeformField[index] = (float)newVec;
							currentDeformField.set(i,j,k,c, 0);
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
	

	public int coarseGradientParameters() {
		return 3; 
	}

	void reintializeFromFile() {
		double sMin = this.subject.getMin();
		double sMax = this.subject.getMax();
		double sIntervals = this.subject.getIntervals();
		
		double tMin = this.target.getMin();
		double tMax = this.target.getMax();
		double tIntervals = this.target.getIntervals();
		if ( this.subject != null )
		{
			this.subject.disposeLocal();
		}
		if ( this.target != null )
		{
			this.target.disposeLocal();
		}
		this.subject=new VabraVolumeCollection(origSubjectList, chInterpType, numOfBins, false);
		this.target=new VabraVolumeCollection(origTargetList, chInterpType, numOfBins, false);
		this.subject.setMinMax( sMin, sMax, sIntervals );
		this.target.setMinMax( tMin, tMax, tIntervals );
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


	ModelImage getDeformedSubject(){
		ModelImage origSub= (ModelImage)origSubjectList;

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

	ModelImage getDeformationField(){
		ModelImage defField = new ModelImage( ModelStorageBase.FLOAT, new int[]{curX, curY, curZ, 3}, origSubjectList.getImageName()+"_def_field");
		try {
			defField.importData( 0, totalDeformField, true );
		} catch (IOException e) {
			e.printStackTrace();
		}
		return defField;
	}
	
	public int[] getBoundingBox() {
		calculateBoundingBox();
		return boundingBox;
	}

	/**
     *    Robust maximum estimation
     *    @param 	ratio	float fraction in [0,1]: the minimum number of points below or equal to the minimum over the total volume
	 *    @param 	scales	int: the number of times the scale is refined for finding the robust minimum
	 *	  @return 			the robust minimum value	
     */
    public static final float robustMinimum(float[] image, float ratio, int scales, int nx, int ny, int nz ) {
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
					&&(val <= Imin + 1.0f/(float)Nbins*(Imax-Imin) ) ) bins[0]++;
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
			Rmin = Imin + (float)(n-0.5f)/(float)Nbins*(Imax-Imin);
		
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

    /**
     *    Robust maximum estimation
     *    @param 	ratio	float fraction in [0,1]: the minimum number of points above or equal to the maximum over the total volume
	 *    @param 	scales	int: the number of times the scale is refined for finding the robust maximum
	 *	  @param	nx,ny,nz	image dimensions
	 *	  @return 			the robust maximum value	
     */
    public static final float robustMaximum(float[] image, float ratio, int scales, int nx, int ny, int nz ) {
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
					&&(val <= Imin + 1.0f/(float)Nbins*(Imax-Imin) ) ) bins[0]++;
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
			Rmax = Imin + (float)(n+0.5f)/(float)Nbins*(Imax-Imin);	

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
}

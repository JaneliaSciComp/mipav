package gov.nih.mipav.model.algorithms.registration.vabra;

import gov.nih.mipav.view.MipavUtil;

public class VabraOptimizer{

	VabraOptimizer globalPtr;
	VabraSubjectTargetPairs imgSubTarPairs;
	VabraRBF rbf;

	static double lambda = .350;;//jacobian threshold
	int[] localROI;
	double[] localCoarseGradient; // 3 = gradient dimensions
	int[] coarseLocalRegionCenter;
	int[][] fineLocalRegionCenters;
	double[] directionsOptmizationWeight;//Directional Constraints

	//Determines the way the deformation field is updated. 
	//Mode 0 is the original ABA mode where the fields are updated through summation. 
	//Mode 1 reapply the deformation field with each update and registers using the new deformed image;
	//See https://putter.ece.jhu.edu/IACL:MinProjectLogs_VABRA_DefFieldUpdate for details.
	int defFieldUpdateMode;


	public VabraOptimizer(VabraSubjectTargetPairs imgSubTarPairs, double[] directionsOptmizationWeight, int defFieldUpdateMode) {
		this.directionsOptmizationWeight = directionsOptmizationWeight;
		this.imgSubTarPairs = imgSubTarPairs;
		this.defFieldUpdateMode = defFieldUpdateMode;

		globalPtr = this;
		localROI = new int[6];
		rbf = new VabraRBF();
		localCoarseGradient = new double[3];
		coarseLocalRegionCenter = new int[3];


	}

	public void dispose() {

		rbf = null;
		globalPtr = null;
		imgSubTarPairs = null;
		localROI = null;
		localCoarseGradient = null;
		coarseLocalRegionCenter = null;

	}

	public class CoarseOptimizer implements Optimizable1DContinuous
	{

		double domainMax,domainMin,tolerance;

		public CoarseOptimizer(double max, double min) {

			domainMax = max;
			domainMin = min;
			tolerance = 5.0e-5;
		}

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
			return globalPtr.coarseCostFunction(c,false);
		}		
	}

	public void coarseGradient(int[] regionCenter, double[] results) {
		double x, y, z;
		VabraVolumeCollection referenceSubject;

		int tx_valRBFoff, ty_valRBFoff, tz_valRBFoff;
		int supportRBF[] = new int[6];

		//double nmiOrig;

		int targetBins[] = new int[imgSubTarPairs.numOfTar];
		int subjectBins[] = new int[imgSubTarPairs.numOfSub];

		//Step Size
		double defX, defY, defZ;
		double deltaC[] = new double[3];
		deltaC[0] = 0.1;
		deltaC[1] = 0.1;
		deltaC[2] = 0.1;

		int TimeCounter = 0;

		long start, stop;

		//int counter memflag;
		imgSubTarPairs.hist.copyOrigHistograms();

		//nmiOrig = imgSubTarPairs.hist.getOrigNMI();

		supportRBF[0] = Math.max(imgSubTarPairs.boundingBox[0], regionCenter[0] - rbf.getScale());
		supportRBF[1] = Math.min(imgSubTarPairs.boundingBox[1], regionCenter[0] + rbf.getScale());
		supportRBF[2] = Math.max(imgSubTarPairs.boundingBox[2], regionCenter[1] - rbf.getScale());
		supportRBF[3] = Math.min(imgSubTarPairs.boundingBox[3], regionCenter[1] + rbf.getScale());
		supportRBF[4] = Math.max(imgSubTarPairs.boundingBox[4], regionCenter[2] - rbf.getScale());
		supportRBF[5] = Math.min(imgSubTarPairs.boundingBox[5], regionCenter[2] + rbf.getScale());





		start = System.currentTimeMillis();
		for (int i = supportRBF[0]; i <= supportRBF[1]; i++) 
			for (int j = supportRBF[2]; j <= supportRBF[3]; j++) 
				for (int k = supportRBF[4]; k <= supportRBF[5]; k++) {
					/* coordinates relative to region center */
					tx_valRBFoff = i - regionCenter[0] + rbf.getOffset();
					ty_valRBFoff = j - regionCenter[1] + rbf.getOffset();
					tz_valRBFoff = k - regionCenter[2] + rbf.getOffset();
					if (rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff] != 0) {

						//Calculate rbf at point multiplied by step size

						defX = deltaC[0]*rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff];
						defY = deltaC[1]*rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff];
						defZ = deltaC[2]*rbf.values[tx_valRBFoff][ty_valRBFoff][tz_valRBFoff];

						//Find values(normalized) of subject and target at the point 
						for (int ch = 0; ch < imgSubTarPairs.numOfSub; ch++) {		
							subjectBins[ch] = imgSubTarPairs.deformedSubjectBinned.data[ch].getUByte(i,j, k);
							//subjectBins[ch] = (subjectBins[ch]<0?0:subjectBins[ch]>255?255:subjectBins[ch]);					
							//System.out.format(subjectBins[ch]+"\n");
						}
						for (int ch = 0; ch < imgSubTarPairs.numOfTar; ch++) {	
							targetBins[ch] = imgSubTarPairs.targetBinned.data[ch].getUByte(i, j, k);
							//targetBins[ch] = (targetBins[ch]<0?0:targetBins[ch]>255?255:targetBins[ch]);
							//System.out.format(targetBins[ch]+"\n");
						}

						//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
						if (defFieldUpdateMode == 0){
							x = i + imgSubTarPairs.totalDeformField.getFloat(i, j, k, 0);
							y = j + imgSubTarPairs.totalDeformField.getFloat(i, j, k, 1);
							z = k + imgSubTarPairs.totalDeformField.getFloat(i, j, k, 2);
							referenceSubject = imgSubTarPairs.subject;	
						}else{
							x = i;
							y = j;
							z = k;
							referenceSubject = imgSubTarPairs.deformedSubject;
						}

						//Adjusts histograms bins in plus/minus x, y and z directions to find NMI gradient
						imgSubTarPairs.hist.adjustAllGradientBins(referenceSubject, x, y, z, defX, defY, defZ, targetBins, subjectBins);
					}
				}


		//Find Gradients
		imgSubTarPairs.hist.getNMIGradients(results, deltaC);

		//clear out directions not being optimized
		for(int i = 0; i<3;i++)results[i]=results[i]*directionsOptmizationWeight[i];
		
		stop = System.currentTimeMillis();
		if ((float) (stop - start) > 1000) {
			System.out.format("Coarse Gradient time:%f, %d\n", (float) (stop - start), TimeCounter);
			System.out.format("%f/%f\n", (float) MipavUtil.getFreeHeapMemory(), (float) MipavUtil.getMaxHeapMemory());
			TimeCounter = 0;
		} else TimeCounter++;

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

		coarseCostFunction(coeff, true);

	}



	//ModelImage[]  totalDeformFieldSplit = new ModelImage[3];
//	should return change in cost function due to optimization
	public double coarseOptimize(int[] regionCenter, double[] gradient) {
		int gradParams = imgSubTarPairs.coarseGradientParameters();

		localROI[0] = Math.max(imgSubTarPairs.boundingBox[0], regionCenter[0] - rbf.getScale());
		localROI[1] = Math.min(imgSubTarPairs.boundingBox[1], regionCenter[0] + rbf.getScale());
		localROI[2] = Math.max(imgSubTarPairs.boundingBox[2], regionCenter[1] - rbf.getScale());
		localROI[3] = Math.min(imgSubTarPairs.boundingBox[3], regionCenter[1] + rbf.getScale());
		localROI[4] = Math.max(imgSubTarPairs.boundingBox[4], regionCenter[2] - rbf.getScale());
		localROI[5] = Math.min(imgSubTarPairs.boundingBox[5], regionCenter[2] + rbf.getScale());

		// do I use the old gradient or calculate a new in since things may have
		// changed
		coarseGradient(regionCenter, localCoarseGradient);
		RegistrationUtilities.VectorNormalization(localCoarseGradient, gradParams);

		coarseLocalRegionCenter[0] = regionCenter[0];
		coarseLocalRegionCenter[1] = regionCenter[1];
		coarseLocalRegionCenter[2] = regionCenter[2];

		double originalNMI = coarseCostFunction(0,false);
		double delta = 0;
		double optimizedCoeff = 0;
		double optimizedNMI = 0;

		//System.out.format("Orig:" +originalNMI+"\n");
		if(originalNMI != -2){ //skip if already perfectly registered

			double maxCoeff = lambda/(rbf.getMaxValuesChange() * (Math.max(Math.abs(localCoarseGradient[0]), 
					Math.max(Math.abs(localCoarseGradient[1]), Math.abs(localCoarseGradient[2])))));
			CoarseOptimizer fun = new CoarseOptimizer(maxCoeff,-maxCoeff);
			//CoarseOptimizer fun = new CoarseOptimizer(imgSubTarPairs.maxDimensions,-imgSubTarPairs.maxDimensions);
			Optimizer1DContinuous opt = new BrentMethod1D();
			opt.initialize(fun);		
			opt.optimize(true);
			optimizedCoeff = opt.getExtrema();
			optimizedNMI = coarseCostFunction(optimizedCoeff,false);
			//System.out.format("potential:" +optimizedCoeff +" "+optimizedNMI+"\n");
			delta = (optimizedNMI) - originalNMI;
			//System.out.format("delta:" +delta+"\n");
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



	//public float coarseCostFunction(float[] lambda) {
	public double coarseCostFunction(double coeff, boolean commitUpdate) {
		double rbfVal;
		double coeff_x, coeff_y, coeff_z;

		int ox_valRBFoff, oy_valRBFoff, oz_valRBFoff;
		double defX, defY, defZ;
		double tlambda;
		int[] targetBins = new int[imgSubTarPairs.numOfTar];
		int[] subjectBins = new int[imgSubTarPairs.numOfSub];
		int[] testBin = new int[imgSubTarPairs.numOfSub];
		double[] interpValsD  = new double[imgSubTarPairs.numOfSub];
		//int i, j, k, ch;
		double x, y, z;
		VabraVolumeCollection referenceSubject;


		//temp start
		int XN =  imgSubTarPairs.totalDeformField.getExtents()[0];
		int YN =  imgSubTarPairs.totalDeformField.getExtents()[1];
		int ZN =  imgSubTarPairs.totalDeformField.getExtents()[2];
		//temp end

		imgSubTarPairs.hist.resetCurrentHistograms();
		if(coeff == 0) return -imgSubTarPairs.hist.getCurrentNMI(); 

		tlambda = (Math.round((double) coeff * 10000.0)) / 10000.0;

		coeff_x = ((double) tlambda) * localCoarseGradient[0];
		coeff_y = ((double) tlambda) * localCoarseGradient[1];
		coeff_z = ((double) tlambda) * localCoarseGradient[2];

		//optimized by putting outside and incrementing.
		for (int i = localROI[0]; i <= localROI[1]; i++) {
			for (int j = localROI[2]; j <= localROI[3]; j++) {
				for (int k = localROI[4]; k <= localROI[5]; k++) {

					//(ox, oy, oz) are coordinates of (i, j) relative to region center				
					ox_valRBFoff = i - coarseLocalRegionCenter[0] + rbf.getOffset();
					oy_valRBFoff = j - coarseLocalRegionCenter[1] + rbf.getOffset();
					oz_valRBFoff = k - coarseLocalRegionCenter[2] + rbf.getOffset();

					rbfVal = rbf.values[ox_valRBFoff][oy_valRBFoff][oz_valRBFoff];

					if (rbfVal != 0) {
						// steepest descent direction: negative of gradient of NMI wrt c.

						//Amount to adjust with RBF if not constrained
						defX = directionsOptmizationWeight[0]*coeff_x*rbfVal;
						defY = directionsOptmizationWeight[1]*coeff_y*rbfVal;
						defZ = directionsOptmizationWeight[2]*coeff_z*rbfVal;
						

						//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
						if (defFieldUpdateMode == 0){
							x = i + imgSubTarPairs.totalDeformField.getFloat(i, j, k, 0);
							y = j + imgSubTarPairs.totalDeformField.getFloat(i, j, k, 1);
							z = k + imgSubTarPairs.totalDeformField.getFloat(i, j, k, 2);
							referenceSubject = imgSubTarPairs.subject;	
						}else{
							x = i;
							y = j;
							z = k;
							referenceSubject = imgSubTarPairs.deformedSubjectCopy;
						}


						for (int ch = 0; ch < imgSubTarPairs.numOfSub; ch++) {			
							subjectBins[ch] = imgSubTarPairs.deformedSubjectBinned.data[ch].getUByte(i,j, k);
							//subjectBins[ch] = (subjectBins[ch]<0?0:subjectBins[ch]>255?255:subjectBins[ch]);		
							interpValsD[ch] = RegistrationUtilities.Interpolation(referenceSubject.data[ch], XN, YN, ZN, x + defX, y + defY,z + defZ, imgSubTarPairs.chInterpType[ch]);
							testBin[ch] = referenceSubject.calculateBin(interpValsD[ch], ch);
							//if(coeff == 0 && subjectBins[ch] != testBin[ch]) System.out.format(subjectBins[ch]+" "+interpValsD[ch]+" "+ testBin[ch]+"\n");
						}
						for (int ch = 0; ch < imgSubTarPairs.numOfTar; ch++) {		
							targetBins[ch] = imgSubTarPairs.targetBinned.data[ch].getUByte(i, j, k);
							//targetBins[ch] = (targetBins[ch]<0?0:targetBins[ch]>255?255:targetBins[ch]);
						}
						//System.out.format("interp"+interpValsD[0]+"submax"+referenceSubject.maxValsD[0]+"\n");
						//System.out.format("sub"+subjectBins[0]+"test"+testBin[0]+"tar"+targetBins[0]+"\n");
						imgSubTarPairs.hist.adjustCurrentBins(subjectBins, targetBins,testBin);
						//set as current deformation field if actually updating
						if(commitUpdate){			
							//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
							if (defFieldUpdateMode == 0){
								imgSubTarPairs.totalDeformField.set(i, j, k, 0, imgSubTarPairs.totalDeformField.getFloat(i, j, k, 0) + (float)defX);
								imgSubTarPairs.totalDeformField.set(i, j, k, 1, imgSubTarPairs.totalDeformField.getFloat(i, j, k, 1) + (float)defY);
								imgSubTarPairs.totalDeformField.set(i, j, k, 2, imgSubTarPairs.totalDeformField.getFloat(i, j, k, 2) + (float)defZ);
							}else{
								imgSubTarPairs.currentDeformField.set(i, j, k, 0, (float)defX);
								imgSubTarPairs.currentDeformField.set(i, j, k, 1, (float)defY);
								imgSubTarPairs.currentDeformField.set(i, j, k, 2, (float)defZ);
							}

							//change images and real histograms if actually updating
							for (int ch = 0; ch < imgSubTarPairs.numOfSub; ch++) {
								imgSubTarPairs.deformedSubject.data[ch].set(i, j, k, interpValsD[ch]);
								imgSubTarPairs.deformedSubjectBinned.data[ch].set(i, j, k, testBin[ch]);
							}
							imgSubTarPairs.hist.adjustOrigBins(subjectBins, targetBins,testBin);

						}

					}
				}
			}
		}

		//Update copies and final deformation field if actually updating
		if(commitUpdate){
			//Check which type of deformation update method to use(see "defFieldUPdateMode" variable declaration for details)
			//System.out.format("Before:"+ -imgSubTarPairs.hist.getMNMI(imgSubTarPairs.hist.currentDeformedSubject, imgSubTarPairs.hist.origTarget, imgSubTarPairs.hist.currentJointST)+"\n");
			imgSubTarPairs.hist.commitCurrentJointHistogram();
			//System.out.format("After:"+ -imgSubTarPairs.hist.getMNMI(imgSubTarPairs.hist.currentDeformedSubject, imgSubTarPairs.hist.origTarget, imgSubTarPairs.hist.currentJointST)+"\n");
			if (defFieldUpdateMode == 1){
				for(int ch = 0; ch < imgSubTarPairs.numOfSub; ch++){
					for (int i = localROI[0]; i <= localROI[1]; i++) 
						for (int j = localROI[2]; j <= localROI[3]; j++) 
							for (int k = localROI[4]; k <= localROI[5]; k++) 
								imgSubTarPairs.deformedSubjectCopy.data[ch].set(i, j, k, imgSubTarPairs.deformedSubject.data[ch].getDouble(i, j, k));
				}

				imgSubTarPairs.updateDefField(localROI);
			}

		}

	
		//for (int ch = 0; ch < imgSubTarPairs.numOfCh; ch++){
			//nmiValD -= imgSubTarPairs.chWeights[ch] * RegistrationUtilities.NMI(imgSubTarPairs.hist.currentDeformedSubject, imgSubTarPairs.hist.origTarget,
				//	imgSubTarPairs.hist.currentJointST, ch, imgSubTarPairs.numOfBins);
			//if(commitUpdate)System.out.format("Committing" + imgSubTarPairs.hist.getCurrentNMI() + "\n");
			//if(commitUpdate)System.out.format("Committed:" + imgSubTarPairs.hist.getOrigNMI() + "\n");
		//}
		//System.out.format("COARSE COST FUNC %f %f\n",lambda[0],nmiVal);
		return -imgSubTarPairs.hist.getCurrentNMI();
	}

	public VabraRBF getRBF(){
		return rbf;
	}

}

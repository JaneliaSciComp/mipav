package gov.nih.mipav.model.algorithms.registration.vabra;

public class VabraHistogramsMNMI extends VabraHistograms{
	
	public NDimHistogramModifier defSTxPlus,defSTxMinus,defSTyPlus,defSTyMinus,defSTzPlus,defSTzMinus;
	public NDimHistogram origJointST;
	
	public NDimHistogramModifier currentJointST;
	
	public VabraHistogramsMNMI(int numOfSub, int numOfTar, int numOfBins) {
		super(numOfSub, numOfTar, numOfBins);

		origJointST =  new NDimHistogram(numOfSub, numOfTar, numOfBins);

		//allocated joint histograms
		defSTxPlus = new NDimHistogramModifier(origJointST);
		defSTyPlus = new NDimHistogramModifier(origJointST);
		defSTzPlus = new NDimHistogramModifier(origJointST);

		defSTxMinus = new NDimHistogramModifier(origJointST);
		defSTyMinus = new NDimHistogramModifier(origJointST);
		defSTzMinus = new NDimHistogramModifier(origJointST);
		
		currentJointST =  new NDimHistogramModifier(origJointST);
	}
	
	public void copyOrigHistograms() {
		super.copyOrigHistograms();

		//Histograms already reference the original, so only need to reset the Modifiers.
		defSTxPlus.reset();
		defSTxMinus.reset();
		defSTyPlus.reset();
		defSTyMinus.reset();
		defSTzPlus.reset();
		defSTzMinus.reset();
	}

	public void resetCurrentHistograms() {
		currentDeformedSubject = copyHist(origDeformedSubject);
		currentJointST.reset();
	}
	
	
	public void updateHistograms(VabraVolumeCollection normedTarget, VabraVolumeCollection normedDeformedSubject, int[] boundingBox) {
	    // System.out.println(getClass().getCanonicalName()+"\t"+"UPDATE HISTOGRAMS ");
		if (origDeformedSubject == null) {
			System.out.format("null Histograms");
			// initializeHistograms();
		}
		for(int ch = 0; ch < numOfSub; ch++){
			RegistrationUtilities.Histogram3D(normedDeformedSubject.data, numOfBins, boundingBox, origDeformedSubject[ch],
					normedDeformedSubject.getXN(), normedDeformedSubject.getYN(), normedDeformedSubject.getZN() );
		}
		for(int ch = 0; ch < numOfTar; ch++){
			RegistrationUtilities.Histogram3D(normedTarget.data, numOfBins, boundingBox, origTarget[ch],
			normedTarget.getXN(), normedTarget.getYN(), normedTarget.getZN() );
		}
			
		origJointST.fillJointHistogram(normedDeformedSubject.data,normedTarget.data, boundingBox,
				normedDeformedSubject.getXN(), normedDeformedSubject.getYN(), normedDeformedSubject.getZN() );
	}
	
	
	public void dispose(){
		origDeformedSubject = null;
		origTarget = null;
		origJointST = null;

		// used in optimization -- perhaps move to child class
		currentDeformedSubject = null;
		currentTarget = null;
		currentJointST = null;
	}

	public double getOrigNMI(){
		return getNMI(origDeformedSubject, origTarget, origJointST);
	}
	
	public double getCurrentNMI(){
		return getNMI(currentDeformedSubject, origTarget, currentJointST);
	}
	
	public void adjustOrigBins( int[] subBin, int[] tarBin, int[] newBin){
		for(int ch = 0; ch < numOfSub; ch++){
			origDeformedSubject[ch][newBin[ch]] += 1;
			origDeformedSubject[ch][subBin[ch]] -= 1;
		}
	}
	
	public void adjustCurrentBins( int[] subBin, int[] tarBin, int[] newBin){
		adjustBins(currentDeformedSubject, currentJointST, subBin, tarBin, newBin);	
	}
	
	
	public void adjustAllGradientBins(VabraVolumeCollection subject, double origX, double origY, double origZ,  double defX, double defY, double defZ, int[] targetBins, int[] subjectBins){

		adjustGradientBins( subject, origX + defX, origY, origZ, defSxPlus, defSTxPlus, targetBins, subjectBins);
		adjustGradientBins( subject, origX - defX, origY, origZ, defSxMinus, defSTxMinus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY + defY, origZ, defSyPlus, defSTyPlus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY - defY, origZ, defSyMinus, defSTyMinus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY, origZ + defZ, defSzPlus, defSTzPlus, targetBins, subjectBins);
		adjustGradientBins( subject, origX, origY, origZ - defZ, defSzMinus, defSTzMinus, targetBins, subjectBins);
		
	}
	
	public void getNMIGradients(double[] results, double[] deltaC){
		results[0] = (getNMI(defSxPlus, origTarget, defSTxPlus)- getNMI(defSxMinus,origTarget, defSTxMinus))/ (2.0f * deltaC[0]);
		results[1] = (getNMI(defSyPlus, origTarget, defSTyPlus)- getNMI(defSyMinus, origTarget, defSTyMinus))/ (2.0f * deltaC[1]);
		results[2] = (getNMI(defSzPlus, origTarget, defSTzPlus)- getNMI(defSzMinus, origTarget, defSTzMinus))/ (2.0f * deltaC[2]);
	}
	
	public void commitCurrentJointHistogram(){
		System.out.format("USING NMNMI\n");
		currentJointST.commitModifierToRefHist();
	}
	
	private void adjustGradientBins(VabraVolumeCollection subject,double x,double y,double z, int[][] subjectHist,NDimHistogramModifier jointHist, int[] targetBins, int[] subjectBins){
			
			double[] testValsD = new double[numOfSub]; 
			int[] testBins = new int[numOfSub];
			
			if (x < subject.getXN() && x >= 0 && y < subject.getYN() && y >= 0 && z < subject.getZN() && z >= 0) {
				subject.interpolate(x, y, z, testValsD);
			} else {
				for (int ch = 0; ch < numOfSub; ch++) testValsD[ch] = subject.minValsD;
			}
			
			for (int ch = 0; ch < numOfSub; ch++) {
				testBins[ch] = subject.calculateBin(testValsD[ch], ch);
			}
			adjustBins(subjectHist, jointHist, subjectBins,targetBins,testBins);
	}
		
	private void adjustBins(int[][] subjectHist, NDimHistogramModifier jointHist, int[] oldSubBins, int[] tarBins, int[] newSubBins){
		for(int ch = 0; ch < numOfSub; ch++){
			subjectHist[ch][newSubBins[ch]] += 1;
			subjectHist[ch][oldSubBins[ch]] -= 1;
		}
		jointHist.move(oldSubBins, tarBins, newSubBins, tarBins);
	}	

	

	
	private double getNMI(int[][] subHist, int[][] tarHist, IntensityHistogram jointHist) {
		
		double sumOfSubEntropies = 0;
		double sumOfTarEntropies = 0;
		double jointEntropy = 0; 
		
		for (int ch = 0; ch < numOfSub; ch++){
			sumOfSubEntropies += calcEntropy(subHist[ch]);
		}
		
		for (int ch = 0; ch < numOfTar; ch++){
			sumOfTarEntropies += calcEntropy(tarHist[ch]);
		}
		
		jointEntropy = jointHist.getEntropy();
		//System.out.format(sumOfSubEntropies+" "+sumOfTarEntropies+" "+jointEntropy+" "+jointHist.getTotalVol()+"\n");
		if (sumOfSubEntropies == 0 && sumOfTarEntropies == 0 && jointEntropy == 0)
			return numOfSub + numOfTar;
		else
			return (sumOfSubEntropies + sumOfTarEntropies) / jointEntropy;
	}
	
	private double calcEntropy(int[] hist){
		int numVoxel = 0;
		double tmp = 0;
		double entropy = 0;
			for (int i = 0; i < numOfBins; i++) {
				//System.out.format(hist[i] + "\n");
				numVoxel += hist[i];
			}
			for (int i = 0; i < numOfBins; i++) {
				if (hist[i] > 0) {
					tmp = ((double) hist[i]) / numVoxel;
					entropy -= tmp * Math.log(tmp);
				}
			}
			
			//System.out.format("\n");
			
		return entropy;
	}
	
}



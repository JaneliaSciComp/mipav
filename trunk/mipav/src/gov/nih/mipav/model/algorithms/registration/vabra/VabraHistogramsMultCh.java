package gov.nih.mipav.model.algorithms.registration.vabra;


public class VabraHistogramsMultCh extends VabraHistograms{

	public int[][][] origJointST;
	public int[][][] currentJointST;
	public int[][][] defSTxPlus,defSTxMinus,defSTyPlus,defSTyMinus,defSTzPlus,defSTzMinus;
	
	protected int numOfCh;
	protected double[] chWeights;
	
	public VabraHistogramsMultCh(int numOfSub, int numOfTar, int numOfBins) {
		super(numOfSub, numOfTar, numOfBins);
		
		this.numOfBins = numOfBins;
		this.numOfSub = numOfSub;
		this.numOfTar = numOfTar;
		
		this.numOfCh = 1;
		
		chWeights = new double[numOfCh];
		for (int i = 0; i < numOfCh; i++ ) chWeights[i] = 1/((double)numOfCh);
		
		//allocated joint histograms
		defSTxPlus = new int[numOfCh][numOfBins][numOfBins];
		defSTyPlus = new int[numOfCh][numOfBins][numOfBins];
		defSTzPlus = new int[numOfCh][numOfBins][numOfBins];

		defSTxMinus = new int[numOfCh][numOfBins][numOfBins];
		defSTyMinus = new int[numOfCh][numOfBins][numOfBins];
		defSTzMinus = new int[numOfCh][numOfBins][numOfBins];
		
		origJointST = new int[numOfCh][numOfBins][numOfBins];
		
		currentJointST = new int[numOfCh][numOfBins][numOfBins];
	}
	
	public void copyOrigHistograms() {
		super.copyOrigHistograms();

		defSTxPlus = copyHist(origJointST);
		defSTxMinus = copyHist(origJointST);
		defSTyPlus = copyHist(origJointST);
		defSTyMinus = copyHist(origJointST);
		defSTzPlus = copyHist(origJointST);
		defSTzMinus = copyHist(origJointST);
	}
	
	public void resetCurrentHistograms() {
		currentDeformedSubject = copyHist(origDeformedSubject);
		currentJointST = copyHist(origJointST);
	}

	public void updateHistograms(VabraVolumeCollection normedTarget, VabraVolumeCollection normedDeformedSubject, int[] boundingBox) {
	    // System.out.println(getClass().getCanonicalName()+"\t"+"UPDATE HISTOGRAMS ");
		if (origDeformedSubject == null) {
			System.out.format("null Histograms");
			// initializeHistograms();
		}
		for (int ch = 0; ch < numOfCh; ch++) {
			RegistrationUtilities.Histogram3D(normedDeformedSubject.data, ch, numOfBins, boundingBox, origDeformedSubject, normedDeformedSubject.XN, normedDeformedSubject.YN, normedDeformedSubject.ZN );
			RegistrationUtilities.Histogram3D(normedTarget.data, ch, numOfBins, boundingBox, origTarget, normedTarget.XN, normedTarget.YN, normedTarget.ZN);
			RegistrationUtilities.JointHistogram3D(normedDeformedSubject.data,normedTarget.data, ch, 
					numOfBins, boundingBox, origJointST, normedTarget.XN, normedTarget.YN, normedTarget.ZN);
		}
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
		
		double nmiValD=0;
		
		for (int ch = 0; ch < numOfCh; ch++){
			nmiValD += chWeights[ch] * RegistrationUtilities.NMI(origDeformedSubject, origTarget, origJointST, ch, numOfBins); 
		}
		return nmiValD;
	}
	
	public double getCurrentNMI(){
		
		double nmiValD=0;
		
		for (int ch = 0; ch < numOfCh; ch++){
			nmiValD += chWeights[ch] * RegistrationUtilities.NMI(currentDeformedSubject, origTarget, currentJointST, ch, numOfBins); 
		}
		return nmiValD;
	}
		
	public void adjustOrigBins(int[] subBin, int[] tarBin, int[] newBin){
		for (int ch = 0; ch < numOfCh; ch++)adjustBins(origDeformedSubject, origJointST, subBin[ch], tarBin[ch], newBin[ch],ch);
	}
	
	public void adjustCurrentBins( int[] subBin, int[] tarBin, int[] newBin){
		for (int ch = 0; ch < numOfCh; ch++)adjustBins(currentDeformedSubject, currentJointST, subBin[ch], tarBin[ch], newBin[ch],ch);	
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
		double gradx[] = new double[numOfCh];
		double grady[] = new double[numOfCh];
		double gradz[] = new double[numOfCh];
		
		for (int ch = 0; ch < numOfCh; ch++) {
			gradx[ch] = (RegistrationUtilities.NMI(defSxPlus, origTarget,defSTxPlus, ch, numOfBins)
					- RegistrationUtilities.NMI(defSxMinus,origTarget, defSTxMinus, ch, numOfBins))
					/ (2.0f * deltaC[0]);
			grady[ch] = (RegistrationUtilities.NMI(defSyPlus, origTarget, defSTyPlus, ch, numOfBins)
					- RegistrationUtilities.NMI(defSyMinus, origTarget, defSTyMinus, ch, numOfBins))
					/ (2.0f * deltaC[1]);
			gradz[ch] = (RegistrationUtilities.NMI(defSzPlus, origTarget, defSTzPlus, ch, numOfBins)
					- RegistrationUtilities.NMI(defSzMinus, origTarget, defSTzMinus, ch, numOfBins))
					/ (2.0f * deltaC[2]);
		}

		results[0] = 0.0;
		results[1] = 0.0;
		results[2] = 0.0;

		for (int ch = 0; ch < numOfCh; ch++) {
			results[0] += gradx[ch] * chWeights[ch];
			results[1] += grady[ch] * chWeights[ch];
			results[2] += gradz[ch] * chWeights[ch];
		}
	}
	
	public void commitCurrentJointHistogram(){
		//Should be already done
	}
	
	private void adjustGradientBins(VabraVolumeCollection subject,double x,double y,double z, int[][] subjectHist,int[][][] jointHist, int[] targetBins, int[] subjectBins){
		int numOfSub = subjectBins.length;
		
		double[] testValsD = new double[numOfSub]; 
		int testBins;
		
		if (x < subject.getXN() && x >= 0 && y < subject.getYN() && y >= 0 && z < subject.getZN() && z >= 0) {
			subject.interpolate(x, y, z, testValsD);
		} else {
			for (int ch = 0; ch < numOfSub; ch++) testValsD[ch] = subject.minValsD;
		}
		
		for (int ch = 0; ch < numOfSub; ch++) {
			testBins = subject.calculateBin(testValsD[ch], ch);
			adjustBins(subjectHist, jointHist, subjectBins[ch],targetBins[ch],testBins, ch);
		}

	}
	

	private void adjustBins(int[][] subjectHist, int[][][] jointHist, int subBin, int tarBin, int newBin, int ch){
		subjectHist[ch][newBin] += 1;
		subjectHist[ch][subBin] -= 1;
		jointHist[ch][newBin][tarBin] += 1;
		jointHist[ch][subBin][tarBin] -= 1;
	}
		
	
}

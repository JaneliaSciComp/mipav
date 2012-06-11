package gov.nih.mipav.model.algorithms.registration.vabra;

abstract public class VabraHistograms{
	public int[][] defSxPlus, defSxMinus, defSyPlus, defSyMinus, defSzPlus, defSzMinus;
	public int[][] origDeformedSubject, origTarget;
	public int[][] currentDeformedSubject, currentTarget;


	static public int defaultBins = 64;
	
	protected int numOfBins;
	protected int numOfSub;
	protected int numOfTar;
	
	public VabraHistograms(int numOfSub, int numOfTar, int numOfBins) {
		this.numOfBins = numOfBins;
		this.numOfSub = numOfSub;
		this.numOfTar = numOfTar;
	
		defSxPlus = new int[numOfSub][numOfBins];
		defSyPlus = new int[numOfSub][numOfBins];
		defSzPlus = new int[numOfSub][numOfBins];
		
		defSxMinus = new int[numOfSub][numOfBins];
		defSyMinus = new int[numOfSub][numOfBins];
		defSzMinus = new int[numOfSub][numOfBins];

		origDeformedSubject = new int[numOfSub][numOfBins];
		origTarget = new int[numOfTar][numOfBins];
		
		currentDeformedSubject = new int[numOfSub][numOfBins];
		currentTarget = new int[numOfTar][numOfBins];
	}
	public void copyOrigHistograms(){
		defSxPlus = copyHist(origDeformedSubject);
		defSxMinus = copyHist(origDeformedSubject);
		defSyPlus = copyHist(origDeformedSubject);
		defSyMinus = copyHist(origDeformedSubject);
		defSzPlus = copyHist(origDeformedSubject);
		defSzMinus = copyHist(origDeformedSubject);
	}
	abstract public void resetCurrentHistograms();
	abstract public void updateHistograms(VabraVolumeCollection normedTarget, VabraVolumeCollection normedDeformedSubject, int[] boundingBox);
	
	abstract public void dispose();

	abstract public double getOrigNMI();
	abstract public double getCurrentNMI();
	
	abstract public void adjustOrigBins( int[] subBin, int[] tarBin, int[] newBin);
	abstract public void adjustCurrentBins( int[] subBin, int[] tarBin, int[] newBin);

	
	abstract public void adjustAllGradientBins(VabraVolumeCollection subject, double origX, double origY, double origZ,  double defX, double defY, double defZ, int[] targetBins, int[] subjectBins);
	abstract public void getNMIGradients(double[] results, double[] deltaC);
	
	abstract public void commitCurrentJointHistogram();
	
	int[] copyHist(int[] in){
		
		int[] copy = new int[in.length];
		
		for(int i=0; i < in.length; i++){
			copy[i] = in[i];
			
		}
		return copy;
	}
	
	int[][] copyHist(int[][] in){
		
		int[][] copy = new int[in.length][in[0].length];
		
		for(int i=0; i < in.length; i++){
			for(int j=0; j < in[0].length; j++){
				copy[i][j] = in[i][j];
			}
		}
		return copy;
	}
	
	public int[][][] copyHist(int[][][] vol) {
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
	
}

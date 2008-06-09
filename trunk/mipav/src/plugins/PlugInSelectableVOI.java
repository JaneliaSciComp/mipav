import gov.nih.mipav.model.structures.*;

import java.awt.Color;

public class PlugInSelectableVOI extends VOI{//extends VOI{
	
	public static final double NOT_CALC = Double.MIN_VALUE;
	
	private boolean closed;
	
	private int maxCurvesPerSlice;
	
	private int paneNum = INVALID_PANE_NUMBER;
	
	private boolean fillEligible;
	
	private boolean calcEligible;
	
	/**When true, automatic segmentation methods were used 
	 * to generate this VOI and a person has NOT reviewed it.*/
	private boolean computerGenerated;
	
	private boolean created = false;
	
	/** Total area of this VOI using calculating method. */
	private double[] totalAreaCalc;
	
	/** Total area of this VOI using counting method (subject to partial voluming). */
	private double[] totalAreaCount;
	
	/** Total area of this VOI that is neither muscle or fat. */
	private double[] partialArea;
	
	/** Total fat area of this VOI. */
	private double[] fatArea;
	
	/** Total muscle area of this VOI. */
	private double[] leanArea;
	
	/** Mean Hounsfield unit for the fat area of this VOI. */
	private double[] meanFatH;
	
	/** Mean Hounsfield unit for the muscle area of this VOI. */
	private double[] meanLeanH;
	
	/** Mean Hounsfield unit for this VOI. */
	private double[] meanTotalH;

	//~ Static fields --------------------------------------------------------------------------------------------------
	
	public static final int INVALID_PANE_NUMBER = -1;

	public static final Color INVALID_COLOR = new Color(234, 123, 123);
	
	public PlugInSelectableVOI(String name, boolean closed, int maxCurvesPerSlice, int paneNum, 
								boolean fillEligible, boolean calcEligible, int imageSize) {
		super((short)0, name, imageSize);
		this.closed = closed;
		this.maxCurvesPerSlice = maxCurvesPerSlice;
		this.paneNum = paneNum;
		this.fillEligible = fillEligible;
		this.calcEligible = calcEligible;
		this.computerGenerated = false;
		
		this.totalAreaCalc = new double[imageSize+1];
		this.totalAreaCount = new double[imageSize+1];
		this.partialArea = new double[imageSize+1];
		this.fatArea = new double[imageSize+1];
		this.leanArea = new double[imageSize+1];
		this.meanFatH = new double[imageSize+1];
		this.meanLeanH = new double[imageSize+1];
		this.meanTotalH = new double[imageSize+1];
		
		for(int i=0; i<imageSize+1; i++) {
			totalAreaCalc[i] = Double.MIN_VALUE;
			totalAreaCount[i] = Double.MIN_VALUE;
			partialArea[i] = Double.MIN_VALUE;
			fatArea[i] = Double.MIN_VALUE;
			leanArea[i] = Double.MIN_VALUE;
			meanFatH[i] = Double.MIN_VALUE;
			meanLeanH[i] = Double.MIN_VALUE;
			meanTotalH[i] = Double.MIN_VALUE;
		}
		
		setColor(INVALID_COLOR);
	}

	public boolean isClosed() {
		return closed;
	}
	
	public boolean isCreated() {
		return created;
	}
	
	public void setCreated(boolean created) {
		this.created = created;
	}

	public int getMaxCurvesPerSlice() {
		return maxCurvesPerSlice;
	}
	
	public int getLocation() {
		return paneNum;
	}
	
	public void setLocation(int loc) {
		this.paneNum = loc;
	}

	public boolean fillEligible() {
		return fillEligible;
	}
	
	public boolean calcEligible() {
		return calcEligible;
	}

	public double getTotalAreaCalc() {
		return totalAreaCalc[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getTotalAreaCalc(int slice) {
		return totalAreaCalc[slice];
	}

	public void setTotalAreaCalc(double totalAreaCalc) {
		this.totalAreaCalc[getZDim()] = totalAreaCalc;
	}
	
	/**
	 * 
	 * @param totalAreaCalc
	 * @param slice zero based slice number
	 */
	public void setTotalAreaCalc(double totalAreaCalc, int slice) {
		this.totalAreaCalc[slice] = totalAreaCalc;
	}

	public double getTotalAreaCount() {
		return totalAreaCount[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getTotalAreaCount(int slice) {
		return totalAreaCount[slice];
	}
	
	public void setTotalAreaCount(double totalAreaCount) {
		this.totalAreaCount[getZDim()] = totalAreaCount;
	}
	
	/**
	 * 
	 * @param totalAreaCount
	 * @param slice zero based slice number
	 */
	public void setTotalAreaCount(double totalAreaCount, int slice) {
		this.totalAreaCount[slice] = totalAreaCount;
	}

	public double getPartialArea() {
		return partialArea[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getPartialArea(int slice) {
		return partialArea[slice];
	}

	public void setPartialArea(double partialArea) {
		this.partialArea[getZDim()] = partialArea;
	}
	
	/**
	 * 
	 * @param partialArea
	 * @param slice zero based slice number
	 */
	public void setPartialArea(double partialArea, int slice) {
		this.partialArea[slice] = partialArea;
	}

	public double getFatArea() {
		return fatArea[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getFatArea(int slice) {
		return fatArea[slice];
	}

	public void setFatArea(double fatArea) {
		this.fatArea[getZDim()] = fatArea;
	}
	
	/**
	 * 
	 * @param fatArea
	 * @param slice zero based slice number
	 */
	public void setFatArea(double fatArea, int slice) {
		this.fatArea[slice] = fatArea;
	}

	public double getLeanArea() {
		return leanArea[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getLeanArea(int slice) {
		return leanArea[slice];
	}

	public void setLeanArea(double leanArea) {
		this.leanArea[getZDim()] = leanArea;
	}
	
	/**
	 * 
	 * @param leanArea
	 * @param slice zero based slice number
	 */
	public void setLeanArea(double leanArea, int slice) {
		this.leanArea[slice] = leanArea;
	}

	public double getMeanFatH() {
		return meanFatH[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getMeanFatH(int slice) {
		return meanFatH[slice];
	}

	public void setMeanFatH(double meanFatH) {
		this.meanFatH[getZDim()] = meanFatH;
	}
	
	/**
	 * 
	 * @param meanFatH
	 * @param slice zero based slice number
	 */
	public void setMeanFatH(double meanFatH, int slice) {
		this.meanFatH[slice] = meanFatH;
	}

	public double getMeanLeanH() {
		return meanLeanH[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getMeanLeanH(int slice) {
		return meanLeanH[slice];
	}

	public void setMeanLeanH(double meanLeanH) {
		this.meanLeanH[getZDim()] = meanLeanH;
	}
	
	/**
	 * 
	 * @param meanLeanH
	 * @param slice zero based slice number
	 */
	public void setMeanLeanH(double meanLeanH, int slice) {
		this.meanLeanH[slice] = meanLeanH;
	}

	public double getMeanTotalH() {
		return meanTotalH[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getMeanTotalH(int slice) {
		return meanTotalH[slice];
	}

	public void setMeanTotalH(double meanTotalH) {
		this.meanTotalH[getZDim()] = meanTotalH;
	}
	
	/**
	 * 
	 * @param meanTotalH zero based slice number
	 * @param slice
	 */
	public void setMeanTotalH(double meanTotalH, int slice) {
		this.meanTotalH[slice] = meanTotalH;
	}

	public boolean isComputerGenerated() {
		return computerGenerated;
	}

	public void setComputerGenerated(boolean computerGenerated) {
		this.computerGenerated = computerGenerated;
	}
}
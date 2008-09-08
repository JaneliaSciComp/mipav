import gov.nih.mipav.model.structures.*;

import java.awt.Color;
import java.util.Date;

public class PlugInSelectableVOI extends VOI {
	
	
	/**Whether the VOI most be closed.*/
	private boolean closed;
	
	/**Max number of curves that this VOI can have on any particular slice.*/
	private int maxCurvesPerSlice;
	
	/**Position in output file this VOI should have.  In such output files, VOIs where isCreated() == false should
	 * display 0 for calculations. If not calculable should be INVALID_LOC_NUMBER
	 */
	private int outputLoc = INVALID_LOC_NUMBER;
	
	/**Pane location of this VOI on the MuscleDialogPrompt*/
	private int paneNum = INVALID_LOC_NUMBER;
	
	/**Whether this VOI can be filled. */
	private boolean fillEligible;
	
	/**Whether calculations should be performed on this VOI.*/
	private boolean calcEligible;
	
	/**When true, automatic segmentation methods were used 
	 * to generate this VOI and a person has NOT reviewed it.*/
	private boolean computerGenerated;
	
	private boolean created = false;
	
	/** Total area of this VOI using counting method (subject to partial voluming). */
	private double[] totalArea;
	
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
	
	/**If lastCalculated.compareTo(lastModified) < 0, do calculate*/
	private Date lastCalculated = null;
	
	/**Keeps track of when this VOI was last saved*/
	private Date lastModified = null;

	//~ Static fields --------------------------------------------------------------------------------------------------
	
	public static final double NOT_CALC = Double.MIN_VALUE;
	
	public static final int INVALID_LOC_NUMBER = -1;

	public static final Color INVALID_COLOR = new Color(234, 123, 123);
	
	public PlugInSelectableVOI(String name, boolean closed, int maxCurvesPerSlice, int paneNum, 
			boolean fillEligible, boolean calcEligible, int imageSize) {
		this(name, closed, maxCurvesPerSlice, paneNum, fillEligible, calcEligible, imageSize, INVALID_LOC_NUMBER, INVALID_COLOR);
	}
	
	public PlugInSelectableVOI(String name, boolean closed, int maxCurvesPerSlice, int paneNum, 
			boolean fillEligible, boolean calcEligible, int imageSize, Color color) {
		this(name, closed, maxCurvesPerSlice, paneNum, fillEligible, calcEligible, imageSize, INVALID_LOC_NUMBER, color);
	}
	
	public PlugInSelectableVOI(String name, boolean closed, int maxCurvesPerSlice, int paneNum, 
								boolean fillEligible, boolean calcEligible, int imageSize, int outputLoc) {
		this(name, closed, maxCurvesPerSlice, paneNum, fillEligible, calcEligible, imageSize, outputLoc, INVALID_COLOR);
	}
	public PlugInSelectableVOI(String name, boolean closed, int maxCurvesPerSlice, int paneNum, 
			boolean fillEligible, boolean calcEligible, int imageSize, int outputLoc, Color color) {
		super((short)0, name, imageSize);
		this.closed = closed;
		this.maxCurvesPerSlice = maxCurvesPerSlice;
		this.paneNum = paneNum;
		this.fillEligible = fillEligible;
		this.calcEligible = calcEligible;
		this.computerGenerated = false;
		this.outputLoc = outputLoc;
		
		this.totalArea = new double[imageSize+1];
		this.partialArea = new double[imageSize+1];
		this.fatArea = new double[imageSize+1];
		this.leanArea = new double[imageSize+1];
		this.meanFatH = new double[imageSize+1];
		this.meanLeanH = new double[imageSize+1];
		this.meanTotalH = new double[imageSize+1];
		
		this.lastCalculated = new Date(System.currentTimeMillis()-1000);
		this.lastModified = new Date(System.currentTimeMillis());
		
		for(int i=0; i<imageSize+1; i++) {
			totalArea[i] = Double.MIN_VALUE;
			partialArea[i] = Double.MIN_VALUE;
			fatArea[i] = Double.MIN_VALUE;
			leanArea[i] = Double.MIN_VALUE;
			meanFatH[i] = Double.MIN_VALUE;
			meanLeanH[i] = Double.MIN_VALUE;
			meanTotalH[i] = Double.MIN_VALUE;
		}
		
		setColor(color);
	}

	public boolean getCreated() {
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

	public boolean getFillEligible() {
		return fillEligible;
	}
	
	public boolean getCalcEligible() {
		return calcEligible;
	}

	public double getTotalArea() {
		return totalArea[getZDim()];
	}
	
	/**
	 * 
	 * @param slice zero based slice number
	 * @return
	 */
	public double getTotalArea(int slice) {
		return totalArea[slice];
	}
	
	public void setTotalArea(double totalAreaCount) {
		this.totalArea[getZDim()] = totalAreaCount;
	}
	
	/**
	 * 
	 * @param totalAreaCount
	 * @param slice zero based slice number
	 */
	public void setTotalArea(double totalAreaCount, int slice) {
		this.totalArea[slice] = totalAreaCount;
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

	public boolean isClosed() {
		return closed;
	}

	public boolean isComputerGenerated() {
		return computerGenerated;
	}

	public void setComputerGenerated(boolean computerGenerated) {
		this.computerGenerated = computerGenerated;
	}

	public int getOutputLoc() {
		return outputLoc;
	}

	public Date getLastCalculated() {
		return lastCalculated;
	}

	public void setLastCalculated(long time) {
		this.lastCalculated = new Date(time);
	}

	public Date getLastModified() {
		return lastModified;
	}

	public void setLastModified(long time) {
		this.lastModified = new Date(time);
	}
}
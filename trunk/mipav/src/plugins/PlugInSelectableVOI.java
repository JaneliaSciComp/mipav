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
	
	/** VOIs that this VOI uses in its calculations. */
	private PlugInSelectableVOI[] children;
	
	/** VOIs that use this VOI in their calculations. */
	private PlugInSelectableVOI[] dependents;
	
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
	
	/** Amount of time in taken to segment VOI in ms*/
	private long segmentationTime;
	
	/** Amount of time taken to calculate VOI in ms*/
	private long calculationTime;

	//~ Static fields --------------------------------------------------------------------------------------------------
	
	/**Slice number that represents whole volume for calculation purposes.*/
	public static final int WHOLE_VOLUME_SLICE_NUMBER = -1;
	
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
		this.segmentationTime = 0;
		this.calculationTime = 0;
		
		for(int i=0; i<imageSize+1; i++) {
			totalArea[i] = Double.MIN_VALUE;
			partialArea[i] = Double.MIN_VALUE;
			fatArea[i] = Double.MIN_VALUE;
			leanArea[i] = Double.MIN_VALUE;
			meanFatH[i] = Double.MIN_VALUE;
			meanLeanH[i] = Double.MIN_VALUE;
			meanTotalH[i] = Double.MIN_VALUE;
		}
		
		dependents = new PlugInSelectableVOI[0];
		children = new PlugInSelectableVOI[0];
		
		setColor(color);
	}

	/**
	 * Method for adding a child to the list of VOIs this VOI uses for its calculations.
	 * Can only add a calc eligible child, can not add itself.
	 * @param newChild
	 * @return
	 */
	public boolean addChild(PlugInSelectableVOI newChild) {
    	if(!newChild.getCalcEligible() || newChild.equals(this))
    		return false;
    	for(int i=0; i<children.length; i++) 
    		if(children[i].equals(newChild))
    			return false;
    		
		//able to be added
		PlugInSelectableVOI[] oldChildren = children;
    	children = new PlugInSelectableVOI[oldChildren.length+1];
    	for(int i=0; i<oldChildren.length; i++) {
    		children[i] = oldChildren[i];
    	}
    	children[oldChildren.length] = newChild;
    	return true;
    }
	
	/**
     * Gets all VOIs that this VOI uses in its calculations.
     * 
     * @return all VOIs that require this VOI to have been calculated.
     */
    public PlugInSelectableVOI[] getChildren() {
    	return children;
    }
    
    
	
	public boolean getCreated() {
		return created;
	}
	
	public void setCreated(boolean created) {
		this.created = created;
	}
	
	/**
	 * Method for adding a dependent to the list of VOIs that use this VOI in its calculations.
	 * Can only add a calcEligible dependent, can not add itself or a dependent twice.
	 * @param newDependent
	 */
	public boolean addDependent(PlugInSelectableVOI newDependent) {
		if(!newDependent.getCalcEligible() || newDependent.equals(this))
			return false;
		for(int i=0; i<dependents.length; i++) 
    		if(dependents[i].equals(newDependent))
    			return false;
    		
		//able to be added
    	PlugInSelectableVOI[] oldDependents = dependents;
    	dependents = new PlugInSelectableVOI[oldDependents.length+1];
    	for(int i=0; i<oldDependents.length; i++) {
    		dependents[i] = oldDependents[i];
    	}
    	dependents[oldDependents.length] = newDependent;
    	return true;
    }
	
	/**
     * Gets all VOIs that are dependent on having this VOI recalculated.
     * 
     * @return all VOIs that require this VOI to have been calculated.
     */
    public PlugInSelectableVOI[] getDependents() {
    	return dependents;
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			return getTotalArea();
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			setTotalArea(totalAreaCount);
		else
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			return getPartialArea();
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			setPartialArea(partialArea);
		else
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			return getFatArea();
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			setFatArea(fatArea);
		else
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			return getLeanArea();
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			setLeanArea(leanArea);
		else 
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			return getMeanFatH();
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			setMeanFatH(meanFatH);
		else
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			return getMeanLeanH();
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			setMeanLeanH(meanLeanH);
		else
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			return getMeanTotalH();
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
		if(slice == WHOLE_VOLUME_SLICE_NUMBER) 
			setMeanTotalH(meanTotalH);
		else
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

	public long getSegmentationTime() {
		return segmentationTime;
	}

	public void setSegmentationTime(long segmentationTime) {
		this.segmentationTime = segmentationTime;
	}

	public long getCalculationTime() {
		return calculationTime;
	}

	public void setCalculationTime(long calculationTime) {
		this.calculationTime = calculationTime;
	}
}
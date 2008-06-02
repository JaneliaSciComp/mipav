import gov.nih.mipav.model.structures.*;

import java.awt.Color;

public class PlugInSelectableVOI extends VOI{//extends VOI{
	
	public static final double NOT_CALC = Double.MIN_VALUE;
	
	private boolean closed;
	
	private int maxCurvesPerSlice;
	
	private int paneNum = INVALID_PANE_NUMBER;
	
	private boolean fillEligible;
	
	private boolean calcEligible;
	
	private boolean created = false;
	
	/** Total area of this VOI using calculating method. */
	private double totalAreaCalc = Double.MIN_VALUE;
	
	/** Total area of this VOI using counting method (subject to partial voluming). */
	private double totalAreaCount = Double.MIN_VALUE;
	
	/** Total area of this VOI that is neither muscle or fat. */
	private double partialArea = Double.MIN_VALUE;
	
	/** Total fat area of this VOI. */
	private double fatArea = Double.MIN_VALUE;
	
	/** Total muscle area of this VOI. */
	private double leanArea = Double.MIN_VALUE;
	
	/** Mean Hounsfield unit for the fat area of this VOI. */
	private double meanFatH = Double.MIN_VALUE;
	
	/** Mean Hounsfield unit for the muscle area of this VOI. */
	private double meanLeanH = Double.MIN_VALUE;
	
	/** Mean Hounsfield unit for this VOI. */
	private double meanTotalH = Double.MIN_VALUE;

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
		return totalAreaCalc;
	}

	public void setTotalAreaCalc(double totalAreaCalc) {
		this.totalAreaCalc = totalAreaCalc;
	}

	public double getTotalAreaCount() {
		return totalAreaCount;
	}

	public void setTotalAreaCount(double totalAreaCount) {
		this.totalAreaCount = totalAreaCount;
	}

	public double getPartialArea() {
		return partialArea;
	}

	public void setPartialArea(double partialArea) {
		this.partialArea = partialArea;
	}

	public double getFatArea() {
		return fatArea;
	}

	public void setFatArea(double fatArea) {
		this.fatArea = fatArea;
	}

	public double getLeanArea() {
		return leanArea;
	}

	public void setLeanArea(double leanArea) {
		this.leanArea = leanArea;
	}

	public double getMeanFatH() {
		return meanFatH;
	}

	public void setMeanFatH(double meanFatH) {
		this.meanFatH = meanFatH;
	}

	public double getMeanLeanH() {
		return meanLeanH;
	}

	public void setMeanLeanH(double meanLeanH) {
		this.meanLeanH = meanLeanH;
	}

	public double getMeanTotalH() {
		return meanTotalH;
	}

	public void setMeanTotalH(double meanTotalH) {
		this.meanTotalH = meanTotalH;
	}
}
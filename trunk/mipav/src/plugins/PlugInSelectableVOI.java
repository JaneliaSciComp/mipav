import gov.nih.mipav.model.structures.*;

import java.awt.Color;

public class PlugInSelectableVOI extends VOI{//extends VOI{
	
	private boolean closed;
	
	private int maxCurvesPerSlice;
	
	private int paneNum = INVALID_PANE_NUMBER;
	
	private boolean doFill;
	
	private boolean doCalc;
	
	private boolean created = false;

	//~ Static fields --------------------------------------------------------------------------------------------------
	
	public static final int INVALID_PANE_NUMBER = -1;

	public static final Color INVALID_COLOR = new Color(234, 123, 123);
	
	public PlugInSelectableVOI(String name, boolean closed, int maxCurvesPerSlice, int paneNum, 
								boolean doFill, boolean doCalc, int imageSize) {
		super((short)0, name, imageSize);
		this.closed = closed;
		this.maxCurvesPerSlice = maxCurvesPerSlice;
		this.paneNum = paneNum;
		this.doFill = doFill;
		this.doCalc = doCalc;
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

	public boolean doFill() {
		return doFill;
	}
	
	public boolean doCalc() {
		return doCalc;
	}
}
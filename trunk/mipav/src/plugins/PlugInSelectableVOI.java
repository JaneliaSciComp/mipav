import java.awt.Color;

public class PlugInSelectableVOI {//extends VOI{
		
	private String name;
	
	private boolean closed;
	
	private int numCurves;
	
	private int location = INVALID_LOCATION;
	
	private boolean fillable;
	
	private boolean doCalc;
	
	private Color color = INVALID_COLOR;

	//~ Static fields --------------------------------------------------------------------------------------------------
	
	public static final int INVALID_LOCATION = -1;

	public static final Color INVALID_COLOR = Color.black;
	
	public PlugInSelectableVOI(String name, boolean closed, int numCurves, int location, 
								boolean fillable, boolean doCalc) {
		this.name = name;
		this.closed = closed;
		this.numCurves = numCurves;
		this.location = location;
		this.fillable = fillable;
		this.doCalc = doCalc;
	}

	public String getName() {
		return name;
	}

	public boolean isClosed() {
		return closed;
	}

	public int getNumCurves() {
		return numCurves;
	}
	
	public int getLocation() {
		return location;
	}
	
	public void setLocation(int loc) {
		this.location = loc;
	}

	public boolean isFillable() {
		return fillable;
	}
	
	public boolean doCalc() {
		return doCalc;
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		this.color = color;
	}
}
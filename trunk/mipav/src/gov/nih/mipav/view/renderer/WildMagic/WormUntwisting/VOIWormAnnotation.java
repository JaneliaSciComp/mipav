package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import java.awt.Color;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;

public class VOIWormAnnotation extends VOIText {
	
	private static final long serialVersionUID = 4256240928835686605L;

	private boolean retwist = false;
	private boolean modified = false;
	private int latticeSegment = -1;
	private int slice = -1;
	private boolean selected = false;
	private VOI showSelectedVOI = null;
	private VOIContour[] showSelected = null;
	private Vector3f selectionOffset = null;
	private boolean isCurveAnnotation = false;
	private boolean isSeamCell = false;
	
	public VOIWormAnnotation() {
		super();
	}

    public VOIWormAnnotation( Vector3f kPosition ) {
    	super(kPosition);  
    }

    public VOIWormAnnotation( Vector<Vector3f> kPositions ) {
    	super(kPositions);
    }
    
    public VOIWormAnnotation( VOIText kVOI ) {
    	super(kVOI);
    }
    
    public VOIWormAnnotation( VOIWormAnnotation kVOI ) {
    	super(kVOI);
    	this.retwist = kVOI.retwist;
    	this.modified = kVOI.modified;
    	this.latticeSegment = kVOI.latticeSegment;
    	this.slice = kVOI.slice;
    	this.isCurveAnnotation = kVOI.isCurveAnnotation;
    	this.isSeamCell = kVOI.isSeamCell;
    }
    
	public VOIWormAnnotation clone() {
        return new VOIWormAnnotation(this);
    }
	
	public void dispose() {
		showSelected = null;
		showSelectedVOI = null;
		selectionOffset = null;
	}
	
	public void modified( boolean modified ) {
		this.modified = modified;
	}
	
	public boolean modified() {
		return this.modified;
	}
	
	public void retwist( boolean retwist ) {
		this.retwist = retwist;
		if ( retwist ) {
			this.slice = (int) firstElement().Z;
		}
		else {
			this.slice = -1;
		}
	}
	
	public boolean retwist() {
		return this.retwist;
	}
	
	public int getSlice() {
		return this.slice;
	}
	
	public int getLatticeSegment() {
		return this.latticeSegment;
	}
	
	public void setLatticeSegment( int segment ) {
		this.latticeSegment = segment;
	}
	
	public boolean isSeamCell() {
		return isSeamCell;
	}
	
	public void setSeamCell( boolean isSeam ) {
		isSeamCell = isSeam;
	}

	public void setCurveAnnotation( boolean isCurve ) {
		isCurveAnnotation = isCurve;
	}

	public boolean isCurveAnnotation() {
		return isCurveAnnotation;
	}

	public void setSelected( boolean select ) {
		this.selected = select;
	}
	
	public boolean isSelected() {
		return this.selected;
	}
	
	public void setSelectionOffset( Vector3f selection ) {
//		System.err.println( getText() + "  setSelectionOffset " + selection );
		this.selectionOffset = selection;
	}
	
	public Vector3f getSelectionOffset( ) {
		return this.selectionOffset;
	}
	
	private Vector3f untwistPt = null;
	private float minDistance = -Float.MAX_VALUE;
	public void untwistTest( Vector3f pt, float distance ) {
		// save inbounds point:
		if ( untwistPt == null ) {
			untwistPt = new Vector3f(pt);
			minDistance = distance;
		}
		if ( distance < minDistance ) {
			untwistPt.copy(pt);
		}
	}
	
	public Vector3f getUntwistTest() {
		return untwistPt;
	}
	

	private Vector3f minUntwistPt = null;
	private float minUntwistDistance = -Float.MAX_VALUE;
	public void untwistTestNoBounds( Vector3f pt, float distance ) {
		// save minumum point:
		if ( minUntwistPt == null ) {
			minUntwistPt = new Vector3f(pt);
			minUntwistDistance = distance;
		}
		if ( distance < minUntwistDistance ) {
			minUntwistPt.copy(pt);
		}
	}
	
	public Vector3f getUntwistTestNoBounds() {
		return minUntwistPt;
	}
	
	public VOI updateSelected(ModelImage image ) {
		Vector3f pt = this.firstElement();
		if (showSelectedVOI == null) {
			showSelectedVOI = new VOI((short) image.getVOIs().getUniqueID(), "showSelected"+getText(), VOI.POLYLINE, (float) Math.random());
			showSelectedVOI.setColor(new Color(0, 255, 255));
		}
		if ((showSelected == null) || (showSelectedVOI.getCurves().size() == 0)) {
			showSelected = new VOIContour[3];
			showSelected[0] = new VOIContour(true);
			makeSelectionFrame(Vector3f.UNIT_X, Vector3f.UNIT_Y, pt, 4, showSelected[0]);
			showSelectedVOI.getCurves().add(showSelected[0]);
			showSelected[0].update(new ColorRGBA(0, 1, 1, 1));

			showSelected[1] = new VOIContour(true);
			makeSelectionFrame(Vector3f.UNIT_Z, Vector3f.UNIT_Y, pt, 4, showSelected[1]);
			showSelectedVOI.getCurves().add(showSelected[1]);
			showSelected[1].update(new ColorRGBA(0, 1, 1, 1));

			showSelected[2] = new VOIContour(true);
			makeSelectionFrame(Vector3f.UNIT_Z, Vector3f.UNIT_X, pt, 4, showSelected[2]);
			showSelectedVOI.getCurves().add(showSelected[2]);
			showSelected[2].update(new ColorRGBA(0, 1, 1, 1));

			showSelectedVOI.setColor(new Color(0, 255, 255));
		} else {
			for (int i = 0; i < showSelected.length; i++) {
				final Vector3f center = new Vector3f();
				for (int j = 0; j < showSelected[i].size(); j++) {
					center.add(showSelected[i].elementAt(j));
				}
				center.scale(1f / showSelected[i].size());
				final Vector3f diff = Vector3f.sub(pt, center);
				for (int j = 0; j < showSelected[i].size(); j++) {
					showSelected[i].elementAt(j).add(diff);
				}
			}
//			showSelectedVOI.update();
		}
		showSelectedVOI.update();
		if ( selected && (image.isRegistered(showSelectedVOI) == -1) )
		{
			// if selected and not registered register:
			image.registerVOI(showSelectedVOI);
//			System.err.println("updateSelected registering " + "showSelected"+getText() );
		}
		if ( !selected && (image.isRegistered(showSelectedVOI) != -1) )
		{
			// if !selected and is register unregister:
			image.unregisterVOI(showSelectedVOI);
//			System.err.println("updateSelected UNregistering " + "showSelected"+getText() );
		}
		return showSelectedVOI;
	}

    
    /**
     * Turns the display setting on/off for the 
     * VolumeVOI displayed in the volume renderer.
     * @param show
     */
    public void display( boolean show ) {
    	super.display(show);
    	if ( showSelectedVOI != null ) {
    		for ( int i = 0; i < showSelectedVOI.getCurves().size(); i++ ) {
    			VolumeVOI volVOI = showSelectedVOI.getCurves().elementAt(i).getVolumeVOI();
    			if ( volVOI != null ) {
    				volVOI.SetDisplay(show);
    			}
    		}
    	}
    }
    
	/**
	 * Generates the VOI that highlights which point (lattice or annotation) is currently selected by the user.
	 * 
	 * @param right
	 * @param up
	 * @param center
	 * @param diameter
	 * @param ellipse
	 */
	private void makeSelectionFrame(final Vector3f right, final Vector3f up, final Vector3f center, final float diameter, final VOIContour ellipse) {
		final int numPts = 12;
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (diameter * c), right);
			final Vector3f pos2 = Vector3f.scale((float) (diameter * s), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
	}
}


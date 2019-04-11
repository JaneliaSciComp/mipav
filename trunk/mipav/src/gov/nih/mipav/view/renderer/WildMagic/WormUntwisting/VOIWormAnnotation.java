package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.VOIText;

public class VOIWormAnnotation extends VOIText {
	
	private static final long serialVersionUID = 4256240928835686605L;

	private boolean retwist = false;
	private boolean modified = false;
	private int latticeSegment = -1;
	
	public VOIWormAnnotation() {
		super();
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
    	this.latticeSegment = kVOI.latticeSegment;
    }
    
	public VOIWormAnnotation clone() {
        return new VOIWormAnnotation(this);
    }
	
	public void modified( boolean modified ) {
		this.modified = modified;
	}
	
	public boolean modified() {
		return this.modified;
	}
	
	public void retwist( boolean retwist ) {
		this.retwist = retwist;
	}
	
	public boolean retwist() {
		return this.retwist;
	}
	
	public int getLatticeSegment() {
		return this.latticeSegment;
	}
	
	public void setLatticeSegment( int segment ) {
		this.latticeSegment = segment;
	}
}


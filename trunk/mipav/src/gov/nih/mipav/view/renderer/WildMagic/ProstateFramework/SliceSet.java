package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

class SliceSet implements Comparable<SliceSet> {
	public double cost;
	public int currentSlice;
	public int imageIndex;
	public int sliceIndex;
	
	public SliceSet(int _currentSlice, int _imageIndex, int _sliceIndex, double _cost) {
		cost = _cost;
		currentSlice = _currentSlice;
		imageIndex = _imageIndex;
		sliceIndex = _sliceIndex;
	}
	
	public int compareTo(SliceSet s) {
		if ( this.cost < s.cost ) 
			return -1;
		else if ( s.cost < cost ) 
			return 1;
		else 
			return 0;
	}
	
}
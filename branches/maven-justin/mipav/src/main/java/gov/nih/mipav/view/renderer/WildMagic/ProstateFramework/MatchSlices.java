package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.liblinearsvm.*;

class MatchSlices {
	public int currentSlice;
	public int bestImageIndex;
	public int bestSliceIndex;
	public Model model;
	
	public MatchSlices(int _currentSlice, int _bestImageIndex, int _bestSliceIndex) {
		currentSlice = _currentSlice;
		bestImageIndex = _bestImageIndex;
		bestSliceIndex = _bestSliceIndex;
	}
	
	public MatchSlices(int _currentSlice, int _bestImageIndex, int _bestSliceIndex, Model _model) {
		currentSlice = _currentSlice;
		bestImageIndex = _bestImageIndex;
		bestSliceIndex = _bestSliceIndex;
		model = _model;
	}
	
	
}
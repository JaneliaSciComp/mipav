package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

public interface CurveListener {
	/**
	 * CurveListener are updated whenever a curve changes in the LatticeModel.
	 * This enables plugins to track changes and update information displayed in the
	 * plugin user-interface.
	 */
	public void curveChanged();
}

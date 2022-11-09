package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

public interface AnnotationListener {
	/**
	 * AnnotationListeners are updated whenever annotations change in the LatticeModel.
	 * This enables plugins to track changes and update information displayed in the
	 * plugin user-interface.
	 */
	public void annotationChanged();
}

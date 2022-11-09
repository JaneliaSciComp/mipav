package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

public interface LatticeListener {
	/**
	 * LatticeListener are updated whenever the lattice changes in the LatticeModel.
	 * This enables plugins to track changes and update information displayed in the
	 * plugin user-interface.
	 */
	public void latticeChanged();
}

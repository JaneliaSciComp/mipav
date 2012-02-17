package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

abstract public class VolumeBase
{
	
	public abstract float getData(int x, int y, int z);

	public abstract int getNRows();
	public abstract int getNCols();
	public abstract int getNSlices();

}

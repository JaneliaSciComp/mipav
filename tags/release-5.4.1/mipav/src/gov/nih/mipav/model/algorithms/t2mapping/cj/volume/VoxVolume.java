package gov.nih.mipav.model.algorithms.t2mapping.cj.volume;

public class VoxVolume extends Volume
{
	private double[] voxelSize = new double[3];

	public VoxVolume(Volume v, double[] vox) {super(v); voxelSize = vox;}

	public double[] getVoxelSize() { return (double[])voxelSize.clone(); }
}

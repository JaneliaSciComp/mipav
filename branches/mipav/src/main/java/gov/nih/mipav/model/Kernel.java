package gov.nih.mipav.model;

public interface Kernel {
	float[][] getData();
	void setData(float[][] data);
	int[] getExtents();
}

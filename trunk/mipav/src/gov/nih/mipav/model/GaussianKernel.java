package gov.nih.mipav.model;

public class GaussianKernel implements Kernel {
	private float[][] data;

	public GaussianKernel() {
	}

	
	public float[][] getData() {
		return data;
	}

	public void setData(float[][] data){
		this.data = data;
	}
}

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
	
	public int[] getExtents(){
		if(data == null){
			return null;
		}
		int[] extents = new int[data.length];
		for(int i = 0; i < data.length; i++){
			extents[i] = data[i].length;
		}
		return extents;
	}
}

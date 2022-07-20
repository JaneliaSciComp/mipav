package gov.nih.mipav.model.algorithms.registration.vabra;

public class VabraRBF {

	private double maxValuesChange; //max change between adjacent values
	private int scale;//scale of the RBF
	public float[][][] values;//values of the RBF
	private int offset;//offset to the center of the RBF
	

	
	public VabraRBF() {
		offset = 0;
		scale = 5;
		setScale(5, 5, 5);
	}
	
	public void setScale(float dx, float dy, float dz) {

		int mult = 1;
		scale = (int) Math.ceil(1.7 * Math.max(dx, Math.max(dy, dz)));
		int i, j, k;
		float temp;
		offset = (int)Math.floor(mult * scale);
		
		values = new float[2 * offset + 1][ 2 * offset + 1][ 2 * offset + 1];
		
		for (i = -scale * mult; i <= scale * mult; i++)
			for (j = -scale * mult; j <= scale * mult; j++)
				for (k = -scale * mult; k <= scale * mult; k++) {
					values[i + offset][j + offset][k + offset] = 0;
				}

		for (i = -scale; i <= scale; i++)
			for (j = -scale; j <= scale; j++)
				for (k = -scale; k <= scale; k++) {
					values[i + offset][j + offset][k + offset] = RegistrationUtilities.RBF3D(0, 0, 0, i, j, k, scale);
				}

		maxValuesChange = 0.0;
		for (i = 0; i <= scale; i++)
			for (j = 0; j <= scale; j++)
				for (k = 0; k <= scale; k++) {
					temp = Math.abs(values[i + scale * mult][j + scale* mult][k + scale * mult]
					                       - values[i - 1 + scale * mult][j + scale* mult][k + scale * mult]);
					if (temp > maxValuesChange) maxValuesChange = temp;
				}
	}
	

	public int getScale(){
		return scale;
	}
	
	public int getOffset(){
		return offset;
	}
	
	public double getMaxValuesChange(){
		return maxValuesChange;
	}
	
}

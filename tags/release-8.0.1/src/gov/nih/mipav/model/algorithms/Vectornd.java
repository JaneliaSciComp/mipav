package gov.nih.mipav.model.algorithms;

/**
 * N dimension vector class with the cost value.
 * @author wangh3
 *
 */
public class Vectornd {
	/**
	 * N-dimension vector
	 */
	private double[] point;
	
	/**
	 * Cost value
	 */
	private double cost;
	
	public Vectornd(){
		point = null;
		cost = Double.MAX_VALUE;
	}
	
	public Vectornd(double[] pt){
		this(pt, Double.MAX_VALUE);
	}
	
	public Vectornd(double[] pt, double cost){
	    this(pt, cost, false);
	}
	
	public Vectornd(double[] pt, boolean copy){
	    this(pt, Double.MAX_VALUE, copy);
	}
	
	public Vectornd(double[] pt, double cost, boolean copy){
        if(copy){
            this.point = new double[pt.length];
            System.arraycopy(pt, 0, this.point, 0, pt.length);
        }else{
            this.point = pt;
        }
	    this.cost = cost;
	}
	
	public double get(int index){
		if(index >= 0 && index < point.length){
			return point[index];
		}
		return Float.NaN;
	}
	
	public double getCost() {
		return cost;
	}

	public void setCost(double cost) {
		this.cost = cost;
	}

	public double[] getPoint() {
		return point;
	}

	public void setPoint(double[] point) {
		this.point = point;
	}
	
	public int getDimension(){
		if(point == null){
			return -1;
		}
		return point.length;
	}
	
	public void set(int index, double value){
		if(point == null){
			return;
		}
		
		if(index < 0 || index >= point.length){
			point[index] = value;
		}
	}
}

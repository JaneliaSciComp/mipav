package gov.nih.mipav.model.algorithms.registration.vabra;


//Skeleton for an intensity Histogram
//Author - Min Chen
public abstract class IntensityHistogram{
	protected double entropy;
	protected boolean recentlyChanged;
	
	class HistBin implements Cloneable{
		int value;
		HistBin(int value){
			this.value = value;
		}
		
		HistBin(){
			this.value = 0;
		}
		
		protected HistBin clone(){
			HistBin clone = new HistBin();
			clone.value = this.value;
			return clone;
		}
		
	}
	
	public IntensityHistogram() {	
		this.entropy = 0;
		this.recentlyChanged = true;
	}

	protected abstract void evalEntropy();
	public double getEntropy() {
		if(!recentlyChanged){
			return entropy;
		}else{
			evalEntropy();
			return entropy;
		}
	}

	public abstract int getTotalVol();
	
}
